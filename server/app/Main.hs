{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import Database.Redis (defaultConnectInfo)
import Network.Wai.Handler.Warp (run)
import Prelude
import qualified System.Remote.Monitoring as EKG
import Data.Text.Encoding as TSE

import API
import Database
import Env
import Socket
import Web.JWT (secret)
import Servant.Auth.Server

import           Reason          (Spec (Spec), specsToDir, toReasonDecoderSource,
                               toReasonTypeSource)
import           GHC.Generics (Generic)
import           Servant.API  ((:>), Capture, Get, JSON)
import           Servant.Reason  (ReasonType, Proxy (Proxy), defReasonImports,
                               generateReasonForAPI)
import Servant

import Data.Proxy
import Reason
import Data.Text hiding (intercalate, map)
import Types
import Data.List (intercalate, map)

main :: IO ((), ())
main = do
  let code = defReasonImports
         : toReasonTypeSource    (Proxy :: Proxy ReturnToken)
         : toReasonDecoderSource (Proxy :: Proxy ReturnToken)
         : (generateReasonForAPI api)
  writeFile "Api.re" $ intercalate "\n\n" $ map unpack code
  dbConnString <- getDBConnStrFromEnv
  userAPIPort <- getAuthAPIPort defaultUserAPIPort
  socketAPIPort <- getSocketAPIPort defaultSocketAPIPort
  redisConfig <- getRedisHostFromEnv defaultRedisHost
  print "REDIS config: "
  print redisConfig
  secretKey <- getSecretKey
  let webJWTSecret = secret $ TSE.decodeUtf8 secretKey
  let runSocketAPI =
        runSocketServer webJWTSecret socketAPIPort dbConnString redisConfig
  let runUserAPI = run userAPIPort (app secretKey dbConnString redisConfig)
  migrateDB dbConnString
  ekg <- runMonitoringServer
  concurrently runUserAPI runSocketAPI
  where
    defaultUserAPIPort = 8000
    defaultSocketAPIPort = 5000
    defaultRedisHost = "localhost"
    defaultMonitoringServerAddress = "localhost"
    defaultMonitoringServerPort = 9999
    runMonitoringServer =
      EKG.forkServer defaultMonitoringServerAddress defaultMonitoringServerPort
