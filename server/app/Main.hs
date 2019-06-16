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

import Data.Proxy
import Types

main :: IO ((), ())
main = do
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
