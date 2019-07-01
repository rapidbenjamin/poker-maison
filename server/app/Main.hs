{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.Async
import           Database.Redis                 ( defaultConnectInfo )
import           Network.Wai.Handler.Warp       ( run )
import           Prelude
import qualified System.Remote.Monitoring      as EKG
import           Data.Text.Encoding            as TSE
import qualified Data.ByteString.Lazy          as BL

import qualified Data.ByteString.Lazy.Char8    as C

import           API
import           Database
import           Env
import           Socket

import           Data.Proxy
import           Types
import           Crypto.JWT

main :: IO ((), ())
main = do
  dbConnString  <- getDBConnStrFromEnv
  userAPIPort   <- getAuthAPIPort defaultUserAPIPort
  socketAPIPort <- getSocketAPIPort defaultSocketAPIPort
  redisConfig   <- getRedisHostFromEnv defaultRedisHost
  print "REDIS config: "
  print redisConfig
  secretKey <- getSecretKey
  let runSocketAPI =
        runSocketServer secretKey socketAPIPort dbConnString redisConfig
  let runUserAPI = run userAPIPort (app secretKey dbConnString redisConfig)
  migrateDB dbConnString
  ekg <- runMonitoringServer
  concurrently runUserAPI runSocketAPI
 where
  defaultUserAPIPort             = 8000
  defaultSocketAPIPort           = 5000
  defaultRedisHost               = "localhost"
  defaultMonitoringServerAddress = "localhost"
  defaultMonitoringServerPort    = 9999
  runMonitoringServer =
    EKG.forkServer defaultMonitoringServerAddress defaultMonitoringServerPort
