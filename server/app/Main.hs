{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import Network.Wai.Handler.Warp (run)
import Prelude
import qualified System.Remote.Monitoring as EKG

import API
import Database
import Env
import Socket

main :: IO ((), ())
main = do
  dbConnString <- getDBConnStrFromEnv
  userAPIPort <- getAuthAPIPort defaultUserAPIPort
  socketAPIPort <- getSocketAPIPort defaultSocketAPIPort
  secretKey <- getSecretKey
  let runSocketAPI =
        runSocketServer secretKey socketAPIPort dbConnString redisConfig
  let runUserAPI = run userAPIPort (app secretKey dbConnString redisConfig)
  migrateDB dbConnString
  ekg <- runMonitoringServer
  concurrently runUserAPI runSocketAPI
  where
    defaultUserAPIPort = 8000
    defaultSocketAPIPort = 5000
    redisConfig = redisConnectInfo
    defaultMonitoringServerAddress = "localhost"
    defaultMonitoringServerPort = 9999
    runMonitoringServer =
      EKG.forkServer defaultMonitoringServerAddress defaultMonitoringServerPort
