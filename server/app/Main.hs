{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import Network.Wai.Handler.Warp 
import Prelude
import qualified System.Remote.Monitoring as EKG

import API
import Config
import Database
import Socket

import           Network.Wai.Logger   


main :: IO ((), ())
main = do
  dbConnString <- getDBConnStrFromEnv
  userAPIPort <- getUserAPIPort defaultUserAPIPort
  socketAPIPort <- getSocketAPIPort defaultSocketAPIPort
  secretKey <- getSecretKey
  let runSocketAPI =
        runSocketServer secretKey socketAPIPort dbConnString redisConfig
  let runUserAPI = withStdoutLogger $ \applogger -> do
           runSettings ( setPort userAPIPort $ setLogger applogger defaultSettings)  (app secretKey dbConnString redisConfig)
  migrateDB dbConnString
  ekg <- runMonitoringServer
  concurrently runUserAPI runSocketAPI
  where
    defaultUserAPIPort = 8000
    defaultSocketAPIPort = 3000
    redisConfig = redisConnectInfo
    defaultMonitoringServerAddress = "localhost"
    defaultMonitoringServerPort = 9999
    runMonitoringServer =
      EKG.forkServer defaultMonitoringServerAddress defaultMonitoringServerPort
