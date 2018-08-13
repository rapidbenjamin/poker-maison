{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import Network.Wai.Handler.Warp
import Prelude
import qualified System.Remote.Monitoring as EKG

import API
import Database
import Env
import Socket

import Network.Wai.Logger

main :: IO ((), ())
main = do
  dbConnString <- getDBConnStrFromEnv
  authAPIPort <- getAuthAPIPort defaultAuthAPIPort
  socketAPIPort <- getSocketAPIPort defaultSocketAPIPort
  secretKey <- getSecretKey
  let runSocketAPI =
        runSocketServer secretKey socketAPIPort dbConnString redisConfig
  let runUserAPI =
        withStdoutLogger $ \applogger -> do
          runSettings
            (setPort authAPIPort $ setLogger applogger defaultSettings)
            (app secretKey dbConnString redisConfig)
  migrateDB dbConnString
  ekg <- runMonitoringServer
  concurrently runUserAPI runSocketAPI
  where
    defaultAuthAPIPort = 8000
    defaultSocketAPIPort = 5000
    redisConfig = redisConnectInfo
    defaultMonitoringServerAddress = "localhost"
    defaultMonitoringServerPort = 9999
    runMonitoringServer =
      EKG.forkServer defaultMonitoringServerAddress defaultMonitoringServerPort
