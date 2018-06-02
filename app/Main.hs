{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import API
import Config
import Control.Concurrent.Async
import Database
import Network.Wai.Handler.Warp (run)
import Prelude
import Socket

main :: IO ((), ())
main = do
  dbConnString <- getDBConnStrFromEnv
  userAPIPort <- getUserAPIPort defaultUserAPIPort
  socketAPIPort <- getSocketAPIPort defaultSocketAPIPort
  let runSocketAPI = runSocketServer socketAPIPort dbConnString
  let runUserAPI = run userAPIPort (app dbConnString)
  migrateDB dbConnString
  concurrently runUserAPI runSocketAPI
  where
    defaultUserAPIPort = 8000
    defaultSocketAPIPort = 3000
