{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import API
import Config
import Control.Concurrent.Async
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Char8 as C
import Data.Foldable
import Data.Text
import Database
import Network.Wai.Handler.Warp (run)
import Prelude
import Schema
import Socket
import System.Environment (lookupEnv)

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
