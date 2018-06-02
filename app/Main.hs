{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import API
import Config
import Data.Aeson
import qualified Data.ByteString.Char8 as C
import Data.Text
import Database
import Network.Wai.Handler.Warp (run)
import Prelude
import Schema
import Socket
import System.Environment (lookupEnv)

main :: IO ()
main = do
  dbConnString <- getDBConnStrFromEnv
  userAPIPort <- getUserAPIPort defaultUserAPIPort
  socketAPIPort <- getSocketAPIPort defaultSocketAPIPort
  migrateDB dbConnString
  run userAPIPort (app dbConnString)
  where
    defaultUserAPIPort = 8000
    defaultSocketAPIPort = 3000
