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
import System.Environment (lookupEnv)

main :: IO ()
main = do
  dbConnString <- getDBConnStrFromEnv
  port <- getPortFromEnv defaultPort
  migrateDB dbConnString
  -- writeFile "docs/api.md" apiDocs
  run port (app dbConnString)
  where
    defaultPort = 8000
