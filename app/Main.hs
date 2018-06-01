{-# LANGUAGE OverloadedStrings #-}

module Main where

import API
import Data.Aeson
import Data.Text
import Network.Wai.Handler.Warp (run)

import Database
import Prelude
import Schema

main :: IO ()
main = do
  connString <- fetchPostgresConnection
  migrateDB connString
 -- writeFile "docs/api.md" apiDocs
  run 8000 (app connString)
--  migrateDB localConnString
