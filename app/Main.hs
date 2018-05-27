{-# LANGUAGE OverloadedStrings #-}

module Main where

import API
import Data.Aeson
import Data.Text
import Network.Wai.Handler.Warp (run)

import Database
import Schema

main :: IO ()
main = do
  connString <- fetchPostgresConnection
  run 8000 (app connString)
--  migrateDB localConnString
