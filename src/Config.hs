{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Config where

import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import Data.Text
import Database
import Network.Wai.Handler.Warp (run)
import Prelude
import Schema
import System.Environment (lookupEnv)
import Text.Read

-- get the postgres connection string from dbConnStr env variable
getDBConnStrFromEnv :: IO C.ByteString
getDBConnStrFromEnv = do
  dbConnStr <- lookupEnv "dbConnStr"
  case dbConnStr of
    Nothing -> error "Missing dbConnStr in env"
    (Just conn) -> return $ C.pack conn

-- get the port from the userAPIPort env variable
getUserAPIPort :: Int -> IO Int
getUserAPIPort defaultPort = do
  maybeEnvPort <- lookupEnv "userAPIPort"
  case maybeEnvPort of
    Nothing -> return defaultPort
    (Just port) -> maybe (return defaultPort) return (readMaybe port)

-- get the port from the socketAPIPort env variable
getSocketAPIPort :: Int -> IO Int
getSocketAPIPort defaultPort = do
  maybeEnvPort <- lookupEnv "socketPort"
  case maybeEnvPort of
    Nothing -> return defaultPort
    (Just port) -> maybe (return defaultPort) return (readMaybe port)
