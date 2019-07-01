{-# LANGUAGE OverloadedStrings #-}

module Env where

import qualified Data.ByteString.Char8         as C
import           Data.Either
import           Data.Maybe
import           Data.Text                      ( pack )
import           Database.Redis                 ( ConnectInfo
                                                , HostName
                                                , Redis
                                                , connect
                                                , connectHost
                                                , connectPort
                                                , defaultConnectInfo
                                                , parseConnectInfo
                                                , runRedis
                                                , setex
                                                )
import           Prelude
import           System.Environment             ( lookupEnv )
import           Text.Read
import           Types
import           Web.JWT                        ( secret )
import           Data.ByteString.UTF8          as BSU

getRedisHostFromEnv :: HostName -> IO ConnectInfo
getRedisHostFromEnv defaultHostName = do
  maybeConnInfo <- lookupEnv "redisHost"
  case maybeConnInfo of
    Nothing -> do
      print "couldn't parse redishost from env default used"
      return defaultRedisConn
    Just hostname -> do
      print "hostName from env is: "
      print hostname
      return $ defaultConnectInfo { connectHost = hostname }
  where defaultRedisConn = defaultConnectInfo { connectHost = defaultHostName }

-- get the postgres connection string from dbConnStr env variable
getDBConnStrFromEnv :: IO C.ByteString
getDBConnStrFromEnv = do
  dbConnStr <- lookupEnv "dbConnStr"
  case dbConnStr of
    Nothing   -> error "Missing dbConnStr in env"
    Just conn -> return $ C.pack conn

-- get the port from the userAPIPort env variable
getAuthAPIPort :: Int -> IO Int
getAuthAPIPort defaultPort = do
  maybeEnvPort <- lookupEnv "authAPIPort"
  case maybeEnvPort of
    Nothing   -> return defaultPort
    Just port -> maybe (return defaultPort) return (readMaybe port)

-- get the port from the socketAPIPort env variable
getSocketAPIPort :: Int -> IO Int
getSocketAPIPort defaultPort = do
  maybeEnvPort <- lookupEnv "socketPort"
  case maybeEnvPort of
    Nothing   -> return defaultPort
    Just port -> maybe (return defaultPort) return (readMaybe port)

-- get the secret key for signing JWT authentication tokens
getSecretKey = do
  maybeSecretKey <- lookupEnv "secret"
  case maybeSecretKey of
    Nothing -> error "Missing secret key in env"
    Just s  -> return $ BSU.fromString s
