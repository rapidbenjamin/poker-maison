{-# LANGUAGE OverloadedStrings #-}

module Socket.Setup where

import           Control.Concurrent.Async       ( Async )
import           Control.Lens
import           Control.Monad                  ( void )
import           Control.Monad.Except
import           Control.Monad.Logger           ( LoggingT
                                                , runStdoutLoggingT
                                                )
import           Control.Monad.Reader
import           Control.Monad.Reader           ( runReaderT )
import           Data.ByteString.Char8          ( pack
                                                , unpack
                                                )
import           Data.Int                       ( Int64 )
import           Data.List                      ( unfoldr )
import           Data.Map.Lazy                  ( Map )
import qualified Data.Map.Lazy                 as M
import           Data.Maybe
import           Data.Text                      ( Text )
import           Database.Persist.Postgresql    ( ConnectionString )
import           Database.Redis                 ( Redis
                                                , connect
                                                , runRedis
                                                , setex
                                                )
import qualified Database.Redis                as Redis

import           Database
import           Schema
import           Socket.Clients
import           Socket.Lobby
import           Socket.Types
import           Types

-- lobby including all game state is stored in redis 
setInitialLobby :: RedisConfig -> IO ()
setInitialLobby redisConfig = do
  lobby <- initialLobby
  runRedisAction redisConfig $ void $ Redis.hsetnx "gamesState"
                                                   "lobby"
                                                   (pack $ show lobby)

intialiseGameStateInRedis :: RedisConfig -> IO ()
intialiseGameStateInRedis = setInitialLobby
