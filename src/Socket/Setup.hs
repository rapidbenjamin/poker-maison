{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Socket.Setup where

import Control.Monad (void)
import Control.Monad.Except
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Control.Monad.Reader
import Control.Monad.Reader (runReaderT)
import Data.ByteString.Char8 (pack, unpack)
import Data.Int (Int64)
import Data.List (unfoldr)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe
import Data.Maybe
import Data.Text (Text)
import Database.Redis (Redis, connect, runRedis, setex)
import qualified Database.Redis as Redis

import Database
import Schema
import Socket.Clients
import Socket.Types
import Types

-- lobby inlcuding all game state is stored in redis to allow for horizontal scaling
initialLobby =
  Lobby $ M.fromList [("Black", initialTable), ("White", initialTable)]
  where
    initialTable = Table {observers = [], waitList = [], game = 0}

setInitialLobby :: RedisConfig -> Lobby -> IO ()
setInitialLobby redisConfig lobby = do
  res <-
    liftIO $
    runRedisAction redisConfig $
    void $ Redis.hsetnx "gamesState" "lobby" (pack $ show initialLobby)
  return res

intialiseGameStateInRedis :: RedisConfig -> IO ()
intialiseGameStateInRedis redisConfig = setInitialLobby redisConfig initialLobby
