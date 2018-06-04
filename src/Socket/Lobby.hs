{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Socket.Lobby where

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
import Data.Text (Text)
import Database.Redis (Redis, connect, runRedis, setex)
import qualified Database.Redis as Redis

import Socket.Types
import Socket.Utils
import Types

initialLobby =
  Lobby $ M.fromList [("Black", initialTable), ("White", initialTable)]
  where
    initialTable = Table {observers = [], waitList = [], game = 0}
