{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Socket.Lobby where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, readMVar)
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

import Poker
import Poker.Types
import Socket.Types
import Socket.Utils
import Types

initialLobby =
  Lobby $ M.fromList [("Black", initialTable), ("White", initialTable)]
  where
    initialTable =
      Table {observers = [], waitList = [], game = initialGameState}

unLobby (Lobby lobby) = lobby

-- If game is in predeal stage then add player to game else add to waitlist
-- the waitlist is a queue awaiting the next predeal stage of the game
joinTable :: TableName -> ReaderT MsgHandlerConfig (ExceptT Err IO) ()
joinTable tableName = do
  MsgHandlerConfig {..} <- ask
  ServerState {..} <- liftIO $ readMVar serverState
  let maybeRoom = M.lookup tableName $ unLobby lobby
  case maybeRoom of
    Nothing -> throwError $ TableDoesNotExist tableName
    Just Table {..} ->
      if canJoinGame game
        then if getGameStage game == Predeal
               then joinGame
               else joinGameWaitlist
        else throwError TableFull tableName
      where joinGame = joinGame username game
            joinGameWaitlist = lobby username tableName
  return ()

getGameStage Game {..} = street

joinGame :: Username -> Game -> Game
joinGame (Username username) Game {..} = Game {players = players : username, ..}

joinTableWaitlist :: Username -> TableName -> Lobby -> Lobby
joinTableWaitlist username tableName lobby = updateTable lobby tableName
  where
    newTable = Game {}

updateTable :: TableName -> Table -> Lobby -> Lobby
updateTable tableName newTable (Lobby lobby) =
  Lobby $ M.insert tableName newTable lobby

canJoinGame :: Game -> Bool
canJoinGame Game {..} = length players < maxPlayers
