{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Socket.Lobby where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, readMVar)
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad.STM

import Control.Monad (void)
import Control.Monad.Except
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Control.Monad.Reader
import Data.ByteString.Char8 (pack, unpack)
import Data.Int (Int64)
import Data.List (unfoldr)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Monoid
import Data.Text (Text)
import Poker
import Poker.Types
import Poker.Utils
import Socket.Clients
import Socket.Types
import Socket.Utils
import Types

initialLobby :: IO Lobby
initialLobby = do
  chan <- atomically newTChan
  return $
    Lobby $
    M.fromList
      [ ( "Black"
        , Table
            { subscribers = []
            , waitlist = []
            , game = initialGameState
            , channel = chan
            })
      ]
  where
    maxChanLength = 10000

unLobby :: Lobby -> Map TableName Table
unLobby (Lobby lobby) = lobby

-- If game is in predeal stage then add player to game else add to waitlist
-- the waitlist is a queue awaiting the next predeal stage of the game
joinTable :: TableName -> ReaderT MsgHandlerConfig (ExceptT Err IO) ()
joinTable tableName = do
  MsgHandlerConfig {..} <- ask
  ServerState {..} <- liftIO $ readTVarIO serverStateTVar
  let maybeRoom = M.lookup tableName $ unLobby lobby
  case maybeRoom of
    Nothing -> throwError $ TableDoesNotExist tableName
    Just table@Table {..} ->
      if canJoinGame game
        then do
          let updatedGame = joinGame username chipAmount game
          let updatedTable = Table {game = updatedGame, ..}
          let updatedLobby = updateTable tableName updatedTable lobby
          let tableSubscribers = getTableSubscribers table
          let newServerState = ServerState {lobby = updatedLobby, ..}
          liftIO $ atomically $ swapTVar serverStateTVar newServerState
          liftIO $
            broadcastMsg clients tableSubscribers $
            NewGameState tableName updatedGame
        else do
          let updatedTable = joinTableWaitlist username table
          let updatedLobby = updateTable tableName updatedTable lobby
          let newServerState = ServerState {lobby = updatedLobby, ..}
          liftIO $ atomically $ swapTVar serverStateTVar newServerState
          liftIO $ broadcastAllClients clients $ NewTableList
      where gameStage = getGameStage game
            chipAmount = 2500
  return ()

joinGame :: Username -> Int -> Game -> Game
joinGame (Username username) chips Game {..} =
  Game {_players = _players <> [player], ..}
  where
    player = getPlayer username chips

joinTableWaitlist :: Username -> Table -> Table
joinTableWaitlist username Table {..} =
  Table {waitlist = waitlist <> [username], ..}

updateTable :: TableName -> Table -> Lobby -> Lobby
updateTable tableName newTable (Lobby lobby) =
  Lobby $ M.insert tableName newTable lobby

-- to do - return an either as there are multiple errs for why plyr cant join game ie no chips
canJoinGame :: Game -> Bool
canJoinGame Game {..} = length _players < _maxPlayers

lookupTableSubscribers :: TableName -> Lobby -> [Username]
lookupTableSubscribers tableName (Lobby lobby) =
  case M.lookup tableName lobby of
    Nothing -> []
    Just Table {..} ->
      (fmap Username $ getGamePlayerNames game) ++ waitlist ++ subscribers

updateTableGame :: TableName -> Game -> Lobby -> Lobby
updateTableGame tableName newGame (Lobby lobby) =
  Lobby $ M.adjust updateTable tableName lobby
  where
    updateTable Table {..} = Table {game = newGame, ..}

-- returns playernames of all observers , players in game and on waitlist
getTableSubscribers Table {..} =
  (Username <$> getGamePlayerNames game) ++ waitlist ++ subscribers
