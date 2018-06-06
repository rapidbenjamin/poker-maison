{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Socket.Msg
  ( msgHandler
  ) where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, readMVar)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.State.Lazy
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import Prelude
import Text.Pretty.Simple (pPrint)

import Poker (getGamePlayerNames, getGamePlayers, progressGame)
import Poker.Types (Game, GameErr, PlayerAction)
import Socket.Clients
import Socket.Lobby
import Socket.Types
import Socket.Utils
import Types

msgHandler :: MsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) ()
msgHandler GetTables {} = getTablesHandler
msgHandler msg@GameMove {} = gameMoveHandler msg

getTablesHandler :: ReaderT MsgHandlerConfig (ExceptT Err IO) ()
getTablesHandler = do
  MsgHandlerConfig {..} <- ask
  ServerState {..} <- liftIO $ readMVar serverState
  liftIO $ sendMsg clientConn $ TableList lobby

unUsername :: Username -> Text
unUsername (Username username) = username

-- first we check that table exists and player is sat the game at table otherwise we throw an error
-- then the player move is applied to the table which results in either a new game state which is 
-- broadcast to all table subscribers or an error is returned which is then only sent to the
-- originator of the invalid in-game move
gameMoveHandler :: MsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) ()
gameMoveHandler gameMove@(GameMove tableName move) = do
  MsgHandlerConfig {..} <- ask
  ServerState {..} <- liftIO $ readMVar serverState
  case M.lookup tableName $ unLobby lobby of
    Nothing -> throwError $ TableDoesNotExist tableName
    Just table@Table {..} ->
      if not satAtTable
        then throwError $ NotSatAtTable tableName
        else case updateGameWithMove gameMove username game of
               Left gameErr -> throwError $ GameErr gameErr
               Right updatedGame -> do
                 liftIO $
                   broadcastMsg clients tableSubscribers $
                   NewGameState tableName updatedGame -- propagate this new game state to clients
                 let updatedLobby = updateTableGame tableName lobby updatedGame
                 liftIO $
                   updateServerState
                     serverState
                     ServerState {lobby = updatedLobby, ..}
      where satAtTable = unUsername username `elem` getGamePlayerNames game
            tableSubscribers = getTableSubscribers table

-- get either the new game state or an error when an in-game move is taken by a player 
updateGameWithMove :: MsgIn -> Username -> Game -> Either GameErr Game
updateGameWithMove (GameMove tableName playerAction) (Username username) game = do
  let (maybeErr, gameState) = runState (progressGame username playerAction) game
  case maybeErr of
    Nothing -> Right gameState
    Just gameErr -> Left gameErr
