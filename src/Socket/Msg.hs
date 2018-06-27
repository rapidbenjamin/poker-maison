{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Socket.Msg
  ( msgHandler
  ) where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, readMVar)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import Prelude
import System.Timeout
import Text.Pretty.Simple (pPrint)

import Poker
import Poker.Game
import Poker.Types
import Poker.Utils
import Socket.Clients
import Socket.Lobby
import Socket.Types
import Socket.Utils
import System.Timeout
import Types

-- default action derived from game state 
defaultMsg = GameMove "black" Fold

msgHandler :: MsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) ()
msgHandler GetTables {} = getTablesHandler
msgHandler msg@JoinTable {} = joinTableHandler msg
msgHandler msg@TakeSeat {} = takeSeatHandler msg
msgHandler msg@GameMove {} = gameMoveHandler msg
msgHandler msg@Timeout {} = gameMoveHandler msg

getTablesHandler :: ReaderT MsgHandlerConfig (ExceptT Err IO) ()
getTablesHandler = do
  MsgHandlerConfig {..} <- ask
  ServerState {..} <- liftIO $ readMVar serverState
  liftIO $ sendMsg clientConn $ TableList lobby

-- simply adds client to the list of subscribers
joinTableHandler :: MsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) ()
joinTableHandler (JoinTable tableName) = do
  MsgHandlerConfig {..} <- ask
  ServerState {..} <- liftIO $ readMVar serverState
  case M.lookup tableName $ unLobby lobby of
    Nothing -> throwError $ TableDoesNotExist tableName
    Just table@Table {..} ->
      if username `elem` tableSubscribers
        then throwError $ AlreadySubscribedToTable tableName
        else do
          let updatedTable =
                Table {subscribers = tableSubscribers <> [username], ..}
          let updatedLobby = updateTable tableName updatedTable lobby
          liftIO $
            updateServerState serverState ServerState {lobby = updatedLobby, ..}
      where satAtTable = unUsername username `elem` getGamePlayerNames game
            tableSubscribers = getTableSubscribers table

takeSeatHandler :: MsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) ()
takeSeatHandler move@(TakeSeat tableName) = do
  MsgHandlerConfig {..} <- ask
  ServerState {..} <- liftIO $ readMVar serverState
  case M.lookup tableName $ unLobby lobby of
    Nothing -> throwError $ TableDoesNotExist tableName
    Just table@Table {..} ->
      if (unUsername username) `elem` getGamePlayerNames game
        then throwError $ AlreadySatInGame tableName
        else do
          let chips_Hardcoded = 2000
          let player = getPlayer (unUsername username) chips_Hardcoded
          let takeSeatAction = GameMove tableName $ SitDown player
          (maybeErr, updatedGame) <-
            liftIO $
            runStateT
              (runPlayerAction (unUsername username) (SitDown player))
              game
          case maybeErr of
            Just gameErr -> throwError $ GameErr gameErr
            Nothing -> do
              let tableSubscribers = getTableSubscribers table
              let newTableSubscribers =
                    if username `notElem` tableSubscribers
                      then tableSubscribers <> [username]
                      else tableSubscribers
              let updatedTable =
                    Table
                      { subscribers = newTableSubscribers
                      , game = updatedGame
                      , ..
                      }
              liftIO $
                broadcastMsg clients newTableSubscribers $
                NewGameState tableName updatedGame
              let updatedLobby = updateTable tableName updatedTable lobby
              liftIO $
                updateServerState
                  serverState
                  ServerState {lobby = updatedLobby, ..}
      where satInGame = unUsername username `elem` getGamePlayerNames game

unUsername :: Username -> Text
unUsername (Username username) = username

-- first we check that table exists and player is sat the game at table otherwise we throw an error
-- then the player move is applied to the table which results in either a new game state which is 
-- broadcast to all table subscribers or an error is returned which is then only sent to the
-- originator of the invalid in-game move
gameMoveHandler :: MsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) ()
gameMoveHandler gameMove@(Timeout) = liftIO $ print gameMove
gameMoveHandler gameMove@(GameMove tableName move) = do
  MsgHandlerConfig {..} <- ask
  ServerState {..} <- liftIO $ readMVar serverState
  case M.lookup tableName $ unLobby lobby of
    Nothing -> throwError $ TableDoesNotExist tableName
    Just table@Table {..} ->
      let satAtTable = unUsername username `elem` getGamePlayerNames game
       in if not satAtTable
            then throwError $ NotSatAtTable tableName
            else updateGameWithMove gameMove username game

-- TODO MOVE THE BELOW TO POKER MODULE
-- get either the new game state or an error when an in-game move is taken by a player 
updateGameWithMove ::
     MsgIn -> Username -> Game -> ReaderT MsgHandlerConfig (ExceptT Err IO) ()
updateGameWithMove (GameMove tableName playerAction) (Username username) game = do
  (maybeErr, newGame) <-
    liftIO $ runStateT (runPlayerAction username playerAction) game
  liftIO $ print "next game state"
  liftIO $ pPrint newGame
  liftIO $ pPrint maybeErr
  case maybeErr of
    Just gameErr -> throwError $ GameErr gameErr
    Nothing -> updateGameAndBroadcast tableName newGame

-- 
getNextStage ::
     TableName -> Game -> ReaderT MsgHandlerConfig (ExceptT Err IO) ()
getNextStage tableName game = do
  (maybeErr, newGame) <- liftIO $ runStateT nextStage game
  liftIO $ print "\n new hand"
  liftIO $ pPrint newGame
  liftIO $ pPrint maybeErr
  case maybeErr of
    Just gameErr -> throwError $ GameErr gameErr
    Nothing -> updateGameAndBroadcast tableName newGame

--- If the game gets to a state where no player action is possible 
--  then we need to recursively progress the game to a state where an action 
--  is possible. The game states which would lead to this scenario where the game 
--  needs to be manually progressed are:
--   
--  1. everyone is all in.
--  1. All but one player has folded or the game. 
--  3. Game is in the Showdown stage.
--
updateGameAndBroadcast ::
     TableName -> Game -> ReaderT MsgHandlerConfig (ExceptT Err IO) ()
updateGameAndBroadcast tableName newGame = do
  MsgHandlerConfig {..} <- ask
  ServerState {..} <- liftIO $ readMVar serverState
  case M.lookup tableName $ unLobby lobby of
    Nothing -> throwError $ TableDoesNotExist tableName
    Just table@Table {..} -> do
      let tableSubscribers = getTableSubscribers table
      liftIO $
        broadcastMsg clients tableSubscribers $ NewGameState tableName newGame
      let updatedLobby = updateTableGame tableName newGame lobby
      liftIO $
        updateServerState serverState ServerState {lobby = updatedLobby, ..}
      liftIO $ pPrint ("everyone all in? " ++ show (hasBettingFinished game))
      liftIO $ pPrint ("showdown? " ++ show (_street game == Showdown))
      if (_street newGame == Showdown) ||
         ((hasBettingFinished newGame) && (_street newGame /= Showdown))
        then getNextStage tableName newGame
        else return ()
