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
import Text.Pretty.Simple (pPrint)

import Poker
import Poker.Types
import Poker.Utils
import Socket.Clients
import Socket.Lobby
import Socket.Types
import Socket.Utils
import Types

msgHandler :: MsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) ()
msgHandler GetTables {} = getTablesHandler
msgHandler msg@JoinTable {} = joinTableHandler msg
msgHandler msg@TakeSeat {} = takeSeatHandler msg
msgHandler msg@GameMove {} = gameMoveHandler msg

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
          playerActionResult <-
            liftIO $ updateGameWithMove takeSeatAction username game
          case playerActionResult of
            Left gameErr -> throwError $ GameErr gameErr
            Right updatedGame -> do
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
            tableSubscribers = getTableSubscribers table

unUsername :: Username -> Text
unUsername (Username username) = username

--TODO!!! The game move next game progress calls should be 
--decoupled from the handling of the player action msg
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
        else do
          playerActionResult <-
            liftIO $ updateGameWithMove gameMove username game
          case playerActionResult of
            Left gameErr -> throwError $ GameErr gameErr
            Right updatedGame@Game {..} -> do
              liftIO $
                broadcastMsg clients tableSubscribers $
                NewGameState tableName updatedGame
              let updatedLobby = updateTableGame tableName updatedGame lobby
              liftIO $
                updateServerState
                  serverState
                  ServerState {lobby = updatedLobby, ..}
              liftIO $ print ("curr street is" ++ show _street)
              if _street == Showdown
                then do
                  nextHand <- liftIO $ getNextStage updatedGame
                  case nextHand of
                    Left gameErr -> throwError $ GameErr gameErr
                    Right gameWithNextHand -> do
                      liftIO $
                        broadcastMsg clients tableSubscribers $
                        NewGameState tableName gameWithNextHand
                      let updatedLobby =
                            updateTableGame tableName gameWithNextHand lobby
                      liftIO $
                        updateServerState
                          serverState
                          ServerState {lobby = updatedLobby, ..}
                else if everyoneAllIn updatedGame
                          -- such copy paste from above
                       then do
                         nextHand <- liftIO $ getNextStage updatedGame
                         case nextHand of
                           Left gameErr -> throwError $ GameErr gameErr
                           Right gameWithNextHand -> do
                             liftIO $
                               broadcastMsg clients tableSubscribers $
                               NewGameState tableName gameWithNextHand
                             let updatedLobby =
                                   updateTableGame
                                     tableName
                                     gameWithNextHand
                                     lobby
                             liftIO $
                               updateServerState
                                 serverState
                                 ServerState {lobby = updatedLobby, ..}
                       else return ()
      where satAtTable = unUsername username `elem` getGamePlayerNames game
            tableSubscribers = getTableSubscribers table

-- TODO MOVE THE BELOW TO POKER MODUYLE
-- get either the new game state or an error when an in-game move is taken by a player 
updateGameWithMove :: MsgIn -> Username -> Game -> IO (Either GameErr Game)
updateGameWithMove (GameMove tableName playerAction) (Username username) game = do
  (maybeErr, gameState) <-
    runStateT (runPlayerAction username playerAction) game
  print "next game state"
  pPrint gameState
  pPrint maybeErr
  case maybeErr of
    Nothing -> return $ Right gameState
    Just gameErr -> return $ Left gameErr

-- 
getNextStage :: Game -> IO (Either GameErr Game)
getNextStage game = do
  (maybeErr, gameState) <- runStateT nextStage game
  print "\n new hand"
  pPrint gameState
  pPrint maybeErr
  case maybeErr of
    Nothing -> return $ Right gameState
    Just gameErr -> return $ Left gameErr
