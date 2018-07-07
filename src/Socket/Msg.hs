{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Socket.Msg
  ( authenticatedMsgLoop
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TBChan
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.STM
import Control.Monad.State.Lazy
import Data.Foldable
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import Prelude
import Socket.Types
import System.Timeout
import Text.Pretty.Simple (pPrint)

import Control.Concurrent.Async
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

-- This function processes msgs from authenticated clients 
authenticatedMsgLoop :: MsgHandlerConfig -> IO ()
authenticatedMsgLoop msgHandlerConfig@MsgHandlerConfig {..} = do
  finally
    (catch
       (forever $ do
          msg <- WS.receiveData clientConn
          print msg
         -- s@ServerState {..} <- readTVarIO serverStateTVar
          --pPrint s
          let parsedMsg = parseMsgFromJSON msg
          print parsedMsg
      --   if parsedMsg == TakeSeat then 
         -- for messages like takeSeat we must use withAsync to fork a thread which is auto killed
         -- this thread will run  the game updateloop continously
          for_ parsedMsg $ \parsedMsg -> do
            print $ "parsed msg: " ++ show parsedMsg
            msgOutE <-
              runExceptT $
              runReaderT (gameMsgHandler parsedMsg) msgHandlerConfig
            either
              (\err -> sendMsg clientConn $ ErrMsg err)
              (handleNewGameState serverStateTVar)
              msgOutE
            print msgOutE
          return ())
       (\e -> do
          let err = show (e :: IOException)
          print
            ("Warning: Exception occured in authenticatedMsgLoop for " ++
             show username ++ ": " ++ err)
          (removeClient username serverStateTVar)
          return ()))
    (removeClient username serverStateTVar)

-- takes a channel and if the player in the thread is the current player to act in the room 
-- then if no valid game action is received within 30 secs then we run the Timeout action
--against the game
gameUpdateLoop :: TableName -> TBChan Game -> MsgHandlerConfig -> IO ()
gameUpdateLoop tableName chan msgHandlerConfig@MsgHandlerConfig {..} =
  forever $ do
    print "gameUpdateLoopCalled"
    newGame@Game {..} <- atomically $ readTBChan chan -- WE ARE LISTENING TO THE CHANNEL IN A FORKED THREAD AND SEND MSGS TO CLIENT FROM THIS THREAD
    sendMsg clientConn $ NewGameState tableName newGame
    if False
              --use withAsync to ensure child threads are killed on parenbt death
      then do
        maybeMsg <- timeout 1000000 (WS.receiveData clientConn) -- only do this is current player is thread player
        liftIO $ print maybeMsg
        s@ServerState {..} <- liftIO $ readTVarIO serverStateTVar
        liftIO $ pPrint s
        let parsedMsg = maybe (Just Timeout) parseMsgFromJSON maybeMsg
        liftIO $ print parsedMsg
        for_ parsedMsg $ \parsedMsg -> do
          print $ "parsed msg: " ++ show parsedMsg
          msgOutE <-
            runExceptT $ runReaderT (gameMsgHandler parsedMsg) msgHandlerConfig
          either
            (\err -> sendMsg clientConn $ ErrMsg err)
            (broadcastChanMsg msgHandlerConfig tableName)
            msgOutE
      else do
        sendMsg clientConn $ NewGameState tableName newGame
        return ()

--- If the game gets to a state where no player action is possible 
--  then we need to recursively progress the game to a state where an action 
--  is possible. The game states which would lead to this scenario where the game 
--  needs to be manually progressed are:
--   
--  1. everyone is all in.
--  1. All but one player has folded or the game. 
--  3. Game is in the Showdown stage.
--
updateGameAndBroadcast :: TVar ServerState -> TableName -> Game -> STM ()
updateGameAndBroadcast serverStateTVar tableName newGame = do
  ServerState {..} <- readTVar serverStateTVar
  case M.lookup tableName $ unLobby lobby of
    Nothing -> return ()
    Just table@Table {..} -> do
      writeTBChan channel $ NewGameState tableName newGame
      let updatedLobby = updateTableGame tableName newGame lobby
      swapTVar serverStateTVar ServerState {lobby = updatedLobby, ..}
      return ()

handleNewGameState :: TVar ServerState -> MsgOut -> IO ()
handleNewGameState serverStateTVar (NewGameState tableName newGame) = do
  newServerState <-
    atomically $ updateGameAndBroadcast serverStateTVar tableName newGame
  print newServerState
  if (_street newGame == Showdown) ||
     ((hasBettingFinished newGame) && (_street newGame /= Showdown))
    then do
      (maybeErr, progressedGame) <- runStateT nextStage newGame
      if isNothing maybeErr
        then atomically $
             updateGameAndBroadcast serverStateTVar tableName progressedGame
        else return ()
    else return ()

-- Send a Message to the poker tables channel.
broadcastChanMsg :: MsgHandlerConfig -> TableName -> MsgOut -> IO ()
broadcastChanMsg MsgHandlerConfig {..} tableName msg = do
  ServerState {..} <- readTVarIO serverStateTVar
  case M.lookup tableName (unLobby lobby) of
    Nothing -> error "couldnt find tableName in lobby in broadcastChanMsg"
    Just Table {..} -> atomically $ writeTBChan channel msg

gameMsgHandler :: MsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) MsgOut
gameMsgHandler GetTables {} = undefined
gameMsgHandler msg@JoinTable {} = undefined
gameMsgHandler msg@TakeSeat {} = takeSeatHandler msg
gameMsgHandler msg@GameMove {} = gameMoveHandler msg
gameMsgHandler msg@Timeout {} = gameMoveHandler msg

getTablesHandler :: ReaderT MsgHandlerConfig (ExceptT Err IO) ()
getTablesHandler = do
  MsgHandlerConfig {..} <- ask
  ServerState {..} <- liftIO $ readTVarIO serverStateTVar
  liftIO $ sendMsg clientConn $ TableList

-- simply adds client to the list of subscribers
suscribeToTableChannel ::
     MsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) MsgOut
suscribeToTableChannel (JoinTable tableName) = undefined

takeSeatHandler :: MsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) MsgOut
takeSeatHandler move@(TakeSeat tableName) = do
  MsgHandlerConfig {..} <- ask
  ServerState {..} <- liftIO $ readTVarIO serverStateTVar
  case M.lookup tableName $ unLobby lobby of
    Nothing -> throwError $ TableDoesNotExist tableName
    Just table@Table {..} ->
      if (unUsername username) `elem` getGamePlayerNames game
        then throwError $ AlreadySatInGame tableName
        else do
          let chips_Hardcoded = 2000
          let player = getPlayer (unUsername username) chips_Hardcoded
          let takeSeatAction = GameMove tableName $ SitDown player
          (maybeErr, newGame) <-
            liftIO $
            runStateT
              (runPlayerAction (unUsername username) (SitDown player))
              game
          case maybeErr of
            Just gameErr -> throwError $ GameErr gameErr
            Nothing -> return $ NewGameState tableName newGame

unUsername :: Username -> Text
unUsername (Username username) = username

-- first we check that table exists and player is sat the game at table otherwise we throw an error
-- then the player move is applied to the table which results in either a new game state which is 
-- broadcast to all table subscribers or an error is returned which is then only sent to the
-- originator of the invalid in-game move
gameMoveHandler :: MsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) MsgOut
gameMoveHandler gameMove@(Timeout) = throwError $ NotSatAtTable "black"
gameMoveHandler gameMove@(GameMove tableName move) = do
  MsgHandlerConfig {..} <- ask
  ServerState {..} <- liftIO $ readTVarIO serverStateTVar
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
     MsgIn
  -> Username
  -> Game
  -> ReaderT MsgHandlerConfig (ExceptT Err IO) MsgOut
updateGameWithMove (GameMove tableName playerAction) (Username username) game = do
  liftIO $ print "running player action"
  (maybeErr, newGame) <-
    liftIO $ runStateT (runPlayerAction username playerAction) game
  liftIO $ print "next game state"
  liftIO $ pPrint newGame
  liftIO $ pPrint maybeErr
  case maybeErr of
    Just gameErr -> throwError $ GameErr gameErr
    Nothing -> return $ NewGameState tableName newGame
