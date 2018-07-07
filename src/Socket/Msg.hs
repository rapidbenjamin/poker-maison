{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Socket.Msg
  ( authenticatedMsgLoop
  ) where

import Control.Concurrent
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
          f <- isEmptyMVar serverState
          print f
          x <- tryReadMVar serverState
          print x
          let s@ServerState {..} = fromJust x
          pPrint s
          let parsedMsg = parseMsgFromJSON msg
          print parsedMsg
      --   if parsedMsg == TakeSeat then 
         -- for messages like takeSeat we must use withAsync to fork a thread which is auto killed
         -- this thread will run  the game updateloop continously
          for_ parsedMsg $ \parsedMsg -> do
            print $ "parsed msg: " ++ show parsedMsg
            msgOutE <-
              runExceptT $ runReaderT (msgHandler parsedMsg) msgHandlerConfig
            either
              (\err -> sendMsg clientConn $ ErrMsg err)
              (sendMsg clientConn)
              msgOutE
          return ())
       (\e -> do
          let err = show (e :: IOException)
          print
            ("Warning: Exception occured in authenticatedMsgLoop for " ++
             show username ++ ": " ++ err)
          (removeClient username serverState)
          return ()))
    (removeClient username serverState)

-- takes a channel and if the player in the thread is the current player to act in the room 
-- then if no valid game action is received within 30 secs then we run the Timeout action
--against the game
gameUpdateLoop :: TableName -> TBChan Game -> MsgHandlerConfig -> IO ()
gameUpdateLoop tableName chan msgHandlerConfig@MsgHandlerConfig {..} =
  forever $ do
    print "djhdjhd"
    newGame@Game {..} <- atomically $ readTBChan chan -- WE ARE LISTENING TO THE CHANNEL IN A FORKED THREAD AND SEND MSGS TO CLIENT FROM THIS THREAD
    sendMsg clientConn $ NewGameState tableName newGame
    if False
              --use withAsync to ensure child threads are killed on parenbt death
      then do
        maybeMsg <- timeout 1000000 (WS.receiveData clientConn) -- only do this is current player is thread player
        liftIO $ print maybeMsg
        d <- isEmptyMVar serverState
        print $ d
        s@ServerState {..} <- liftIO $ readMVar serverState
        liftIO $ pPrint s
        let parsedMsg = maybe (Just Timeout) parseMsgFromJSON maybeMsg
        liftIO $ print parsedMsg
        for_ parsedMsg $ \parsedMsg -> do
          print $ "parsed msg: " ++ show parsedMsg
          msgOutE <-
            runExceptT $ runReaderT (msgHandler parsedMsg) msgHandlerConfig
          either
            (\err -> sendMsg clientConn $ ErrMsg err)
            (broadcastChanMsg msgHandlerConfig tableName)
            msgOutE
      else do
        sendMsg clientConn $ NewGameState tableName newGame
        return ()

-- 
getNextStage ::
     TableName -> Game -> ReaderT MsgHandlerConfig (ExceptT Err IO) MsgOut
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
     TableName -> Game -> ReaderT MsgHandlerConfig (ExceptT Err IO) MsgOut
updateGameAndBroadcast tableName newGame = do
  msgHandlerConfig@MsgHandlerConfig {..} <- ask
  ServerState {..} <- liftIO $ readMVar serverState
  case M.lookup tableName $ unLobby lobby of
    Nothing -> throwError $ TableDoesNotExist tableName
    Just table@Table {..} -> do
      let tableSubscribers = getTableSubscribers table
      liftIO $ atomically $ writeTBChan channel $ NewGameState tableName newGame
      let updatedLobby = updateTableGame tableName newGame lobby
      liftIO $
        updateServerState serverState ServerState {lobby = updatedLobby, ..}
      liftIO $ pPrint ("everyone all in? " ++ show (hasBettingFinished game))
      liftIO $ pPrint ("showdown? " ++ show (_street game == Showdown))
      if (_street newGame == Showdown) ||
         ((hasBettingFinished newGame) && (_street newGame /= Showdown))
        then getNextStage tableName newGame
        else return Noop

-- Send a Message to the poker tables channel.
broadcastChanMsg :: MsgHandlerConfig -> TableName -> MsgOut -> IO ()
broadcastChanMsg MsgHandlerConfig {..} tableName msg = do
  ServerState {..} <- readMVar serverState
  case M.lookup tableName (unLobby lobby) of
    Nothing -> error "couldnt find tableName in lobby in broadcastChanMsg"
    Just Table {..} -> atomically $ writeTBChan channel msg

msgHandler :: MsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) MsgOut
msgHandler GetTables {} = undefined
msgHandler msg@JoinTable {} = undefined
msgHandler msg@TakeSeat {} = takeSeatHandler msg
msgHandler msg@GameMove {} = gameMoveHandler msg
msgHandler msg@Timeout {} = gameMoveHandler msg

getTablesHandler :: ReaderT MsgHandlerConfig (ExceptT Err IO) ()
getTablesHandler = do
  MsgHandlerConfig {..} <- ask
  ServerState {..} <- liftIO $ readMVar serverState
  liftIO $ sendMsg clientConn $ TableList

-- simply adds client to the list of subscribers
suscribeToTableChannel ::
     MsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) MsgOut
suscribeToTableChannel (JoinTable tableName) = undefined

takeSeatHandler :: MsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) MsgOut
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
            Nothing -> updateGameAndBroadcast tableName updatedGame

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
     MsgIn
  -> Username
  -> Game
  -> ReaderT MsgHandlerConfig (ExceptT Err IO) MsgOut
updateGameWithMove (GameMove tableName playerAction) (Username username) game = do
  (maybeErr, newGame) <-
    liftIO $ runStateT (runPlayerAction username playerAction) game
  liftIO $ print "next game state"
  liftIO $ pPrint newGame
  liftIO $ pPrint maybeErr
  case maybeErr of
    Just gameErr -> throwError $ GameErr gameErr
    Nothing -> return $ NewGameState tableName newGame
