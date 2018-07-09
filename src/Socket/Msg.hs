{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Socket.Msg
  ( authenticatedMsgLoop
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Exception
import Control.Monad
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.STM
import Control.Monad.State.Lazy
import Data.Either
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

-- catches all synchronous exceptions for logging purposes
tryAny :: IO a -> IO (Either SomeException a)
tryAny action = do
  var <- newEmptyMVar
  uninterruptibleMask_ $
    forkIOWithUnmask $ \restore -> do
      res <- try $ restore action
      putMVar var res
  takeMVar var

-- default action derived from game state 
defaultMsg = GameMove "black" Fold

-- process msgs sent by the client socket
handleReadChanMsgs :: MsgHandlerConfig -> IO ()
handleReadChanMsgs msgHandlerConfig@MsgHandlerConfig {..} =
  forever $ do
    msg <- atomically $ readTChan msgReaderChan
    print $ "reader received msg: " ++ show msg
    msgOutE <- runExceptT $ runReaderT (gameMsgHandler msg) msgHandlerConfig
    either
      (\err -> sendMsg clientConn $ ErrMsg err)
      (handleNewGameState serverStateTVar)
      msgOutE

-- This function processes msgs from authenticated clients 
authenticatedMsgLoop :: MsgHandlerConfig -> IO ()
authenticatedMsgLoop msgHandlerConfig@MsgHandlerConfig {..} = do
  withAsync (handleReadChanMsgs msgHandlerConfig) $ \sockMsgReaderThread -> do
    finally
      (catch
         (forever $ do
            msg <- WS.receiveData clientConn
            print msg
            let parsedMsg = parseMsgFromJSON msg
            print parsedMsg
            for_ parsedMsg (\msg -> atomically $ writeTChan msgReaderChan msg)
            return ())
         (\e -> do
            let err = show (e :: IOException)
            print
              ("Warning: Exception occured in authenticatedMsgLoop for " ++
               show username ++ ": " ++ err)
            (removeClient username serverStateTVar)
            return ()))
      (removeClient username serverStateTVar)
  {-
-- Read the next value from a TChan or timeout
readTChanTimeout :: Int -> TChan a -> IO (Maybe a)
readTChanTimeout timeoutAfter pktChannel = do
  delay <- registerDelay timeoutAfter
  atomically $ Just <$> readTChan pktChannel <|> pure Nothing <* fini delay

-- fork a thread for reading socket msgs for the given game 
-- into a buffer created specifically for game msgs related to this table
-- for
readGameMsgsToBuffer :: WS.Connection -> TChan MsgOut -> STM () 
readGameMsgsToBuffer = do 
  maybeMsg <- WS.receiveData clientConn
-}

isPlayerToAct playerName game =
  (_street game /= PreDeal || _street game /= Showdown) &&
  ((_playerName (_players game !! _currentPosToAct game)) == playerName)

-- takes a channel and if the player in the thread is the current player to act in the room 
-- then if no valid game action is received within 30 secs then we run the Timeout action
-- against the game
tableReceiveMsgLoop :: TableName -> TChan MsgOut -> MsgHandlerConfig -> IO ()
tableReceiveMsgLoop tableName channel msgHandlerConfig@MsgHandlerConfig {..} =
  forever $ do
    print "tableReceiveMsgLoop"
    dupChan <- atomically $ dupTChan channel
    chanMsg <- atomically $ readTChan dupChan
    sendMsg clientConn chanMsg
    case chanMsg of
      NewGameState _ game -> do
        let isPlayerToAct' = isPlayerToAct (unUsername username) game
        if isPlayerToAct'
          then do
            print $ show username <> " turn to act? " <> show isPlayerToAct'
            maybeMsg <- timeMsg msgReaderChan
            x <- atomically $ isEmptyTChan msgReaderChan
            print $ "is chan empty " <> show x
            if isNothing maybeMsg
              then atomically $
                   writeTChan msgReaderChan (GameMove tableName Timeout)
              else return ()
          else return ()
      _ -> return ()

catchE :: TableName -> WS.ConnectionException -> IO MsgIn
catchE tableName e = do
  print e
  return $ GameMove tableName Timeout

-- Forks a new thread to run the timeout race in and propagates 
-- updates to the game state with either the resulting timeout or player action
runTimedMsg :: Int -> MsgHandlerConfig -> TableName -> MsgIn -> IO ()
runTimedMsg duration msgHandlerConfig tableName timeoutMsg =
  withAsync
    (catch
       (awaitTimedMsg duration msgHandlerConfig tableName timeoutMsg)
       (catchE tableName)) $ \timedAction -> do
    playerActionE <- waitCatch timedAction
    let playerAction = fromRight timeoutMsg playerActionE
  --  handleSocketMsg msgHandlerConfig playerAction
    return ()

-- If the timeout occurs then we return the default msg 
awaitTimedMsg :: Int -> MsgHandlerConfig -> TableName -> MsgIn -> IO MsgIn
awaitTimedMsg duration msgHandlerConfig@MsgHandlerConfig {..} tableName defaultMsg = do
  maybeMsg <- timeout duration (WS.receiveData clientConn)
  return $ maybe defaultMsg parseWithDefaultMsg maybeMsg
  where
    timeoutDuration = 5000000
    parseWithDefaultMsg = (fromMaybe defaultMsg) . parseMsgFromJSON

timeMsg :: TChan MsgIn -> IO (Maybe MsgIn)
timeMsg chan = do
  delayTVar <- registerDelay 5000000
  print "delay started"
  atomically $
    (Just <$> readTChan chan) `orElse`
    (Nothing <$ (readTVar delayTVar >>= check))

--- If the game gets to a state where no player action is possible 
--  then we need to recursively progress the game to a state where an action 
--  is possible. The game states which would lead to this scenario where the game 
--  needs to be manually progressed are:
--   
--  1. everyone is all in.
--  1. All but one player has folded or the game. 
--  3. Game is in the Showdown stage.
--
updateGameAndBroadcastT :: TVar ServerState -> TableName -> Game -> STM ()
updateGameAndBroadcastT serverStateTVar tableName newGame = do
  ServerState {..} <- readTVar serverStateTVar
  case M.lookup tableName $ unLobby lobby of
    Nothing -> throwSTM $ TableDoesNotExistEx tableName
    Just table@Table {..} -> do
      writeTChan channel $ NewGameState tableName newGame
      let updatedLobby = updateTableGame tableName newGame lobby
      swapTVar serverStateTVar ServerState {lobby = updatedLobby, ..}
      return ()

handleNewGameState :: TVar ServerState -> MsgOut -> IO ()
handleNewGameState serverStateTVar (NewGameState tableName newGame) = do
  newServerState <-
    atomically $ updateGameAndBroadcastT serverStateTVar tableName newGame
  print newServerState
  if (_street newGame == Showdown) ||
     ((hasBettingFinished newGame) && (_street newGame /= Showdown))
    then do
      (maybeErr, progressedGame) <- runStateT nextStage newGame
      if isNothing maybeErr
        then atomically $
             updateGameAndBroadcastT serverStateTVar tableName progressedGame
        else return ()
    else return ()
handleNewGameState serverStateTVar msg = do
  print msg
  return ()

-- Send a Message to the poker tables channel.
broadcastChanMsg :: MsgHandlerConfig -> TableName -> MsgOut -> IO ()
broadcastChanMsg MsgHandlerConfig {..} tableName msg = do
  ServerState {..} <- readTVarIO serverStateTVar
  case M.lookup tableName (unLobby lobby) of
    Nothing -> error "couldnt find tableName in lobby in broadcastChanMsg"
    Just Table {..} -> atomically $ writeTChan channel msg

gameMsgHandler :: MsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) MsgOut
gameMsgHandler GetTables {} = undefined
gameMsgHandler msg@JoinTable {} = undefined
gameMsgHandler msg@TakeSeat {} = takeSeatHandler msg
gameMsgHandler msg@GameMove {} = gameMoveHandler msg

getTablesHandler :: ReaderT MsgHandlerConfig (ExceptT Err IO) ()
getTablesHandler = do
  MsgHandlerConfig {..} <- ask
  ServerState {..} <- liftIO $ readTVarIO serverStateTVar
  liftIO $ sendMsg clientConn $ TableList

-- simply adds client to the list of subscribers
suscribeToTableChannel ::
     MsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) MsgOut
suscribeToTableChannel (JoinTable tableName) = undefined

-- We fork a new thread for each game joined to receive game updates and propagate them to the client
-- We link the new thread to the current thread so on any exception in either then both threads are
-- killed to prevent memory leaks.
takeSeatHandler :: MsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) MsgOut
takeSeatHandler move@(TakeSeat tableName) = do
  msgHandlerConfig@MsgHandlerConfig {..} <- ask
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
            Nothing -> do
              liftIO $ atomically $ joinTable tableName msgHandlerConfig
              asyncGameReceiveLoop <-
                liftIO $
                async (tableReceiveMsgLoop tableName channel msgHandlerConfig)
              liftIO $ link asyncGameReceiveLoop
              return $ NewGameState tableName newGame

-- If game is in predeal stage then add player to game else add to waitlist
-- the waitlist is a queue awaiting the next predeal stage of the game
joinTable :: TableName -> MsgHandlerConfig -> STM ()
joinTable tableName MsgHandlerConfig {..} = do
  ServerState {..} <- readTVar serverStateTVar
  let maybeRoom = M.lookup tableName $ unLobby lobby
  case maybeRoom of
    Nothing -> throwSTM $ TableDoesNotExistEx tableName
    Just table@Table {..} ->
      if canJoinGame game
        then do
          let updatedGame = joinGame username chipAmount game
          let updatedTable = Table {game = updatedGame, ..}
          let updatedLobby = updateTable tableName updatedTable lobby
          let tableSubscribers = getTableSubscribers table
          let newServerState = ServerState {lobby = updatedLobby, ..}
          swapTVar serverStateTVar newServerState
        else do
          let updatedTable = joinTableWaitlist username table
          let updatedLobby = updateTable tableName updatedTable lobby
          let newServerState = ServerState {lobby = updatedLobby, ..}
          swapTVar serverStateTVar newServerState
      where gameStage = getGameStage game
            chipAmount = 2500
  return ()

unUsername :: Username -> Text
unUsername (Username username) = username

-- first we check that table exists and player is sat the game at table otherwise we throw an error
-- then the player move is applied to the table which results in either a new game state which is 
-- broadcast to all table subscribers or an error is returned which is then only sent to the
-- originator of the invalid in-game move
gameMoveHandler :: MsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) MsgOut
gameMoveHandler gameMove@(GameMove tablename Timeout) = do
  liftIO $ print gameMove
  return Noop
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
