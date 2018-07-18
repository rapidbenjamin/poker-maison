{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

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
import Poker.ActionValidation
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
    msgOutE <- runExceptT $ runReaderT (gameMsgHandler msg) msgHandlerConfig
    either
      (\err -> sendMsg clientConn $ ErrMsg err)
      (handleNewGameState serverStateTVar)
      msgOutE
    print "msgReaderChannel"
    pPrint msgOutE
    return ()

-- This function writes msgs received from the websocket to the socket threads msgReader channel 
-- then forks a new thread to read msgs from the authenticated client
-- The use of channels in this way makes it feasible to implement timeouts
-- if an expected msg in not received in a given time without killing threads.
-- This is preferable as killing threads inside IO actions is not safe 
authenticatedMsgLoop :: MsgHandlerConfig -> IO ()
authenticatedMsgLoop msgHandlerConfig@MsgHandlerConfig {..} = do
  withAsync (handleReadChanMsgs msgHandlerConfig) $ \sockMsgReaderThread -> do
    finally
      (catch
         (forever $ do
            msg <- WS.receiveData clientConn
            let parsedMsg = parseMsgFromJSON msg
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

isPlayerToAct playerName game =
  (_street game /= PreDeal && _street game /= Showdown) &&
  ((_playerName (_players game !! _currentPosToAct game)) == (playerName))

-- takes a channel and if the player in the thread is the current player to act in the room 
-- then if no valid game action is received within 30 secs then we run the Timeout action
-- against the game
tableReceiveMsgLoop :: TableName -> TChan MsgOut -> MsgHandlerConfig -> IO ()
tableReceiveMsgLoop tableName channel msgHandlerConfig@MsgHandlerConfig {..} = do
  msgReaderDup <- atomically $ dupTChan msgReaderChan
  dupChan <- atomically $ dupTChan channel
  forever $ do
    chanMsg <- atomically $ readTChan dupChan
    sendMsg clientConn chanMsg
    case chanMsg of
      NewGameState _ game -> do
        let isPlayerToAct' = isPlayerToAct (unUsername username) game
        if isPlayerToAct'
          then let timeoutDuration = 12000000
                in do print $
                        show username <> " turn to act? " <> show isPlayerToAct'
                      maybeMsg <-
                        timeGameMoveMsg
                          game
                          (unUsername username)
                          timeoutDuration
                          msgReaderChan
                      if isNothing maybeMsg
                        then atomically $
                             writeTChan
                               msgReaderChan
                               (GameMove tableName Timeout)
                        else print maybeMsg
                          --atomically $
                           --  writeTChan msgReaderChan (fromJust maybeMsg)
          else return ()
      _ -> return ()

catchE :: TableName -> WS.ConnectionException -> IO MsgIn
catchE tableName e = do
  print e
  return $ GameMove tableName Timeout

-- A return value of Nothing denotes that no valid action
-- was received in the given time period.
-- If a valid gameMove player action was received then we
-- wrap the msgIn in a Just
timeGameMoveMsg :: Game -> PlayerName -> Int -> TChan MsgIn -> IO (Maybe MsgIn)
timeGameMoveMsg game playerName duration chan = do
  delayTVar <- registerDelay duration
  print "delay started"
  awaitValidAction game playerName delayTVar chan

-- We duplicate the channel reading the socket msgs and start a timeout
-- The thread will be blocked until either a valid action is received 
-- or the timeout finishes 
awaitValidAction ::
     Game -> PlayerName -> TVar Bool -> TChan MsgIn -> IO (Maybe MsgIn)
awaitValidAction game playerName delayTVar chan = do
  dupChan <- atomically $ dupTChan chan
  atomically $
    (Just <$>
     (readTChan dupChan >>= \msg ->
        guard (isValidAction game playerName msg) *> pure msg)) `orElse`
    (Nothing <$ (readTVar delayTVar >>= check))
  where
    isValidAction game playerName =
      \case
        (GameMove _ action) -> isNothing $ validateAction game playerName action
        _ -> False

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
  pPrint newGame
  progressGameAlong serverStateTVar tableName newGame
handleNewGameState serverStateTVar msg = do
  print msg
  return ()

progressGameAlong :: TVar ServerState -> TableName -> Game -> IO ()
progressGameAlong serverStateTVar tableName game@Game {..}
  | canProgress = do
    (maybeErr, progressedGame) <- runStateT nextStage game
    if (isNothing maybeErr)
      then do
        print $ "isEveryoneAllin" ++ (show $ isEveryoneAllIn progressedGame)
        atomically $
          updateGameAndBroadcastT serverStateTVar tableName progressedGame
        pPrint progressedGame
      else print $ "progressGameAlong " ++ show maybeErr
  | otherwise = return ()
  where
    canProgress =
      (_street == Showdown) ||
      ((hasBettingFinished game) && (_street /= Showdown))

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
updateGameWithMove (GameMove tableName playerAction) (Username username) game
--  liftIO $ print "running player action"
 = do
  (maybeErr, newGame) <-
    liftIO $ runStateT (runPlayerAction username playerAction) game
  liftIO $ print "next game state"
 -- liftIO $ pPrint newGame
  liftIO $ print $ ("updateGameWithMove") ++ show maybeErr
  case maybeErr of
    Just gameErr -> throwError $ GameErr gameErr
    Nothing -> return $ NewGameState tableName newGame
