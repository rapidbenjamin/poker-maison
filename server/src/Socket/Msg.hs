-- TODO - Factor out repetitive STM actions that lookup table and throw stm error if not found 
-- with monadic composition
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Socket.Msg where

import           Control.Applicative

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan

import           Control.Exception

import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.STM
import           Control.Monad.State.Lazy

import           Data.Either
import           Data.Foldable
import           Data.Functor
import           Data.Map.Lazy                  ( Map )
import qualified Data.Map.Lazy                 as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Database.Persist.Postgresql    ( ConnectionString )
import qualified Network.WebSockets            as WS

import           Prelude

import           Debug.Trace

import           Text.Pretty.Simple             ( pPrint )

import           Control.Concurrent.Async
import           Poker.ActionValidation
import           Poker.Game.Game
import           Poker.Game.Utils
import           Poker.Poker
import           Poker.Types             hiding ( LeaveSeat )
import           Schema
import           Socket.Clients
import           Socket.Lobby
import           Socket.Subscriptions
import           Socket.Types
import           Socket.Utils

import           System.Timeout
import           Types

import           Database

msgHandler :: MsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) MsgOut
msgHandler GetTables{}            = getTablesHandler
msgHandler msg@SubscribeToTable{} = subscribeToTableHandler msg
msgHandler (GameMsgIn msg)        = gameMsgHandler msg

gameMsgHandler :: GameMsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) MsgOut
gameMsgHandler msg@TakeSeat{}  = takeSeatHandler msg
gameMsgHandler msg@LeaveSeat{} = leaveSeatHandler msg
gameMsgHandler msg@GameMove{}  = gameActionHandler msg

-- process msgs sent by the client socket
handleReadChanMsgs :: MsgHandlerConfig -> IO ()
handleReadChanMsgs msgHandlerConfig@MsgHandlerConfig {..} = forever $ do
  msg <- atomically $ readTChan socketReadChan
  print msg
  print "aboooo"
  msgOutE <- runExceptT $ runReaderT (msgHandler msg) msgHandlerConfig
  pPrint msgOutE
  case msgOutE of
    Right m@(GameMsgOut gm) ->
      sendMsg clientConn m >> handleNewGameState dbConn serverStateTVar gm
    Right m   -> sendMsg clientConn m
    Left  err -> sendMsg clientConn (ErrMsg err)


-- This function writes msgs received from the websocket to the socket threads msgReader channel 
-- then forks a new thread to read msgs from the authenticated client
-- The use of channels in this way makes it feasible to implement timeouts
-- if an expected msg in not received in a given time without killing threads.
-- This is preferable as killing threads inside IO actions is not safe 
authenticatedMsgLoop :: MsgHandlerConfig -> IO ()
authenticatedMsgLoop msgHandlerConfig@MsgHandlerConfig {..} =
  withAsync (handleReadChanMsgs msgHandlerConfig) $ \sockMsgReaderThread ->
    finally
      (catch
        (forever $ do
          msg <- WS.receiveData clientConn
          print msg
          let parsedMsg = parseMsgFromJSON msg
          print "parsed msg:"
          print parsedMsg
          for_ parsedMsg $ atomically . writeTChan socketReadChan
        )
        (\e -> do
          let err = show (e :: IOException)
          print
            (  "Warning: Exception occured in authenticatedMsgLoop for "
            ++ show username
            ++ ": "
            ++ err
            )
          removeClient username serverStateTVar
          return ()
        )
      )
      (removeClient username serverStateTVar)

-- | Forks a thread which timeouts the player when they dont act in sufficient time
-- We fork this thread using async once a player sits down at a table
--
-- Duplicates the channel which reads table updates and starts a timeout if it is the players turn to act
-- If no valid game action is received from the socket msg read thread then we update the game with a special
-- timeout action which will usually check or fold the player.
--
-- Note that timeouts are only enforced when the expediency of the player's action is required
-- to avoid halting the progress of the game.
playerTimeoutLoop :: TableName -> TChan GameMsgOut -> MsgHandlerConfig -> IO ()
playerTimeoutLoop tableName channel msgHandlerConfig@MsgHandlerConfig {..} = do
  msgReaderDup <- atomically $ dupTChan socketReadChan
  dupTableChan <- atomically $ dupTChan channel
  forever $ do
    dupTableChanMsg <- atomically $ readTChan dupTableChan
    case dupTableChanMsg of
      (NewGameState tableName newGame) ->
        let playerHasToAct = doesPlayerHaveToAct (unUsername username) newGame
        in  when playerHasToAct $ awaitTimedPlayerAction socketReadChan
                                                         newGame
                                                         tableName
                                                         username
      _ -> return ()

-- We read from a duplicate of the channel which reads msgs from the client socket.
-- We use a duplicate to listen to the client so as to not consume messages intended for other tables
awaitValidPlayerAction
  :: TableName -> Game -> PlayerName -> TChan MsgIn -> STM MsgIn
awaitValidPlayerAction tableName game name dupChan =
  readTChan dupChan >>= \msg -> if isValidAction game name msg
    then return msg
    else awaitValidPlayerAction tableName game name dupChan
 where
  isValidAction game name = \case
    GameMsgIn (GameMove tableName' action) ->
      (isRight $ validateAction game name action) && tableName == tableName'
    _ -> False

-- Race the timer and the blocking read call which awaits a valid player msg.
-- The race results in an Either where the Left signals a timeout and a 
-- Right denotes a valid game action was received from the client in sufficient
-- time.
awaitTimedPlayerAction :: TChan MsgIn -> Game -> TableName -> Username -> IO ()
awaitTimedPlayerAction socketReadChan game tableName (Username playerName) = do
  delayTVar         <- registerDelay timeoutDuration
  dupChan           <- atomically $ dupTChan socketReadChan
  validPlayerAction <- async $ atomically $ awaitValidPlayerAction tableName
                                                                   game
                                                                   playerName
                                                                   dupChan
  timer           <- async $ atomically $ readTVar delayTVar >>= check
  msgInOrTimedOut <- waitEitherCancel timer validPlayerAction
  when (isLeft msgInOrTimedOut) $ atomically $ writeTChan
    socketReadChan
    (GameMsgIn $ GameMove tableName Timeout)
  where timeoutDuration = 6500000

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
    Nothing               -> throwSTM $ TableDoesNotExistInLobby tableName
    Just table@Table {..} -> do
      writeTChan channel $ GameMsgOut $ NewGameState tableName newGame
      let updatedLobby = updateTableGame tableName newGame lobby
      swapTVar serverStateTVar ServerState { lobby = updatedLobby, .. }
      return ()

handleNewGameState
  :: ConnectionString -> TVar ServerState -> GameMsgOut -> IO ()
handleNewGameState connString serverStateTVar (NewGameState tableName newGame)
  = do
    newServerState <- atomically
      $ updateGameAndBroadcastT serverStateTVar tableName newGame
    async (progressGame' connString serverStateTVar tableName newGame)
    return ()
handleNewGameState _ _ msg = do
  return ()


progressGame'
  :: ConnectionString -> TVar ServerState -> TableName -> Game -> IO ()
progressGame' connString serverStateTVar tableName game@Game {..} = do
  threadDelay stagePauseDuration
  when (canProgressGame game) $ do
    progressedGame <- progressGame game
    pPrint "PROGRESED GAME"
    pPrint progressedGame
    print "haveAllPlayersActed:"
    print (haveAllPlayersActed progressedGame)

    let currentStreet = progressedGame ^. street
    atomically
      $ updateGameAndBroadcastT serverStateTVar tableName progressedGame
 --        when
 --          (currentStreet == Showdown)
 --          (dbUpdateUsersChips connString $ getPlayerChipCounts progressedGame)
 --        pPrint progressedGame
    progressGame' connString serverStateTVar tableName progressedGame
  where stagePauseDuration = 5000000

getTablesHandler :: ReaderT MsgHandlerConfig (ExceptT Err IO) MsgOut
getTablesHandler = do
  MsgHandlerConfig {..} <- ask
  ServerState {..}      <- liftIO $ readTVarIO serverStateTVar
  let tableSummaries = TableList $ summariseTables lobby
  liftIO $ print tableSummaries
  liftIO $ sendMsg clientConn tableSummaries
  return tableSummaries

-- First we check the table exists and if the user is not already subscribed then we add them to the list of subscribers
-- Game and any other table updates will be propagated to those on the subscriber list
subscribeToTableHandler
  :: MsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) MsgOut
subscribeToTableHandler (SubscribeToTable tableName) = do
  liftIO $ putStrLn "CALLLLLLLLLLLLLLLLLLLLLLLLLED3"
  msgHandlerConfig@MsgHandlerConfig {..} <- ask
  ServerState {..}                       <- liftIO $ readTVarIO serverStateTVar
  case M.lookup tableName $ unLobby lobby of
    Nothing         -> throwError $ TableDoesNotExist tableName
    Just Table {..} -> do
      liftIO $ print subscribers
      if username `notElem` subscribers
        then do
          liftIO $ atomically $ subscribeToTable tableName msgHandlerConfig
          --liftIO $ sendMsg clientConn (GameMsgOut $ NewGameState tableName game)
          liftIO $ sendMsg clientConn
                           (SuccessfullySubscribedToTable tableName game)
          return $ SuccessfullySubscribedToTable tableName game
        else do
          liftIO
            $ sendMsg clientConn (ErrMsg $ AlreadySubscribedToTable tableName)
          return $ ErrMsg $ AlreadySubscribedToTable tableName

subscribeToTable :: TableName -> MsgHandlerConfig -> STM ()
subscribeToTable tableName MsgHandlerConfig {..} = do
  ServerState {..} <- readTVar serverStateTVar
  let maybeTable = M.lookup tableName $ unLobby lobby
  case maybeTable of
    Nothing               -> throwSTM $ TableDoesNotExistInLobby tableName
    Just table@Table {..} -> if username `notElem` subscribers
      then do
        let updatedTable =
              Table { subscribers = subscribers <> [username], .. }
        let updatedLobby   = updateTable tableName updatedTable lobby
        let newServerState = ServerState { lobby = updatedLobby, .. }
        swapTVar serverStateTVar newServerState
        return ()
      else throwSTM $ CannotAddAlreadySubscribed tableName

-- We fork a new thread for each game joined to receive game updates and propagate them to the client
-- We link the new thread to the current thread so on any exception in either then both threads are
-- killed to prevent memory leaks.
--
---- If game is in predeal stage then add player to game else add to waitlist
-- the waitlist is a queue awaiting the next predeal stage of the game
takeSeatHandler :: GameMsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) MsgOut
takeSeatHandler (TakeSeat tableName chipsToSit) = do
  msgHandlerConfig@MsgHandlerConfig {..} <- ask
  ServerState {..}                       <- liftIO $ readTVarIO serverStateTVar
  case M.lookup tableName $ unLobby lobby of
    Nothing               -> throwError $ TableDoesNotExist tableName
    Just table@Table {..} -> do
      liftIO $ print "GAME STATE AFTER TAKE SEAT ACTION RECVD"
      liftIO $ print game
      if unUsername username `elem` getGamePlayerNames game
        then throwError $ AlreadySatInGame tableName
        else do
          hasEnoughChipsErrE <- canTakeSeat chipsToSit tableName table
          case hasEnoughChipsErrE of
            Left  err -> throwError err
            Right ()  -> do
              let player       = initPlayer (unUsername username) chipsToSit
                  playerAction = PlayerAction { name   = unUsername username
                                              , action = SitDown player
                                              }
                  takeSeatAction = GameMove tableName (SitDown player)
              eitherProgressedGame <- liftIO $ runPlayerAction
                game
                PlayerAction { name   = unUsername username
                             , action = SitDown player
                             }

              case eitherProgressedGame of
                Left  gameErr -> throwError $ GameErr gameErr
                Right newGame -> do
                  liftIO $ dbDepositChipsIntoPlay dbConn
                                                  (unUsername username)
                                                  chipsToSit
                  when
                    (username `notElem` subscribers)
                    (liftIO $ atomically $ subscribeToTable tableName
                                                            msgHandlerConfig
                    )
                  --asyncGameReceiveLoop <-
                  --  liftIO $
                  --  async (playerTimeoutLoop tableName channel msgHandlerConfig)
                  --liftIO $ link asyncGameReceiveLoop
                  liftIO $ sendMsg clientConn
                                   (SuccessfullySatDown tableName newGame)
                  return $ GameMsgOut $ NewGameState tableName newGame

leaveSeatHandler
  :: GameMsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) MsgOut
leaveSeatHandler leaveSeatMove@(LeaveSeat tableName) = do
  msgHandlerConfig@MsgHandlerConfig {..} <- ask
  ServerState {..}                       <- liftIO $ readTVarIO serverStateTVar
  case M.lookup tableName $ unLobby lobby of
    Nothing -> throwError $ TableDoesNotExist tableName
    Just table@Table {..} ->
      if unUsername username `notElem` getGamePlayerNames game
        then throwError $ NotSatInGame tableName
        else do
          eitherProgressedGame <-
            liftIO
              $ (runPlayerAction
                  game
                  PlayerAction { name   = unUsername username
                               , action = LeaveSeat'
                               }
                )
          case eitherProgressedGame of
            Left  gameErr -> throwError $ GameErr gameErr
            Right newGame -> do
              let maybePlayer = find
                    (\Player {..} -> unUsername username == _playerName)
                    (_players game)
              case maybePlayer of
                Nothing -> throwError $ NotSatInGame tableName
                Just Player { _chips = chipsInPlay, ..} -> do
                  liftIO $ dbWithdrawChipsFromPlay dbConn
                                                   (unUsername username)
                                                   chipsInPlay
                  return $ GameMsgOut $ NewGameState tableName newGame

canTakeSeat
  :: Int
  -> Text
  -> Table
  -> ReaderT MsgHandlerConfig (ExceptT Err IO) (Either Err ())
canTakeSeat chipsToSit tableName Table { game = Game {..}, ..}
  | chipsToSit >= _minBuyInChips && chipsToSit <= _maxBuyInChips = do
    availableChipsE <- getPlayersAvailableChips
    case availableChipsE of
      Left  err            -> throwError err
      Right availableChips -> if availableChips >= chipsToSit
        then return $ Right ()
        else return $ Left NotEnoughChipsToSit
  | otherwise = return $ Left $ ChipAmountNotWithinBuyInRange tableName

getPlayersAvailableChips
  :: ReaderT MsgHandlerConfig (ExceptT Err IO) (Either Err Int)
getPlayersAvailableChips = do
  MsgHandlerConfig {..} <- ask
  maybeUser             <- liftIO $ dbGetUserByUsername dbConn username
  return $ case maybeUser of
    Nothing -> Left $ UserDoesNotExistInDB (unUsername username)
    Just UserEntity {..} ->
      Right $ userEntityAvailableChips - userEntityChipsInPlay


-- first we check that table exists and player is sat the game at table otherwise we throw an error
-- then the player move is applied to the table which results in either a new game state which is 
-- broadcast to all table subscribers or an error is returned which is then only sent to the
-- originator of the invalid in-game move
gameActionHandler
  :: GameMsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) MsgOut
gameActionHandler gameMove@(GameMove tableName action) = do
  liftIO $ print action
  MsgHandlerConfig {..} <- ask
  ServerState {..}      <- liftIO $ readTVarIO serverStateTVar
  case M.lookup tableName $ unLobby lobby of
    Nothing -> throwError $ TableDoesNotExist tableName
    Just table@Table {..} ->
      let satAtTable = unUsername username `elem` getGamePlayerNames game
      in  if not satAtTable
            then throwError $ NotSatAtTable tableName
            else do
              eitherNewGame <- liftIO $ runPlayerAction
                game
                PlayerAction { name = unUsername username, .. }
              case eitherNewGame of
                Left gameErr -> do
                  liftIO $ print "Error! :<"
                  liftIO $ print gameErr
                  throwError $ GameErr gameErr
                Right newGame -> do
                  liftIO $ print "No error :)"
                  liftIO $ pPrint newGame
                  return $ GameMsgOut $ NewGameState tableName newGame
