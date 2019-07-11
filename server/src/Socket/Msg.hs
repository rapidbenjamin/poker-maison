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
import Socket.Table
import           System.Timeout
import           Types
import           System.Random
import           Database

msgHandler :: MsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) MsgOut
msgHandler GetTables{}            = getTablesHandler
msgHandler msg@SubscribeToTable{} = subscribeToTableHandler msg
msgHandler (GameMsgIn msg)        = gameMsgHandler msg


gameMsgHandler :: GameMsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) MsgOut
gameMsgHandler msg@TakeSeat{}  = takeSeatHandler msg
gameMsgHandler msg@LeaveSeat{} = leaveSeatHandler msg
gameMsgHandler msg@GameMove{}  = undefined --FIX ME -- ADJUST TYPES


--- If the game gets to a state where no player action is possible 
--  then we need to recursively progress the game to a state where an action 
--  is possible. The game states which would lead to this scenario where the game 
--  needs to be manually progressed are:
--   
--  1. everyone is all in.
--  1. All but one player has folded or the game. 
--  3. Game is in the Showdown stage.
--


handleNewGameState :: ConnectionString -> TVar ServerState -> MsgOut -> IO ()
handleNewGameState connString serverStateTVar (NewGameState tableName newGame)
  = do
    print " OLD BROADCASTING!!!!!!!!!!!"
    newServerState <- atomically  $ updateTable' serverStateTVar tableName newGame
    return ()
handleNewGameState _ _ msg = return ()


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
        let updatedLobby   = insertTable tableName updatedTable lobby
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
      canSit <- canTakeSeat chipsToSit tableName table
      case canSit of
        Left  err -> throwError err
        Right ()  -> do
          let player       = initPlayer (unUsername username) chipsToSit
              playerAction = PlayerAction { name   = unUsername username
                                          , action = SitDown player
                                          }
              takeSeatAction = GameMove tableName (SitDown player)
          case
              runPlayerAction
                game
                PlayerAction { name   = unUsername username
                             , action = SitDown player
                             }
            of
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
                liftIO $ sendMsg clientConn
                                 (SuccessfullySatDown tableName newGame)
                let msgOut =  NewGameState tableName newGame
                liftIO $ handleNewGameState dbConn serverStateTVar msgOut
                return msgOut


leaveSeatHandler :: GameMsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) MsgOut
leaveSeatHandler leaveSeatMove@(LeaveSeat tableName) = do
  msgHandlerConfig@MsgHandlerConfig {..} <- ask
  ServerState {..}                       <- liftIO $ readTVarIO serverStateTVar
  case M.lookup tableName $ unLobby lobby of
    Nothing -> throwError $ TableDoesNotExist tableName
    Just table@Table {..} ->
      if unUsername username `notElem` getGamePlayerNames game
        then throwError $ NotSatInGame tableName
        else do
          let eitherProgressedGame = runPlayerAction
                game
                PlayerAction { name = unUsername username, action = LeaveSeat' }

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
                  let msgOut =  NewGameState tableName newGame
                  liftIO $ handleNewGameState dbConn serverStateTVar msgOut
                  return msgOut

canTakeSeat
  :: Int
  -> Text
  -> Table
  -> ReaderT MsgHandlerConfig (ExceptT Err IO) (Either Err ())
canTakeSeat chipsToSit tableName Table { game = Game {..}, ..}
  | chipsToSit >= _minBuyInChips && chipsToSit <= _maxBuyInChips = do
    availableChipsE <- getPlayersAvailableChips
    MsgHandlerConfig{..} <- ask
    case availableChipsE of
      Left  err            -> throwError err
      Right chips -> do 
        tableE <- liftIO $ checkTableExists serverStateTVar tableName 
        return $ tableE <* hasEnoughChips chips chipsToSit
  | otherwise = return $ Left $ ChipAmountNotWithinBuyInRange tableName
  where 
    hasEnoughChips availableChips chipsNeeded = 
      if availableChips >= chipsToSit
      then return $ Right ()
      else return $ Left NotEnoughChipsToSit
    checkTableExists s name = do
          t <- atomically $ getTable s name
          case t of 
            Nothing -> return $ Left $ TableDoesNotExist name
            _ -> return $ Right ()


getPlayersAvailableChips :: ReaderT MsgHandlerConfig (ExceptT Err IO) (Either Err Int)
getPlayersAvailableChips = do
  MsgHandlerConfig {..} <- ask
  maybeUser             <- liftIO $ dbGetUserByUsername dbConn username
  return $ case maybeUser of
    Nothing -> Left $ UserDoesNotExistInDB (unUsername username)
    Just UserEntity {..} ->
      Right $ userEntityAvailableChips - userEntityChipsInPlay
