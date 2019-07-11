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


msgHandler :: MsgIn -> ReaderT MsgHandlerConfig IO (Either Err MsgOut)
msgHandler GetTables{}            = getTablesHandler
msgHandler msg@SubscribeToTable{} = subscribeToTableHandler msg
msgHandler (GameMsgIn msg)        = gameMsgHandler msg


gameMsgHandler :: GameMsgIn -> ReaderT MsgHandlerConfig IO (Either Err MsgOut)
gameMsgHandler msg@TakeSeat{}  = takeSeatHandler msg
gameMsgHandler msg@LeaveSeat{} = leaveSeatHandler msg
gameMsgHandler m@(GameMove tableName action) =  do
  conf@MsgHandlerConfig{..}  <- ask
  let playerAction = PlayerAction { name = unUsername username, .. }
  moveResult <- liftIO $ playMove conf tableName playerAction
  return $ either Left ( Right . (NewGameState tableName)) moveResult


playMove :: MsgHandlerConfig -> TableName -> PlayerAction -> IO (Either Err Game)
playMove conf@MsgHandlerConfig{..} tableName playerAction  = do 
   maybeTable <- liftIO $ atomically $ getTable serverStateTVar tableName
   case maybeTable of
     Nothing -> return $ Left $ TableDoesNotExist tableName
     Just Table{..} -> do 
      return $ either (Left . GameErr) Right $ runPlayerAction game playerAction


getTablesHandler :: ReaderT MsgHandlerConfig IO (Either Err MsgOut)
getTablesHandler = do
  MsgHandlerConfig {..} <- ask
  ServerState {..}      <- liftIO $ readTVarIO serverStateTVar
  let tableSummaries = TableList $ summariseTables lobby
  liftIO $ print tableSummaries
  liftIO $ sendMsg clientConn tableSummaries
  return $ Right $ tableSummaries


-- We fork a new thread for each game joined to receive game updates and propagate them to the client
-- We link the new thread to the current thread so on any exception in either then both threads are
-- killed to prevent memory leaks.
--
---- If game is in predeal stage then add player to game else add to waitlist
-- the waitlist is a queue awaiting the next predeal stage of the game
takeSeatHandler :: GameMsgIn -> ReaderT MsgHandlerConfig IO (Either Err MsgOut)
takeSeatHandler (TakeSeat tableName chipsToSit) = do
  conf@MsgHandlerConfig {..} <- ask
  ServerState {..}                       <- liftIO $ readTVarIO serverStateTVar
  case M.lookup tableName $ unLobby lobby of
    Nothing               -> return $ Left $ TableDoesNotExist tableName
    Just table@Table {..} -> do
      liftIO $ print "GAME STATE AFTER TAKE SEAT ACTION RECVD"
      liftIO $ print game
      canSit <- canTakeSeat chipsToSit tableName table
      case canSit of
        Left  err -> return $ Left err
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
              Left  gameErr -> return $ Left $ GameErr gameErr
              Right newGame -> do
                liftIO $ postTakeSeat conf tableName chipsToSit
                liftIO $ sendMsg clientConn
                                 (SuccessfullySatDown tableName newGame)
                let msgOut = NewGameState tableName newGame
                liftIO $ atomically $ updateTable' serverStateTVar tableName newGame
                return $ Right msgOut


postTakeSeat :: MsgHandlerConfig -> TableName -> Int -> IO ()
postTakeSeat conf@MsgHandlerConfig{..} name chipsSatWith = do
  dbDepositChipsIntoPlay dbConn
    (unUsername username)
    chipsSatWith
  --when
  --  (username `notElem` subscribers)
  --  (atomically $ subscribeToTable name conf)


leaveSeatHandler :: GameMsgIn -> ReaderT MsgHandlerConfig IO (Either Err MsgOut)
leaveSeatHandler leaveSeatMove@(LeaveSeat tableName) = do
  msgHandlerConfig@MsgHandlerConfig {..} <- ask
  ServerState {..}                       <- liftIO $ readTVarIO serverStateTVar
  case M.lookup tableName $ unLobby lobby of
    Nothing -> return $ Left $ TableDoesNotExist tableName
    Just table@Table {..} ->
      if unUsername username `notElem` getGamePlayerNames game
        then return $ Left $ NotSatInGame tableName
        else do
          let eitherProgressedGame = runPlayerAction
                game
                PlayerAction { name = unUsername username, action = LeaveSeat' }

          case eitherProgressedGame of
            Left  gameErr -> return $ Left $ GameErr gameErr
            Right newGame -> do
              let maybePlayer = find
                    (\Player {..} -> unUsername username == _playerName)
                    (_players game)
              case maybePlayer of
                Nothing -> return $ Left $ NotSatInGame tableName
                Just Player { _chips = chipsInPlay, ..} -> do
                  liftIO $ dbWithdrawChipsFromPlay dbConn
                                                   (unUsername username)
                                                   chipsInPlay
                  let msgOut =  NewGameState tableName newGame
                  liftIO $ atomically $ updateTable' serverStateTVar tableName newGame
                  return $ Right msgOut

canTakeSeat
  :: Int
  -> Text
  -> Table
  -> ReaderT MsgHandlerConfig IO (Either Err ())
canTakeSeat chipsToSit tableName Table { game = Game {..}, ..}
  | chipsToSit >= _minBuyInChips && chipsToSit <= _maxBuyInChips = do
    availableChipsE <- getPlayersAvailableChips
    MsgHandlerConfig{..} <- ask
    case availableChipsE of
      Left  err            -> return $ Left err
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


getPlayersAvailableChips :: ReaderT MsgHandlerConfig IO (Either Err Int)
getPlayersAvailableChips = do
  MsgHandlerConfig {..} <- ask
  maybeUser             <- liftIO $ dbGetUserByUsername dbConn username
  return $ case maybeUser of
    Nothing -> Left $ UserDoesNotExistInDB (unUsername username)
    Just UserEntity {..} ->
      Right $ userEntityAvailableChips - userEntityChipsInPlay
