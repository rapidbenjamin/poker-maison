{-
  Logic for updating players about table changes
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Socket.Subscriptions where

import           Control.Applicative

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan

import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.STM
import           Control.Monad.State.Lazy
import           Control.Monad

import           Data.Either
import           Data.Functor
import           Data.Map.Lazy                  ( Map )
import qualified Data.Map.Lazy                 as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Network.WebSockets            as WS

import           Prelude

import           Text.Pretty.Simple             ( pPrint )

import           Control.Concurrent.Async
import           Socket.Clients
import           Socket.Lobby
import           Socket.Types
import           Socket.Utils
import           System.Timeout

import           Types
import           Poker.Game.Privacy

getTableSubscribers :: TableName -> Lobby -> [Username]
getTableSubscribers tableName (Lobby lobby) = case M.lookup tableName lobby of
  Nothing         -> []
  Just Table {..} -> subscribers


-- First we check the table exists and if the user is not already subscribed then we add them to the list of subscribers
-- Game and any other table updates will be propagated to those on the subscriber list
subscribeToTableHandler
  :: MsgIn -> ReaderT MsgHandlerConfig IO (Either Err MsgOut)
subscribeToTableHandler (SubscribeToTable tableName) = do
  msgHandlerConfig@MsgHandlerConfig {..} <- ask
  ServerState {..}                       <- liftIO $ readTVarIO serverStateTVar
  case M.lookup tableName $ unLobby lobby of
    Nothing         -> return $ Left $ TableDoesNotExist tableName
    Just Table {..} -> do
      let 
        privatisedGame = excludePrivateCards (Just (unUsername username)) game
        msg' = SuccessfullySubscribedToTable tableName privatisedGame
      if username `notElem` subscribers
        then do
          liftIO $ atomically $ subscribeToTable tableName msgHandlerConfig
          liftIO $ sendMsg clientConn msg'
          return $ Right msg'
        else do
          return $ Right msg'

          
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
