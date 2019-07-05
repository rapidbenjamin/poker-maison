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

getTableSubscribers :: TableName -> Lobby -> [Username]
getTableSubscribers tableName (Lobby lobby) = case M.lookup tableName lobby of
  Nothing         -> []
  Just Table {..} -> subscribers

  {-
-- | Connects the table channel which emits table updates to the client's socket connection
-- After a player subscribes to a table they are added to the subscribers list
-- This function forks a thread which broadcasts all the msgs from a tables channel to the
-- users on the subscribers list
notifyTableSubscribersLoop :: TVar ServerState -> TableName -> IO (Async ())
notifyTableSubscribersLoop serverState tableName = do
  ServerState {..} <- readTVarIO serverState
  let maybeTable = M.lookup tableName $ unLobby lobby
  case maybeTable of
    Nothing -> do
      atomically $ throwSTM $ TableDoesNotExistInLobby tableName
    Just table@Table {..} -> async $
      --  print "TABLE SUBSCRIPTION LOOP FORKED!!!!!!!!!!!!!!!!!-------"
                                     do
      dupTableChan <- atomically $ dupTChan channel
      forever $ do
        tableUpdateMsgOut <- atomically $ readTChan dupTableChan
        ServerState {..}  <- readTVarIO serverState
        let tableSubscribers = getTableSubscribers tableName lobby
   --     print "TABLE SUBSCRIPTION LOOP CALLED!!!!!!!!!!!!!!!!!-------"
        broadcastMsg clients tableSubscribers tableUpdateMsgOut
-}