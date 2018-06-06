{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Socket.Msg
  ( msgHandler
  ) where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, readMVar)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import Prelude
import Text.Pretty.Simple (pPrint)

import Socket.Clients
import Socket.Lobby
import Socket.Types
import Types

msgHandler :: MsgIn ->  ReaderT MsgHandlerConfig (ExceptT Err IO) ()
msgHandler msg@GetTables = undefined -- getTablesHandler

getTablesHandler ::  ReaderT MsgHandlerConfig (ExceptT Err IO) ()
getTablesHandler = do
  MsgHandlerConfig {..} <- ask
  ServerState {..} <- liftIO $ readMVar serverState
  liftIO $ sendMsg clientConn $ TableList lobby
