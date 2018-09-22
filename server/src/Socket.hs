{-# LANGUAGE OverloadedStrings #-}

module Socket
  ( runSocketServer
  ) where

import Control.Concurrent (MVar, newMVar)
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Data.Map.Lazy as M
import Database.Persist.Postgresql (ConnectionString)
import qualified Network.WebSockets as WS
import Prelude
import Web.JWT (Secret)

import Socket.Clients
import Socket.Concurrency
import Socket.Lobby
import Socket.Msg
import Socket.Setup
import Socket.Subscriptions
import Socket.Types
import Socket.Workers
import Types

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as X
import qualified Data.Text.Lazy.Encoding as D

initialServerState :: Lobby -> ServerState
initialServerState lobby = ServerState {clients = M.empty, lobby = lobby}

-- Create the initial lobby holding all game state and then fork a new thread for each table in the lobby
-- to write new game states to the DB
runSocketServer :: Secret -> Int -> ConnectionString -> RedisConfig -> IO ()
runSocketServer secretKey port connString redisConfig = do
  lobby <- initialLobby
  serverStateTVar <- atomically $ newTVar $ initialServerState lobby
  forkBackgroundJobs connString serverStateTVar lobby
  print $ "Socket server listening on " ++ (show port :: String)
  WS.runServer "0.0.0.0" port $
    application secretKey connString redisConfig serverStateTVar

-- New WS connections are expected to supply an access token as an initial msg
-- Once the token is verified the connection only then will the server state be 
-- updated with the newly authenticated client.
--
-- After the client has been authenticated we fork a thread which writes
-- the clients msgs to a channel.
application ::
     Secret
  -> ConnectionString
  -> RedisConfig
  -> TVar ServerState
  -> WS.ServerApp
application secretKey dbConnString redisConfig serverState pending = do
  newConn <- WS.acceptRequest pending
  WS.forkPingThread newConn 30
  msg <- WS.receiveData newConn
  authClient
    secretKey
    serverState
    dbConnString
    redisConfig
    authenticatedMsgLoop
    newConn $
    Token msg
