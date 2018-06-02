{-# LANGUAGE OverloadedStrings #-}

module Socket
  ( runSocketServer
  ) where

import Control.Concurrent (MVar, newMVar)
import qualified Data.Map.Lazy as M
import Database.Persist.Postgresql (ConnectionString)
import qualified Network.WebSockets as WS
import Prelude

import Socket.Clients
import Socket.Types
import Types

initialServerState :: ServerState
initialServerState = ServerState {clients = M.empty, games = M.empty}

runSocketServer :: Int -> ConnectionString -> IO ()
runSocketServer port dbConnString = do
  serverState <- newMVar initialServerState
  print $ "Socket server listening on " ++ (show port :: String)
  WS.runServer "127.0.0.1" port $ application dbConnString serverState

-- New WS connections are expected to supply an access token as an initial msg
-- Once the token is verified the connection only then will the server state be 
-- updated with the newly authenticated client
application :: ConnectionString -> MVar ServerState -> WS.ServerApp
application dbConnString serverState pending = do
  newConn <- WS.acceptRequest pending
  WS.forkPingThread newConn 30
  msg <- WS.receiveData newConn
  authClient serverState dbConnString newConn $ Token msg
