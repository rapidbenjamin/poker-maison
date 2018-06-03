{-# LANGUAGE OverloadedStrings #-}

module Socket
  ( runSocketServer
  ) where

import Control.Concurrent (MVar, newMVar)
import qualified Data.Map.Lazy as M
import Database.Persist.Postgresql (ConnectionString)
import qualified Network.WebSockets as WS
import Prelude
import Web.JWT (Secret)

import Socket.Clients
import Socket.Types
import Types

initialServerState :: ServerState
initialServerState = ServerState {clients = M.empty, games = M.empty}

runSocketServer :: Secret -> Int -> ConnectionString -> RedisConfig -> IO ()
runSocketServer secretKey port dbConnString redisConfig = do
  serverState <- newMVar initialServerState
  print $ "Socket server listening on " ++ (show port :: String)
  WS.runServer "127.0.0.1" port $
    application secretKey dbConnString redisConfig serverState

-- New WS connections are expected to supply an access token as an initial msg
-- Once the token is verified the connection only then will the server state be 
-- updated with the newly authenticated client
application ::
     Secret
  -> ConnectionString
  -> RedisConfig
  -> MVar ServerState
  -> WS.ServerApp
application secretKey dbConnString redisConfig serverState pending = do
  newConn <- WS.acceptRequest pending
  WS.forkPingThread newConn 30
  msg <- WS.receiveData newConn
  authClient secretKey serverState dbConnString redisConfig newConn $ Token msg
