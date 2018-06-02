{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Socket
  ( runSocketServer
  ) where

import Config
import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Data.Foldable
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Postgresql (ConnectionString)
import qualified Network.WebSockets as WS
import Prelude
import Socket.Clients
import Types

import Socket.Types

initialServerState :: ServerState
initialServerState = ServerState {clients = M.empty, games = M.empty}

runSocketServer :: Int -> ConnectionString -> IO ()
runSocketServer port dbConnString = do
  print "d"
  serverState <- newMVar initialServerState
  print $ "Socket server listening on " ++ (show port :: String)
  WS.runServer "127.0.0.1" port $ application dbConnString serverState

-- New WS connections are expected to supply an access token as an initial msg
-- Once the token is verified the connection only then will the server state be 
-- updated with the newly authenticated client
application :: ConnectionString -> MVar ServerState -> WS.ServerApp
application dbConn serverState pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  msg <- WS.receiveData conn
  print msg
  authClient serverState dbConn conn $ Token msg
