{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Socket.Clients where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, readMVar)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Either
import Data.Foldable
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Postgresql (ConnectionString, entityVal)
import qualified Network.WebSockets as WS
import Prelude
import Web.JWT (Secret)

import Auth
import Database
import Schema
import Socket.Types
import Socket.Utils
import Types

-- call handler function for all decodable JSON Messages with client and Msg
addClientMsgListener ::
     (MsgIn -> ReaderT MsgHandlerConfig IO ()) -> ReaderT MsgHandlerConfig IO ()
addClientMsgListener msgCallback = do
  msgHandlerConfig@MsgHandlerConfig {..} <- ask
  liftIO $
    forever $ do
      msg <- WS.receiveData clientConn
      print $ "raw msg: " ++ T.unpack msg
      print $ parseMsg msg
      for_ (parseMsg msg) $ \parsedMsg -> do
        print $ "parsed msg: " ++ show parsedMsg
        runReaderT (msgCallback parsedMsg) msgHandlerConfig
        return ()

authClient ::
     Secret
  -> MVar ServerState
  -> ConnectionString
  -> RedisConfig
  -> (MsgIn -> ReaderT MsgHandlerConfig IO ())
  -> WS.Connection
  -> Token
  -> IO ()
authClient secretKey state dbConn redisConfig msgHandler conn token = do
  authResult <- runExceptT $ verifyToken secretKey dbConn redisConfig token
  case authResult of
    (Left err) -> sendMsg conn $ ErrMsg $ AuthFailed err
    (Right User {..}) -> do
      sendMsg conn AuthSuccess
      ServerState {..} <- liftIO $ readMVar state
      updateServerState state $
        ServerState
          { clients =
              addClient Client {conn = conn, email = userEmail} username clients
          , ..
          }
      runReaderT (addClientMsgListener msgHandler) msgHandlerConfig
      where username = Username userUsername
            msgHandlerConfig =
              MsgHandlerConfig
                { serverState = state
                , username = username
                , dbConn = dbConn
                , clientConn = conn
                , redisConfig = redisConfig
                }

clientExists :: Username -> Map Username Client -> Bool
clientExists = M.member

addClient :: Client -> Username -> Map Username Client -> Map Username Client
addClient client username = M.insert username client

removeClient :: Map Username Client -> Username -> Map Username Client
removeClient clients username = M.delete username clients

getClient :: Map Username Client -> Username -> Maybe Client
getClient clients username = M.lookup username clients

sendMsgs :: [WS.Connection] -> MsgOut -> IO ()
sendMsgs conns msg = forM_ conns $ \conn -> sendMsg conn msg

sendMsg :: WS.Connection -> MsgOut -> IO ()
sendMsg conn msg = WS.sendTextData conn (encodeMsg msg)

sendMsgX :: WS.Connection -> MsgIn -> IO ()
sendMsgX conn msg = WS.sendTextData conn (encodeMsgX msg)

getClientConn :: Client -> WS.Connection
getClientConn Client {..} = conn
