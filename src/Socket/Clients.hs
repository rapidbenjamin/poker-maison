{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Socket.Clients
  ( authClient
  ) where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Either
import Data.Foldable
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe
import Data.Text (Text)
import Database.Persist.Postgresql (ConnectionString, entityVal)
import qualified Network.WebSockets as WS
import Prelude

import Auth
import Database
import Schema
import Socket.Msg
import Socket.Types
import Socket.Utils
import Types

-- call handler function for all decodable JSON Messages with client and Msg
addClientMsgListener ::
     (Msg -> ReaderT MsgHandlerConfig IO a) -> ReaderT MsgHandlerConfig IO b
addClientMsgListener msgCallback = do
  MsgHandlerConfig {..} <- ask
  liftIO $
    forever $ do
      msg <- WS.receiveData clientConn
      return $ for_ (parseMsg msg) $ \parsedMsg -> msgCallback parsedMsg

authClient ::
     MVar ServerState -> ConnectionString -> WS.Connection -> Token -> IO ()
authClient state dbConn conn token = do
  authResult <- runExceptT $ verifyClientToken state dbConn conn token
  case authResult of
    (Left err) -> sendMsg conn $ ErrMsg $ AuthFailed err
    (Right username) ->
      let msgHandlerConfig =
            MsgHandlerConfig
              { serverState = state
              , username = username
              , dbConn = dbConn
              , clientConn = conn
              }
       in do sendMsg conn AuthSuccess
             runReaderT (addClientMsgListener msgHandler) msgHandlerConfig

verifyClientToken ::
     MVar ServerState
  -> ConnectionString
  -> WS.Connection
  -> Token
  -> ExceptT Text IO Username
verifyClientToken state dbConnString conn token = do
  ServerState {..} <- liftIO $ readMVar state
  email <- liftIO $ checkToken' token
  parsedToken <- liftIO $ checkToken' token
  case parsedToken of
    (Left err) -> throwError err
    (Right email) -> do
      maybeUser <- liftIO $ dbGetUserByEmail dbConnString email
      case maybeUser of
        Nothing -> throwNotInDbErr
        (Just userEntity) ->
          let User {..} = entityVal userEntity
           in return $ Username userUsername
  where
    throwNotInDbErr = (throwError "No User with Given Email Exists in DB")

clientExists :: Username -> Map Username Client -> Bool
clientExists = M.member

addClient :: Client -> Username -> Map Username Client -> Map Username Client
addClient client username = M.insert username client

removeClient :: Map Username Client -> Username -> Map Username Client
removeClient clients username = M.delete username clients

getClient :: Map Username Client -> Username -> Maybe Client
getClient clients username = M.lookup username clients

sendMsgs :: [WS.Connection] -> Msg -> IO ()
sendMsgs conns msg = forM_ conns $ \conn -> sendMsg conn msg

sendMsg :: WS.Connection -> Msg -> IO ()
sendMsg conn msg = WS.sendTextData conn (encodeMsg msg)

getClientConn :: Client -> WS.Connection
getClientConn Client {..} = conn
