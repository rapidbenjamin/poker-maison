{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Socket.Clients
  ( authClient
  ) where

import Config
import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Control.Exception (finally)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Either
import Data.Foldable
import Data.Foldable
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import qualified Network.WebSockets as WS
import qualified Network.WebSockets as WS
import Prelude
import Prelude
import Socket.Types

import Auth
import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Data.Foldable
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Database
import Database.Persist.Postgresql (ConnectionString, entityVal)
import Database.Persist.Postgresql (ConnectionString)
import Schema
import Socket.Types
import Socket.Utils

import Socket.Msg
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
    (Left err) -> do
      sendMsg conn $ ErrMsg $ AuthFailed err
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
      maybeUser <- liftIO $ dbGetUserByEmail dbConnString (email)
      case maybeUser of
        Nothing -> throwNotInDbErr
        (Just userEntity) ->
          let User {..} = entityVal userEntity
           in return $ Username userUsername
  where
    throwNotInDbErr = (throwError "No User with Given Email Exists in DB")
  {-
  case msg of
    _
      | clientExists username clients ->
        WS.sendTextData conn ("User already exists" :: Text)
      | otherwise ->
        flip finally disconnect $ do
          modifyMVar_ state $ \ServerState {..} -> do
            print clientName
            return ServerState {clients = addClient client clients, ..}
            clientListener state clientName conn msgHandler
      where clientName = T.filter (\c -> c `notElem` ['"', ' ']) msg
            client = Client {email = clientName, conn = conn}
                    -- disconnect is called when the connection is closed.
            disconnect
                        -- Remove client and return new state
             = do
              s <-
                modifyMVar state $ \ServerState {..} ->
                  let s' =
                        ServerState {clients = removeClient client clients, ..}
                   in return (s', s')
              return ()
    -}

clientExists :: Username -> Map Username Client -> Bool
clientExists username clients = M.member username clients

addClient :: Client -> Username -> Map Username Client -> Map Username Client
addClient client username clients = M.insert username client clients

removeClient :: Map Username Client -> Username -> Map Username Client
removeClient clients username = M.delete username clients

getClient :: Map Username Client -> Username -> Maybe Client
getClient clients username = M.lookup username clients

getClientWsConns :: [Client] -> [WS.Connection]
getClientWsConns = Prelude.map getClientConn

sendMsgs :: [WS.Connection] -> Msg -> IO ()
sendMsgs conns msg = forM_ conns $ \conn -> sendMsg conn msg

sendMsg :: WS.Connection -> Msg -> IO ()
sendMsg conn msg = WS.sendTextData conn (encodeMsg msg)

getClientConn :: Client -> WS.Connection
getClientConn Client {..} = conn
