{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Socket.Clients where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, readMVar)
import Control.Exception
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
import qualified Data.Set as Set
import Database
import Schema
import Socket.Types
import Socket.Utils
import Types

-- call handler function for all decodable JSON Messages with client and Msg
addClientMsgListener ::
     (MsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) ())
  -> MsgHandlerConfig
  -> IO ()
addClientMsgListener msgCallback msgHandlerConfig@MsgHandlerConfig {..} = do
  finally
    (forever $ do
       msg <- WS.receiveData clientConn
       parseMsg msg msgHandlerConfig msgCallback)
    (removeClient username serverState)

-- if msg is successfully parsed fromJSON then we run our msg handler with the config consisting of
-- our apps configuration such as db connection and server state. When processing the client's msg
-- any errors will be thrown and thus propagated to the client in our Left handling logic here whereby
-- the error msg will be sent to the client
parseMsg ::
     Text
  -> MsgHandlerConfig
  -> (MsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) ())
  -> IO ()
parseMsg msg config@MsgHandlerConfig {..} msgCallback = do
  print $ "raw msg: " ++ T.unpack msg
  print $ parseMsgFromJSON msg
  for_ (parseMsgFromJSON msg) $ \parsedMsg -> do
    print $ "parsed msg: " ++ show parsedMsg
    result <- runExceptT $ runReaderT (msgCallback parsedMsg) config
    either (\err -> sendMsg clientConn $ ErrMsg err) return result
  return ()

--    runReaderT (msgCallback parsedMsg) msgHandlerConfig
authClient ::
     Secret
  -> MVar ServerState
  -> ConnectionString
  -> RedisConfig
  -> (MsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) ())
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
      addClientMsgListener msgHandler msgHandlerConfig
      print $ encodeMsgToJSON $ JoinTable "Black"
      where username = Username userUsername
            msgHandlerConfig =
              MsgHandlerConfig
                { serverState = state
                , username = username
                , dbConn = dbConn
                , clientConn = conn
                , redisConfig = redisConfig
                }

removeClient :: Username -> MVar ServerState -> IO ()
removeClient username serverStateMVar = do
  ServerState {..} <- readMVar serverStateMVar
  let newClients = M.delete username clients
  let newState = ServerState {clients = newClients, ..}
  updateServerState serverStateMVar newState

clientExists :: Username -> Map Username Client -> Bool
clientExists = M.member

addClient :: Client -> Username -> Map Username Client -> Map Username Client
addClient client username = M.insert username client

getClient :: Map Username Client -> Username -> Maybe Client
getClient clients username = M.lookup username clients

broadcastAllClients :: Map Username Client -> MsgOut -> IO ()
broadcastAllClients clients msg =
  forM_ (M.elems clients) (\Client {..} -> sendMsg conn msg)

broadcastTableSubscribers :: Table -> Map Username Client -> MsgOut -> IO ()
broadcastTableSubscribers Table {..} clients msg =
  forM_ subscriberConns (\Client {..} -> sendMsg conn msg)
  where
    subscriberConns = clients `M.restrictKeys` Set.fromList subscribers

sendMsgs :: [WS.Connection] -> MsgOut -> IO ()
sendMsgs conns msg = forM_ conns $ \conn -> sendMsg conn msg

sendMsg :: WS.Connection -> MsgOut -> IO ()
sendMsg conn msg = WS.sendTextData conn (encodeMsgToJSON msg)

sendMsgX :: WS.Connection -> MsgIn -> IO ()
sendMsgX conn msg = WS.sendTextData conn (encodeMsgX msg)

getClientConn :: Client -> WS.Connection
getClientConn Client {..} = conn

broadcastMsg :: Map Username Client -> [Username] -> MsgOut -> IO ()
broadcastMsg clients usernames msg =
  forM_ conns (\Client {..} -> sendMsg conn msg)
  where
    conns = clients `M.restrictKeys` Set.fromList usernames
