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
import Text.Pretty.Simple (pPrint)
import Web.JWT (Secret)

import Auth
import qualified Data.Set as Set
import Database
import Schema
import Socket.Types
import Socket.Utils
import Types

import Data.Maybe
import System.Timeout

-- This function will be used to process msgs from authenticated clients
-- call handler function for all decodable JSON Messages with client and Msg
authenticatedMsgLoop ::
     (MsgIn -> ReaderT MsgHandlerConfig (ExceptT Err IO) ())
  -> MsgHandlerConfig
  -> IO ()
authenticatedMsgLoop msgCallback msgHandlerConfig@MsgHandlerConfig {..}
 -- 
 = do
  finally
    (catch
       (forever $ do
          maybeMsg <- timeout 1000000 (WS.receiveData clientConn)
          print maybeMsg
          s@ServerState {..} <- liftIO $ readMVar serverState
          pPrint s
          let parsedMsg = maybe (Just Timeout) parseMsgFromJSON maybeMsg
          print parsedMsg
          for_ parsedMsg $ \parsedMsg -> do
            print $ "parsed msg: " ++ show parsedMsg
            result <-
              runExceptT $ runReaderT (msgCallback parsedMsg) msgHandlerConfig
            either (\err -> sendMsg clientConn $ ErrMsg err) return result)
       (\e -> do
          let err = show (e :: IOException)
          print
            ("Warning: Exception occured in authenticatedMsgLoop for " ++
             show username ++ ": " ++ err)
          (removeClient username serverState)
          return ()))
    (removeClient username serverState)

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
      authenticatedMsgLoop msgHandler msgHandlerConfig
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
