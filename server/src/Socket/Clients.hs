{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Socket.Clients where


import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Either
import           Data.Foldable
import           Data.Map.Lazy                  ( Map )
import qualified Data.Map.Lazy                 as M
import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Database.Persist.Postgresql    ( ConnectionString
                                                , entityVal
                                                )
import qualified Network.WebSockets            as WS
import           Prelude
import           Text.Pretty.Simple             ( pPrint )
import qualified Data.ByteString.Lazy          as BL

import qualified Data.Set                      as Set
import           Database
import           Schema
import           Socket.Types
import           Socket.Utils
import           Types
import           Servant.Auth.Server
import qualified Data.ByteString               as BS

import           Crypto.JOSE                   as Jose
import           Crypto.JWT                    as Jose

import           Data.Maybe
import           System.Timeout
import           Crypto.JWT
import           Poker.Game.Privacy             ( excludeAllPlayerCards
                                                , excludeOtherPlayerCards
                                                )
import qualified Data.ByteString.Lazy.Char8    as C
import           Socket.Auth


authClient
  :: BS.ByteString
  -> TVar ServerState
  -> ConnectionString
  -> RedisConfig
  -> WS.Connection
  -> Token
  -> IO ()
authClient secretKey state dbConn redisConfig conn (Token token) = do
  authResult <- runExceptT $ liftIO $ verifyJWT secretKey
                                                (C.pack $ T.unpack token)
  case authResult of
    (Left err) -> sendMsg conn $ ErrMsg $ AuthFailed err
    (Right (Left err)) ->
      sendMsg conn $ ErrMsg $ AuthFailed $ T.pack $ show err
    (Right (Right claimsSet)) -> case (decodeJWT claimsSet) of
      (Left jwtErr) ->
        sendMsg conn $ ErrMsg $ AuthFailed $ T.pack $ show jwtErr
      (Right username@(Username name)) -> do
        sendMsg conn AuthSuccess
        ServerState {..} <- readTVarIO state
        atomically $ swapTVar
          state
          (ServerState
            { clients = addClient
              Client { conn = conn, clientUsername = name }
              username
              clients
            , ..
            }
          )
        return ()
         --   socketReadChan <- newTChanIO
         --   let msgHandlerConfig =
         --         MsgHandlerConfig
         --           { serverStateTVar = state
         --           , dbConn = dbConn
         --           , clientConn = conn
         --           , redisConfig = redisConfig
         --           , ..
         --           }
--       --     authMsgLoop msgHandlerConfig

removeClient :: Username -> TVar ServerState -> IO ServerState
removeClient username serverStateTVar = do
  ServerState {..} <- readTVarIO serverStateTVar
  let newClients = M.delete username clients
  let newState   = ServerState { clients = newClients, .. }
  atomically $ swapTVar serverStateTVar newState

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
broadcastTableSubscribers Table {..} clients msg = forM_
  subscriberConns
  (\Client {..} -> sendMsg conn msg)
  where subscriberConns = clients `M.restrictKeys` Set.fromList subscribers

sendMsgs :: [WS.Connection] -> MsgOut -> IO ()
sendMsgs conns msg = forM_ conns $ \conn -> sendMsg conn msg

sendMsg :: WS.Connection -> MsgOut -> IO ()
sendMsg conn (SuccessfullySubscribedToTable tableName game) =
  let msg' =
          SuccessfullySubscribedToTable tableName (excludeAllPlayerCards game)
  in  WS.sendTextData conn (encodeMsgToJSON msg')
sendMsg conn msg = WS.sendTextData conn (encodeMsgToJSON msg)

sendMsgX :: WS.Connection -> MsgIn -> IO ()
sendMsgX conn msg = WS.sendTextData conn (encodeMsgX msg)

getClientConn :: Client -> WS.Connection
getClientConn Client {..} = conn

broadcastMsg :: Map Username Client -> [Username] -> MsgOut -> IO ()
broadcastMsg clients usernames msg = forM_
  conns
  (\Client {..} -> sendMsg conn $ filterPrivateGameData clientUsername msg)
  where conns = clients `M.restrictKeys` Set.fromList usernames

-- Filter out private data such as other players cards which is not
-- intended for the client.
filterPrivateGameData :: Text -> MsgOut -> MsgOut
filterPrivateGameData username (SuccessfullySatDown tableName game) =
  SuccessfullySatDown tableName (excludeOtherPlayerCards username game)
filterPrivateGameData username (SuccessfullySubscribedToTable tableName game) =
  SuccessfullySubscribedToTable tableName
                                (excludeOtherPlayerCards username game)
filterPrivateGameData username (GameMsgOut (NewGameState tableName game)) =
  GameMsgOut $ NewGameState tableName (excludeOtherPlayerCards username game)
filterPrivateGameData _ unfilteredMsg = unfilteredMsg
