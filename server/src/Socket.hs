{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Socket
  ( runSocketServer
  )
where

import           Control.Concurrent      hiding ( yield )
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Data.Map.Lazy                 as M
import           Database.Persist.Postgresql    ( ConnectionString )
import qualified Network.WebSockets            as WS
import           Prelude
import           Web.JWT                        ( Secret )

import           Socket.Clients
import           Socket.Lobby
import           Socket.Msg
import           Socket.Setup
import           Socket.Subscriptions
import           Socket.Types
import           Socket.Workers
import           Types

import           Control.Lens            hiding ( Fold, each)
import           Database
import           Poker.Types             hiding ( LeaveSeat )
--import           Data.Traversable
import           Data.Foldable                  ( traverse_ )

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy.Char8    as C
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as X
import qualified Data.Text.Lazy.Encoding       as D
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.STM
--import           Control.Monad.State.Lazy hiding (evalStateT)
import qualified Data.ByteString.Lazy          as BL

import           Socket.Types
import qualified Data.List                     as L
import qualified Data.Text.Lazy                as X

import qualified Data.Text.Lazy.Encoding       as D
import           System.Random

import           Poker.ActionValidation
import           Poker.Game.Blinds
import           Data.Either
import           System.Timeout
import           Poker.Game.Game
import           Poker.Types                    ( Player )
import           Data.Maybe
import           Poker.Game.Utils
import           Socket.Types
import           Socket.Msg
import           Socket.Utils
import           Data.ByteString.UTF8           ( fromString )

import           Poker.Poker
import           Crypto.JWT
import qualified Data.Aeson                    as A

import           Data.ByteString.Lazy           ( fromStrict
                                                , toStrict
                                                )

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Poker.Types

import           Control.Monad
import           Control.Exception
import qualified GHC.IO.Exception              as G

import qualified Network.WebSockets            as WS

import           Pipes.Aeson
import           Pipes
import           Pipes.Core                     ( push )
import           Pipes.Concurrent
import           Pipes.Parse             hiding ( decode
                                                , encode
                                                )
import qualified Pipes.Prelude                 as P

import           Socket.Table
import           Socket.TimeAction
import           Bots
import Socket.Clients (getSubscribedGameStates)


initialServerState :: Lobby -> ServerState
initialServerState lobby = ServerState { clients = M.empty, lobby = lobby }

-- Create the initial lobby holding all game state and then fork a new thread for each table in the lobby
-- to write new game states to the DB
runSocketServer
  :: BS.ByteString -> Int -> ConnectionString -> RedisConfig -> IO ()
runSocketServer secretKey port connString redisConfig = do
  putStrLn $ T.unpack $ encodeMsgX (GameMsgIn $ TakeSeat "black" 2222)
  putStrLn $ T.unpack $ encodeMsgX (GameMsgIn $ GameMove "black" $ Raise 400)
  putStrLn $ T.unpack $ encodeMsgX (GameMsgIn $ GameMove "black" Fold)

  putStrLn $ T.unpack $ encodeMsgX
    (GameMsgIn $ GameMove "black" $ PostBlind Big)

  lobby           <- initialLobby
  serverStateTVar <- atomically $ newTVar $ initialServerState lobby
  -- set up pipelines for broadcasting, progressing and logging new game states
  traverse_ (uncurry $ setUpTablePipes connString serverStateTVar)
            (M.toList $ unLobby lobby)
  -- workers for refilling chips
  forkBackgroundJobs connString serverStateTVar lobby
  print $ "Socket server listening on " ++ (show port :: String)
  _ <- forkIO $ WS.runServer "0.0.0.0" port $ application secretKey
                                                          connString
                                                          redisConfig
                                                          serverStateTVar
--  _ <- forkIO $ delayThenSeatPlayer connString 1000000 serverStateTVar bot1
--  _ <- forkIO $ delayThenSeatPlayer connString 2000000 serverStateTVar bot2
  --_ <- forkIO $ delayThenSeatPlayer connString 3000000 serverStateTVar bot3
 -- _ <- forkIO $ delayThenSeatPlayer connString 3000000 serverStateTVar bot4
 -- _ <- forkIO $ delayThenSeatPlayer connString 3000000 serverStateTVar bot5
 -- threadDelay 100000 --delay so bots dont start game until all of them sat down
  --_ <- forkIO $ startBotActionLoops connString serverStateTVar playersToWaitFor botNames
  return ()
 where
  botNames         = (^. playerName) <$> [bot1, bot2]
  playersToWaitFor = length botNames

-- subscriptions are handled by combining each subscribers mailbox into one large mailbox
-- where mew MsgOuts with new game states are posted
--
-- The new game state msgs will then propogate to to the subscribers mailbox and 
-- sent via their websocket connection automatically
subscribeToTable :: Output MsgOut -> Output MsgOut -> Output MsgOut
subscribeToTable tableOutput playerOutput = tableOutput <> playerOutput


-- Note this doesn't propagate new game state to clients just updates the game in the lobby
updateGame :: TVar ServerState -> TableName -> Game -> STM ()
updateGame s tableName g = do
  ServerState {..} <- readTVar s
  let newLobby = updateTableGame tableName g lobby
  writeTVar s ServerState { lobby = newLobby, .. }


-- creates a mailbox which has both an input sink and output source which
-- models the bidirectionality of websockets.
-- We return input source which emits our received socket msgs.
websocketInMailbox :: MsgHandlerConfig -> IO (Output MsgIn, Output MsgOut)
websocketInMailbox conf@MsgHandlerConfig {..} = do
  (writeMsgInSource , readMsgInSource ) <- spawn $ newest 1
  (writeMsgOutSource, readMsgOutSource) <- spawn $ newest 1
  async
    $   forever
    $   runEffect
    $   fromInput readMsgInSource
    >-> msgInHandler conf
    >-> toOutput writeMsgOutSource -- process received MsgIn's and place resulting MsgOut in outgoing mailbox
  async $ socketMsgOutWriter clientConn readMsgOutSource -- send encoded MsgOuts from outgoing mailbox to socket
  return (writeMsgInSource, writeMsgOutSource)


-- Runs an IO action forever which parses read MsgIn's from the websocket connection 
-- and puts them in our mailbox waiting to be processed by our MsgIn handler
-- 
--  Note - only parsed MsgIns make it into the mailbox - socket msgs which cannot be parsed
-- are silently ignored but logged anyway.
socketMsgInWriter :: WS.Connection -> Output MsgIn -> IO ()
socketMsgInWriter conn writeMsgInSource = do
  a <-
    async
    $   forever
    $   runEffect
    $   msgInDecoder (socketReader conn >-> logMsgIn)
    >-> toOutput writeMsgInSource
  link a


socketMsgOutWriter :: WS.Connection -> Input MsgOut -> IO (Async ())
socketMsgOutWriter conn is = forever $ runEffect $ for
  (fromInput is >-> msgOutEncoder)
  (lift . WS.sendTextData conn)


-- Converts a websocket connection into a producer 
socketReader :: WS.Connection -> Producer BS.ByteString IO ()
socketReader conn = forever $ do
  msg <- liftIO $ WS.receiveData conn
  liftIO $ print "oooooooooo Pipes Produced msg from Connection ooooooooooo"
  liftIO $ print msg
  liftIO $ print "ooooooooooooooooooooooooooooooooooooooooooooooooooooooooo"
  yield msg


-- Convert a raw Bytestring producer of raw JSON into a new producer which yields 
-- only successfully parsed values of type MsgIn.
--
-- Note that this parser deliberately ignores parsing errors as the naive implementation
-- would lead to parse errors closing the stream pipeline and thus the socket connection
msgInDecoder :: Producer BS.ByteString IO () -> Producer MsgIn IO ()
msgInDecoder rawMsgProducer = do
  (x, p') <- lift $ runStateT decode rawMsgProducer
  case x of
    Nothing       -> return ()
    Just (Left a) -> do
      (x, p'') <- lift $ runStateT draw p'
     -- lift $ print "left err"
     -- lift $ print x
      -- x is the problem input msg which failed to parse. We ignore it here by just resuming
      msgInDecoder p''
    Just c@(Right msgIn) -> do -- successful parsing case
   --   lift $ print c
      yield msgIn
      msgInDecoder p'


msgOutEncoder :: Pipe MsgOut Text IO ()
msgOutEncoder = do
  msgOut <- await
  -- lift $ print "encoding msg: "
  -- lift $ print msgOut
  yield $ encodeMsgToJSON msgOut


-- branches of code which do not yield messages place the burden of informing the client
-- onto the table pipeline as opposed to the remaining components after the player's socket
-- pipeline. Or in other words without yielding a msg this pipe will not directly inform the client
-- about what has happened.
msgInHandler :: MsgHandlerConfig -> Pipe MsgIn MsgOut IO ()
msgInHandler conf@MsgHandlerConfig {..} = do
  msgIn <- await
  res   <- lift $ runReaderT (msgHandler msgIn) conf
  case res of
    Left  err                        -> yield $ ErrMsg err
    Right (NewGameState tableName g) -> do
      liftIO $ async $ toGameInMailbox serverStateTVar tableName g
      liftIO $ atomically $ updateTable' serverStateTVar tableName g
      return ()
    Right m -> yield m


logMsgIn :: Pipe BS.ByteString BS.ByteString IO ()
logMsgIn = do
  msg <- await
  lift $ putStrLn "logging MsgIn"
  liftIO $ print msg
  yield msg
  --case x of
  --    -- Gracefully terminate if we got a broken pipe error
  --  Left e@G.IOError { G.ioe_type = t } ->
  --    lift $ unless (t == G.ResourceVanished) $ throwIO e
  --  -- Otherwise loop
  --  Right () -> yield msg >> logMsgIn


logMsgOut :: Pipe MsgOut MsgOut IO ()
logMsgOut = do
  msg <- await
  --lift $ putStrLn "logging MsgOut"
  --print msg
  yield msg


  -- case x of
  --     -- Gracefully terminate if we got a broken pipe error
  --   Left e@G.IOError { G.ioe_type = t } ->
  --     lift $ unless (t == G.ResourceVanished) $ throwIO e
  --   -- Otherwise loop
  --   Right () -> yield msg >> logMsgOut


-- get a pipe which only forwards the game moves which occur at the given table
filterMsgsForTable :: Monad m => TableName -> Pipe GameMsgIn GameMsgIn m ()
filterMsgsForTable tableName =
  P.filter $ \(GameMove tableName' _) -> tableName == tableName'


-- New WS connections are expected to supply an access token as an initial msg
-- Once the token is verified the connection only then will the server state be 
-- updated with the newly authenticated client.
--
-- After the client has been authenticated we fork a thread which writes
-- the clients msgs to a channel.
application :: BS.ByteString -> ConnectionString -> RedisConfig -> TVar ServerState -> WS.ServerApp
application secretKey dbConnString redisConfig s pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  authMsg          <- WS.receiveData conn
  ServerState {..} <- readTVarIO s
  eUsername        <- authClient secretKey
                                 s
                                 dbConnString
                                 redisConfig
                                 conn
                                 (Token authMsg)
  case eUsername of
    Right u@(Username clientUsername) -> do
      (incomingMailbox, outgoingMailbox) <- websocketInMailbox $ msgConf conn u
      let client = Client {..}
      sendMsg conn AuthSuccess
      let isReconnect = client `elem` clients -- if client already on our list of clients then this is a reconnect
      --when isReconnect $ do
      updateWithLatestGames client lobby -- Sync game state with reconnected clients
      let tableSummaries = TableList $ summariseTables lobby
     --   liftIO $ print tableSummaries
      liftIO $ sendMsg conn tableSummaries
      atomically $ addClient s client
      ServerState {..} <- liftIO $ atomically $ readTVar s
      liftIO $ print ""
      liftIO $ print "[[[[[[[[[[[[[[[[[[[[[[[[[[[["
      liftIO $ print $ T.pack "New Connection " <> (clientUsername)
      liftIO $ print clients
      liftIO $ print "[[[[[[[[[[[[[[[[[[[[[[[[[[[["
      liftIO $ print ""
      forever $ do
        m <- WS.receiveData conn
        liftIO $ print ""
        liftIO $ print "-------------- Raw msg received from socket -----------"
        liftIO $ print m
        liftIO $ print "-------------------------------------------------------"
        liftIO $ print ""
        runEffect
          $   msgInDecoder (yield m >-> logMsgIn)
          >-> toOutput incomingMailbox
    Left err -> sendMsg conn (ErrMsg err)
 where
  msgConf c username = MsgHandlerConfig
    { serverStateTVar = s
    , dbConn          = dbConnString
    , clientConn      = c
    , redisConfig     = redisConfig
    , ..
    }
