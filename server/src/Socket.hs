{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Socket
  ( runSocketServer
  ) where

import Control.Concurrent 
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Data.Map.Lazy as M
import Database.Persist.Postgresql (ConnectionString)
import qualified Network.WebSockets as WS
import Prelude
import Web.JWT (Secret)

import Socket.Clients
import Socket.Concurrency
import Socket.Lobby
import Socket.Msg
import Socket.Setup
import Socket.Subscriptions
import Socket.Types
import Socket.Workers
import Types

import Control.Lens hiding (Fold)
import Database
import Poker.Types hiding (LeaveSeat)
import Data.Traversable


import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as X
import qualified Data.Text.Lazy.Encoding as D
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.STM
import Control.Monad.State.Lazy

import Socket.Types
import qualified Data.List as L

import System.Random

import Poker.ActionValidation
import Poker.Game.Blinds
import Data.Either
import System.Timeout
import Poker.Game.Game
import Poker.Types (Player)
import Data.Maybe
import Poker.Game.Utils
import Socket.Types
import Socket.Msg
import Socket.Utils
import Poker.Poker

initialServerState :: Lobby -> ServerState
initialServerState lobby = ServerState {clients = M.empty, lobby = lobby}

-- Create the initial lobby holding all game state and then fork a new thread for each table in the lobby
-- to write new game states to the DB
runSocketServer :: Secret -> Int -> ConnectionString -> RedisConfig -> IO ()
runSocketServer secretKey port connString redisConfig = do
  lobby <- initialLobby
  serverStateTVar <- atomically $ newTVar $ initialServerState lobby
  forkBackgroundJobs connString serverStateTVar lobby
  print $ "Socket server listening on " ++ (show port :: String)
  _ <- forkIO $ delayThenSeatPlayer connString 1000000 serverStateTVar bot1
  _ <- forkIO $ delayThenSeatPlayer connString 2000000 serverStateTVar bot2
  _ <- forkIO $ delayThenSeatPlayer connString 3000000 serverStateTVar bot3
  threadDelay 900000 --delay so bots dont start game until all of them sat down
  forkIO $ startBotActionLoops connString serverStateTVar botNames
  WS.runServer "0.0.0.0" port $
    application secretKey connString redisConfig serverStateTVar
 where botNames = (^. playerName) <$> [bot1, bot2, bot3]

-- New WS connections are expected to supply an access token as an initial msg
-- Once the token is verified the connection only then will the server state be 
-- updated with the newly authenticated client.
--
-- After the client has been authenticated we fork a thread which writes
-- the clients msgs to a channel.
application ::
     Secret
  -> ConnectionString
  -> RedisConfig
  -> TVar ServerState
  -> WS.ServerApp
application secretKey dbConnString redisConfig serverState pending = do
  newConn <- WS.acceptRequest pending
  WS.forkPingThread newConn 30
  msg <- WS.receiveData newConn
  authClient
    secretKey
    serverState
    dbConnString
    redisConfig
    authenticatedMsgLoop
    newConn $
    Token msg

    -------- bots


delayThenSeatPlayer :: ConnectionString -> Int -> TVar ServerState -> Player -> IO ()
delayThenSeatPlayer dbConn delayDuration s p = do
  print "delaying before sit down bot ... "
  _ <- threadDelay delayDuration
  print "about to sit down bot ... "
  sitDownBot dbConn p s
  print "... done . bot sat down "


bot1 :: Player
bot1 = initPlayer "1@1" 2000


bot2 :: Player
bot2 = initPlayer "2@2" 2000

bot3 :: Player
bot3 = initPlayer "3@3" 2000

--    dupTableChanMsg <- atomically $ readTChan dupTableChan

startBotActionLoops :: ConnectionString -> TVar ServerState -> [PlayerName] -> IO ()
startBotActionLoops db s botNames = do
  threadDelay 2000000 --delay so bots dont start game until all of them sat down
  ServerState{..} <- readTVarIO s
  case M.lookup tableName $ unLobby lobby of
    Nothing -> error "TableDoesNotExist "
    Just table@Table {..} -> mapM_ (botActionLoop db s channel) botNames
 where tableName = "Black"


botActionLoop :: ConnectionString -> TVar ServerState -> TChan MsgOut -> PlayerName -> IO ThreadId
botActionLoop dbConn s tableChan botName = forkIO $ do 
    chan <- atomically $ dupTChan tableChan
    print botName
    print "create action loop"
    forever $ do
      msg <- atomically $ readTChan chan
      print "action received"
      case msg of
        (NewGameState tableName g) -> actIfNeeded g botName
        _ -> return ()
  where
    actIfNeeded g' pName' =
       let hasToAct = doesPlayerHaveToAct pName' g'
         in 
          when (hasToAct || (blindRequiredByPlayer g' pName' /= NoBlind)) $ 
            do
              print hasToAct
              runBotAction dbConn s g' pName'


runBotAction :: ConnectionString -> TVar ServerState -> Game -> PlayerName -> IO ()
runBotAction dbConn serverStateTVar g pName = do
      maybeAction <- getValidAction g pName
      print g
      print ("Random action from " <> show pName  <> " is " <> show maybeAction)
      case maybeAction of 
         Nothing -> return ()
         Just a -> do 
             eitherNewGame <- runPlayerAction g pName a
             case eitherNewGame of
               Left gameErr -> print (show $ GameErr gameErr) >> return ()
               Right newGame -> do
                 atomically $ updateGameAndBroadcastT serverStateTVar tableName newGame
                 progressGame' dbConn serverStateTVar tableName newGame
    where
      tableName = "Black"
      chipsToSit = 2000

sitDownBot :: ConnectionString -> Player -> TVar ServerState -> IO ()
sitDownBot dbConn player@Player{..} serverStateTVar = do
    s@ServerState {..} <- readTVarIO serverStateTVar
    let gameMove = SitDown player
    case M.lookup tableName $ unLobby lobby of
      Nothing -> error "table doesnt exist" >> return ()
      Just Table {..} -> do
          eitherNewGame <- liftIO $ runPlayerAction game _playerName takeSeatAction
          case eitherNewGame of
            Left gameErr -> print $ GameErr gameErr
            Right newGame -> do
              dbDepositChipsIntoPlay dbConn _playerName chipsToSit
              atomically $ updateGameAndBroadcastT serverStateTVar tableName newGame
  where 
    chipsToSit = 2000
    tableName = "Black"
    takeSeatAction = (SitDown player)

--runBotAction :: TVar ServerState -> TableName -> Game -> PlayerAction -> STM ()
--runBotAction serverS tableName g botAction = do


actions :: Street -> Int -> [PlayerAction]
actions st chips | st == PreDeal = [PostBlind Big, PostBlind Small]
                 | otherwise     = [Check, Call, Fold, Bet chips, Raise chips]


getValidAction :: Game -> PlayerName -> IO (Maybe PlayerAction)
getValidAction g@Game{..} name  
  | length _players < 2 = return Nothing  
  | _street == PreDeal =  return $ case blindRequiredByPlayer g name of
                        Small -> Just $ PostBlind Small
                        Big ->  Just $ PostBlind Big
                        NoBlind -> Nothing
  | otherwise = 
     do
      betAmount' <- randomRIO (lowerBetBound, chipCount)
      let possibleActions = (actions _street betAmount')
      let actionsValidated = validateAction g name <$> possibleActions
      let actionPairs = zip possibleActions actionsValidated
      print actionPairs
      let validActions = (<$>) fst $ filter (isRight . snd) actionPairs
      print validActions
      if null validActions then panic else return ()

      randIx <- randomRIO (0, length validActions - 1)

      return $ Just $ validActions !! randIx
    where 
        lowerBetBound = if (_maxBet > 0) then (2 * _maxBet) else _bigBlind
        chipCount = fromMaybe 0 ((^. chips) <$> (getGamePlayer g name))
        panic = do
          print $ "UHOH no valid actions for " <> show name
          print g
          error $ "UHOH no valid actions"
