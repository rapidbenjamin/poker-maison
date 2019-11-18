{-# LANGUAGE RecordWildCards #-}

module Socket.Table where
import           Control.Concurrent      hiding ( yield )
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Data.Map.Lazy                 as M
import           Database.Persist.Postgresql
import qualified Network.WebSockets            as WS
import           Prelude

import           Database.Persist
import           Database.Persist.Postgresql    ( ConnectionString
                                                , SqlPersistT
                                                , runMigration
                                                , withPostgresqlConn
                                                )
import           Types

import           Control.Lens            hiding ( Fold )
import           Poker.Types             hiding ( LeaveSeat )
--import           Data.Traversable


import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy.Char8    as C
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as X
import qualified Data.Text.Lazy.Encoding       as D
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.STM
--import           Control.Monad.State.Lazy hiding (evalStateT)
import qualified Data.ByteString.Lazy          as BL

import           Socket.Types
import qualified Data.List                     as L

import           System.Random
import           Data.Map.Lazy                  ( Map )
import qualified Data.Map.Lazy                 as M

import           Poker.ActionValidation
import           Poker.Game.Blinds
import           Data.Either
import           System.Timeout
import           Poker.Game.Game
import           Poker.Types                    ( Player )
import           Data.Maybe
import           Poker.Game.Utils
--import           Socket
import           Socket.Types
import           Socket.Utils
import           System.Random

import           Data.ByteString.UTF8           ( fromString )

import           Database
import           Schema
import qualified Data.List.Safe                as Safe

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
import           Pipes hiding (next)
import           Pipes.Core                     ( push )
import           Pipes.Concurrent
import           Pipes.Parse             hiding ( decode
                                                , encode
                                                , next
                                                )
import qualified Pipes.Prelude                 as P
import           Poker.Game.Privacy
import           Socket.Clients



setUpTablePipes
  :: ConnectionString -> TVar ServerState -> TableName -> Table -> IO (Async ())
setUpTablePipes connStr s name Table {..} = do
  t <- dbGetTableEntity connStr name
  let (Entity key _) = fromMaybe notFoundErr t
  async $ forever $ runEffect $ gamePipeline connStr
                                             s
                                             key
                                             name
                                             gameOutMailbox
                                             gameInMailbox
  where notFoundErr = error $ "Table " <> show name <> " doesn't exist in DB"


-- this is the pipeline of effects we run everytime a new game state
-- is placed in the tables
-- incoming mailbox for new game states.
--
-- New game states are send to the table's incoming mailbox every time a player acts
-- in a way that follows the game rules 
--
-- Delays with "pause" at the end of each game stage (Flop, River etc) for UX
-- are done client side.
gamePipeline
  :: ConnectionString
  -> TVar ServerState
  -> Key TableEntity
  -> TableName
  -> Input Game
  -> Output Game
  -> Effect IO ()
gamePipeline connStr s key tableName outMailbox inMailbox = do
  fromInput outMailbox
    >-> broadcast s tableName
    >-> logGame tableName
    >-> updateTable s tableName
    >-> writeGameToDB connStr key
    >-> nextStagePause
   -- >-> timePlayer s tableName
    >-> progress inMailbox
    -- should all be in stm monad not IO -- perhaps


-- Delay to enhance UX based on game stages
timePlayer ::  TVar ServerState -> TableName -> Pipe Game Game IO ()
timePlayer s tableName = do
  g <- await
  liftIO $ forM_ (currPlayerNameToAct g) $ runPlayerTimer s tableName g
  yield g
  where
    currPlayerNameToAct g@Game{..} = 
        ((!!) (getGamePlayerNames g)) <$> _currentPosToAct

-----------------------------------
-- VERSION 2 OF TIMEOUTS  - v1 is in timeaction file---------

-- this version is better than version one because now
-- each client thread isn't watching to see if there is a 
--need to time their respective player
--
-- Instead, there is just one centralised game watcher which just uses
-- the game state when the timer starts and compares it with 
-- the game state when the timer ends.
-- if the is the same then the game state hasn't changed
-- which means the player we are timing hasnt acted.

-- If this is the case then we can deliver a timeout action
-- to force the progression of the game.
runPlayerTimer :: TVar ServerState -> TableName -> Game -> PlayerName -> IO (Async ())
runPlayerTimer s tableName gameWhenTimerStarts plyrName = 
  async $ do 
    threadDelay (5 * 1000000) -- 5 seconds
    mbTable <- atomically $ getTable s tableName
    case mbTable of 
      Nothing -> return ()
      Just Table{..} -> do 
        let
          gameHasNotProgressed = gameWhenTimerStarts == game
          playerStillHasToAct = doesPlayerHaveToAct plyrName game
        when (gameHasNotProgressed && playerStillHasToAct) $
           case runPlayerAction game timeoutAction of
              Left err -> print err
              Right progressedGame -> 
                runEffect $ yield progressedGame >-> toOutput gameInMailbox  
  where timeoutAction = PlayerAction {name = plyrName, action = Timeout }


-- Delay to enhance UX so game doesn't move through stages
-- instantly when no players can act i.e everyone all in.
nextStagePause :: Pipe Game Game IO ()
nextStagePause = do
  g <- await
  when (canProgressGame g) $ liftIO $ threadDelay $ pauseDuration g
  yield g
 where 
    pauseDuration :: Game -> Int
    pauseDuration g@Game{..} 
      | _street == PreDeal = 0
      | _street == Showdown = 5 * 1000000 -- 5 seconds
      | otherwise = 3 * 1000000 -- 3 seconds


-- Progresses to the next state which awaits a player action.
--
--- If the next game state is one where no player action is possible 
--  then we need to recursively progress the game.

--  These such states are:
--
--  1. everyone is all in.
--  1. All but one player has folded or the game. 
--  3. Game is in the Showdown stage.
--
-- After each progression the new game state is sent to the table 
-- mailbox. This sends the new game state through the pipeline that 
-- the previous game state just went through.
progress :: Output Game -> Consumer Game IO ()
progress inMailbox = do
  g <- await
  liftIO $ print "can progress game in pipe?"
  liftIO $ print $ (canProgressGame g)
  when (canProgressGame g) (progress' g)
 where
  progress' game = do
    gen <- liftIO getStdGen
    liftIO $ setStdGen $ snd $ next gen 
    liftIO $ print "PIPE PROGRESSING GAME"
    runEffect $ yield (progressGame gen game) >-> toOutput inMailbox


writeGameToDB :: ConnectionString -> Key TableEntity -> Pipe Game Game IO ()
writeGameToDB connStr tableKey = do
  g <- await
  _ <- liftIO $ async $ dbInsertGame connStr tableKey g
  yield g


-- write MsgOuts for new game states to outgoing mailbox for
-- client's who are observing the table
-- ensure they only get to see data they are allowed to see
informSubscriber :: TableName -> Game -> Client -> IO ()
informSubscriber n g Client {..} = do
 -- print "informing subscriber"
  let filteredGame = excludeOtherPlayerCards clientUsername g
  runEffect $ yield (NewGameState n filteredGame) >-> toOutput outgoingMailbox
  return ()

-- sends new game states to subscribers
-- At the moment all clients receive updates from every game indiscriminately
broadcast :: TVar ServerState -> TableName -> Pipe Game Game IO ()
broadcast s n = do
  g <- await
  ServerState {..} <- liftIO $ readTVarIO s
  let usernames' = M.keys clients -- usernames to broadcast to
  liftIO $ async $ mapM_ (informSubscriber n g) clients
  yield g

logGame :: TableName -> Pipe Game Game IO ()
logGame tableName = do
  g <- await
  yield g

-- Lookups up a table with the given name and writes the new game state
-- to the gameIn mailbox for propagation to observers.
--
-- If table with tableName is not found in the serverState lobby 
-- then we just return () and do nothing.
toGameInMailbox :: TVar ServerState -> TableName -> Game -> IO ()
toGameInMailbox s name game = do
  table' <- atomically $ getTable s name
  forM_ table' send
  where send Table {..} = runEffect $ yield game >-> toOutput gameInMailbox


-- Get a combined outgoing mailbox for a group of clients who are observing a table
-- 
-- Here we monoidally combined so we then have one mailbox 
-- we use to broadcast new game states to which will be sent out to each client's
-- socket connection under the hood
-- 
-- Warning:
-- You will pay a performance price if you combine thousands of Outputs ("mailboxes")
-- (thousands of subscribers) or more.
--
-- This is because by doing so will create a very large STM transaction. You can improve performance for very large broadcasts 
-- if you sacrifice atomicity and manually combine multiple send actions in IO
-- instead of STM.
combineOutMailboxes :: [Client] -> Consumer MsgOut IO ()
combineOutMailboxes clients = toOutput $ foldMap outgoingMailbox clients

getTable :: TVar ServerState -> TableName -> STM (Maybe Table)
getTable s tableName = do
  ServerState {..} <- readTVar s
  return $ M.lookup tableName $ unLobby lobby

updateTable :: TVar ServerState -> TableName -> Pipe Game Game IO ()
updateTable serverStateTVar tableName = do
  g <- await
  liftIO $ async $ atomically $ updateTable' serverStateTVar tableName g
  yield g

updateTable' :: TVar ServerState -> TableName -> Game -> STM ()
updateTable' serverStateTVar tableName newGame = do
  ServerState {..} <- readTVar serverStateTVar
  case M.lookup tableName $ unLobby lobby of
    Nothing               -> throwSTM $ TableDoesNotExistInLobby tableName
    Just table@Table {..} -> do
      let updatedLobby = updateTableGame tableName newGame lobby
      swapTVar serverStateTVar ServerState { lobby = updatedLobby, .. }
      return ()

updateTableGame :: TableName -> Game -> Lobby -> Lobby
updateTableGame tableName newGame (Lobby lobby) = Lobby
  $ M.adjust updateTable tableName lobby
  where updateTable Table {..} = Table { game = newGame, .. }
