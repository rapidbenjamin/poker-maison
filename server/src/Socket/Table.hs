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


-- this is the effect we want to run everytime a new game state is placed in the tables
-- incoming mailbox for new game states.
-- New game states are send to the table's incoming mailbox every time a player acts
-- in a way that follows the game rules 
gamePipeline
  :: ConnectionString
  -> TVar ServerState
  -> Key TableEntity
  -> TableName
  -> Input Game
  -> Output Game
  -> Effect IO ()
gamePipeline connStr s key name outMailbox inMailbox =
  fromInput outMailbox
    >-> broadcast s name
    >-> logGame name
    >-> writeGameToDB connStr key
    >-> pause 3
    >-> progress inMailbox


pause :: Int -> Pipe a a IO ()
pause seconds = do
  a <- await
  _ <- liftIO $ threadDelay $ seconds * 1000000
  yield a



-- when the game can be progressed we get the progressed game an place it into the 
-- mailbox for the table which processes new game states
progress :: Output Game -> Consumer Game IO ()
progress inMailbox = do
  g <- await
  when (canProgressGame g) (progress' g)
 where
  progress' game = do
    gen <- liftIO getStdGen
    runEffect $ yield (progressGame gen game) >-> toOutput inMailbox


writeGameToDB :: ConnectionString -> Key TableEntity -> Pipe Game Game IO ()
writeGameToDB connStr tableKey = do 
  g <- await
  liftIO $ dbInsertGame connStr tableKey g
  yield g


-- write MsgOuts for new game states to outgoing mailbox for
-- client's who are observing the table
informSubscriber :: TableName -> Game -> Output MsgOut -> IO ()
informSubscriber n g outMailbox = do
  print "informing subscriber"
  runEffect $ yield msgOut >-> toOutput outMailbox
  return ()
  where msgOut = NewGameState n g


-- sends new game states to subscribers
-- At the moment all clients receive updates from every game indiscriminately
broadcast :: TVar ServerState -> TableName -> Pipe Game Game IO ()
broadcast s n =  do
  g <- await
  ServerState {..} <- liftIO $ readTVarIO s
  liftIO $ mapM_ (informSubscriber n g . outgoingMailbox) clients
  yield g

logGame :: TableName -> Pipe Game Game IO ()
logGame tableName = do
  g <- await
  liftIO $ print
    "woop /n \n \n table \n /n \n pipe \n got \n  a \n \n \n /n/n/n\n\n game"
  yield g



-- Lookups up a table with the given name and writes the new game state
-- to the gameIn mailbox for propagation to observers.
--
-- If table with tableName is not found in the serverState lobby 
-- then we just return () and do nothing.
toGameInMailbox :: TVar ServerState -> TableName -> Game -> IO ()
toGameInMailbox s name game = do
  print "SENT TO GAME IN MAILBOX \n \n \n __"
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
