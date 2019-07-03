{-# LANGUAGE RecordWildCards #-}

module Socket.Table where
import           Control.Concurrent      hiding ( yield )
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Data.Map.Lazy                 as M
import           Database.Persist.Postgresql    ( ConnectionString )
import qualified Network.WebSockets            as WS
import           Prelude


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


-- PROGRESS GAME HERE AFTER ACTIONS RATHER THAN ELSEWHERE SO
-- PROGRESSIONS AND EVAL PLAYER ACTIONS ARE DECOUPLED

-- fork a new thread which does the following tasks when a new gameState
-- 
--  
-- 1. when possible progress game to next staged 
-- 2. and broadcast
-- 3.  write new game state to db 
-- 4. log new gameState

-- TODO USE SERVERSTATE TVAR SO CAN GET SUBSCRIBERS TO BROADCAST TO AT ANYTIME
setUpTablePipes :: TableName -> Table -> IO (Async ())
setUpTablePipes name Table {..} =
  async
    $   forever
    $   runEffect
    $   fromInput gameOutMailbox
    >-> logGame name
    >-> P.map show
    >-> P.stdoutLn
    -- progressGame
   -- >->  -- broadcast
    -- writeGameToDB


progressGame'' :: Pipe Game Game IO ()
progressGame'' = undefined

gameStateProducer :: Input Game -> Producer Game IO ()
gameStateProducer source = fromInput source


gameToMsgOut :: TableName -> Pipe Game MsgOut IO ()
gameToMsgOut name = P.map $ NewGameState name

---- yields MsgOuts from new game states
gamePropagator = undefined

--getMsgOut :: TableName -> Pipe Game MsgOut IO ()
--getMsgOut name outgoingMailboxes g = forever $ do 
--    g <- await
--    yield (NewGameState name g)

-- write MsgOuts for new game states to outgoing mailbox for
-- client's who are observing the table
propagateGame :: [Client] -> Game -> Effect IO ()
propagateGame subscribers g = undefined


logGame :: TableName -> Pipe Game Game IO ()
logGame tableName = do
  g <- await
  liftIO $ print
    "woop /n \n \n table \n /n \n pipe \n got \n  a \n \n \n /n/n/n\n\n game"
  yield g

  -- P.chain print


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
