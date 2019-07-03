{-# LANGUAGE RecordWildCards #-}

module Socket.Table where
import           Control.Concurrent      hiding ( yield )
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Data.Map.Lazy                 as M
import           Database.Persist.Postgresql    ( ConnectionString )
import qualified Network.WebSockets            as WS
import           Prelude

import           Socket.Clients
import           Socket.Lobby
import           Socket.Setup
import           Socket.Subscriptions
import           Socket.Workers
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

import           Poker.ActionValidation
import           Poker.Game.Blinds
import           Data.Either
import           System.Timeout
import           Poker.Game.Game
import           Poker.Types                    ( Player )
import           Data.Maybe
import           Poker.Game.Utils
import           Socket
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



tableGameStates :: Input Game -> Producer Game IO ()
tableGameStates source = fromInput source


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


logGame :: Pipe Game Game IO ()
logGame = P.chain print

-- Get a combined outgoing mailbox for a group of clients who are observing a table
-- 
-- Here we monoidally combined so we then have one mailbox 
-- we use to broadcast new game states to which will be sent out to each client's
-- socket connection under the hood
-- 
-- Warning:
-- You will pay a performance price if you combine thousands of Outputs 
-- (thousands of subscribers) or more.
--
-- This is because by doing so will create a very large STM transaction. You can improve performance for very large broadcasts 
-- if you sacrifice atomicity and manually combine multiple send actions in IO
-- instead of STM.
combineOutMailboxes :: [Client] -> Consumer MsgOut IO ()
combineOutMailboxes clients = toOutput $ foldMap outgoingMailbox clients
