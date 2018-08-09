{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Socket.Types where

import Control.Concurrent (MVar)
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Exception
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Types
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Monoid
import Data.Text
import Data.Time.Clock
import Database.Persist.Postgresql (ConnectionString)
import GHC.Generics
import qualified Network.WebSockets as WS

import Poker.Types (Game, GameErr, PlayerAction)
import Types (RedisConfig, Username)

data MsgHandlerConfig = MsgHandlerConfig
  { dbConn :: ConnectionString
  , serverStateTVar :: TVar ServerState
  , username :: Username
  , clientConn :: WS.Connection
  , redisConfig :: RedisConfig
  , socketReadChan :: TChan MsgIn
  }

type TableName = Text

newtype Lobby =
  Lobby (Map TableName Table)
  deriving (Ord, Eq)

instance Show Lobby

instance Show ServerState

newtype TableDoesNotExistEx =
  TableDoesNotExistEx Text
  deriving (Show)

instance Exception TableDoesNotExistEx

data Table = Table
  { subscribers :: [Username] -- not sat at table or on waitlist but observing game state
  , waitlist :: [Username] -- waiting to join a full table  and subscribed to updates
  , game :: Game
  , channel :: TChan MsgOut
  }

instance Show Table where
  show Table {..} =
    show subscribers <> "\n" <> show waitlist <> "\n" <> show game

instance Eq Table where
  Table {game = game1} == Table {game = game2} = game1 == game2

instance Ord Table where
  Table {game = game1} `compare` Table {game = game2} = game1 `compare` game2

data Client = Client
  { email :: Text
  , conn :: WS.Connection
  }

instance Show Client where
  show Client {..} = show email

-- TODO wrap clients Map in a newtype
data ServerState = ServerState
  { clients :: Map Username Client
  , lobby :: Lobby
  }

instance Eq Client where
  Client {email = email1} == Client {email = email2} = email1 == email2

-- incoming messages from a ws client
data MsgIn
  = GetTables
  | JoinTable TableName
  | LeaveTable
  | TakeSeat TableName
             Int
  | LeaveSeat TableName
  | GameMove TableName
             PlayerAction
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- outgoing messages for ws client(s)
data MsgOut
  = TableList -- TODO only broadcast public table info -- add list of tables to msg
  | NewTableList -- TODO only broadcast public table info -- add list of tables to msg
  | PlayerLeft
  | SuccessfullySatDown TableName
  | SuccessfullyLeftSeat TableName
  | PlayerJoined TableName
                 Text
  | NewGameState TableName
                 Game
  | ErrMsg Err
  | AuthSuccess
  | Noop
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Err
  = TableFull TableName
  | TableDoesNotExist TableName
  | NotSatAtTable TableName
  | AlreadySatInGame TableName
  | NotSatInGame TableName
  | AlreadySatAtTable TableName
  | AlreadySubscribedToTable TableName
  | NotEnoughChipsToSit
  | GameErr GameErr
  | InvalidGameAction
  | ChipAmountNotWithinBuyInRange TableName
  | UserDoesNotExistInDB Text
  | AuthFailed Text
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
