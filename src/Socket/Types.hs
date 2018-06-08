{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Socket.Types where

import Control.Concurrent (MVar)
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Types
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Text
import Data.Time.Clock
import Database.Persist.Postgresql (ConnectionString)
import GHC.Generics
import qualified Network.WebSockets as WS

import Poker.Types (Game, GameErr, PlayerAction)
import Types (RedisConfig, Username)

data MsgHandlerConfig = MsgHandlerConfig
  { dbConn :: ConnectionString
  , serverState :: MVar ServerState
  , username :: Username
  , clientConn :: WS.Connection
  , redisConfig :: RedisConfig
  }

type TableName = Text

newtype Lobby =
  Lobby (Map TableName Table)
  deriving (Show, Ord, Eq, Read, Generic, ToJSON, FromJSON)

data Table = Table
  { subscribers :: [Username] -- not sat at table or on waitlist but observing game state
  , waitlist :: [Username] -- waiting to join a full table  and subscribed to updates
  , game :: Game
  } deriving (Show, Ord, Eq, Read, Generic, ToJSON, FromJSON)

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
  } deriving (Show)

instance Eq Client where
  Client {email = email1} == Client {email = email2} = email1 == email2

-- incoming messages from a ws client
data MsgIn
  = GetTables
  | JoinTable TableName
  | LeaveTable
  | TakeSeat TableName
  | LeaveSeat
  | GameMove TableName
             PlayerAction
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- outgoing messages for ws client(s)
data MsgOut
  = TableList Lobby -- TODO only broadcast public table info
  | NewTableList Lobby -- TODO only broadcast public table info
  | PlayerLeft
  | PlayerJoined
  | NewGameState TableName
                 Game
  | ErrMsg Err
  | AuthSuccess
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Err
  = TableFull TableName
  | TableDoesNotExist TableName
  | NotSatAtTable TableName
  | AlreadySatInGame TableName
  | AlreadySatAtTable TableName
  | AlreadySubscribedToTable TableName
  | NotEnoughChips
  | GameErr GameErr
  | InvalidGameAction
  | AuthFailed Text
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
