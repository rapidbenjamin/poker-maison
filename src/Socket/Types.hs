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

import Poker.Types (Game, GameErr)
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
  { observers :: [Username] -- not sat at table or on waitlist but subscribed to updates
  , waitList :: [Username] -- waiting to join a full table  and subscribed to updates
  , game :: Game
  } deriving (Show, Ord, Eq, Read, Generic, ToJSON, FromJSON)

data Client = Client
  { email :: Text
  , conn :: WS.Connection
  }

instance Show Client where
  show Client {..} = show email

data ServerState = ServerState
  { clients :: Map Username Client
  , lobby :: Lobby
  } deriving (Show)

instance Eq Client where
  Client {email = email1} == Client {email = email2} = email1 == email2

-- incoming messages from a ws client
data MsgIn
  = GetTables
  | JoinTable
  | LeaveTable
  | TakeSeat
  | LeaveSeat
  | GameAction TableName
               GameAction
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data GameAction
  = Fold
  | Call
  | Raise Int
  | Check
  | Bet Int
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- outgoing messages for ws client(s)
data MsgOut
  = TableList Lobby
  | PlayerLeft
  | PlayerJoined
  | NewGameState Int
  | ErrMsg Err
  | AuthSuccess
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Err
  = TableFull TableName
  | TableDoesNotExist TableName
  | NotEnoughChips
  | InvalidGameAction
  | AuthFailed Text
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
