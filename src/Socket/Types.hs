{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Socket.Types where

import Control.Concurrent (MVar)
import Data.Aeson
import Data.Aeson.Types
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Text
import Data.Time.Clock
import Database.Persist.Postgresql (ConnectionString)
import GHC.Generics
import qualified Network.WebSockets as WS

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
  deriving (Show, Ord, Read, Eq, Generic, ToJSON, FromJSON)

data Table = Table
  { observers :: [Username] -- not sat at table or on waitlist but subscribed to updates
  , waitList :: [Username] -- waiting to join a full table  and subscribed to updates
  , game :: Int
  } deriving (Show, Read, Ord, Eq, Generic, ToJSON, FromJSON)

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

data Msg
  = JoinTable -- incoming
  | LeaveTable -- incoming
  | SitAtTable
  | PlayerLeft -- outgoing
  | PlayerJoined
  | ErrMsg Err
  | AuthSuccess
  deriving (Show, Generic, FromJSON, ToJSON)

data Err
  = TableFull
  | NotEnoughChips
  | InvalidGameAction
  | AuthFailed Text
  deriving (Show, Generic, FromJSON, ToJSON)
