{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Socket.Types where

import Control.Concurrent (MVar)
import Data.Aeson.Types
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Text (Text)
import Data.Time.Clock
import Database.Persist.Postgresql (ConnectionString)
import GHC.Generics
import qualified Network.WebSockets as WS

import Types (Username)

data MsgHandlerConfig = MsgHandlerConfig
  { dbConn :: ConnectionString
  , serverState :: MVar ServerState
  , username :: Username
  , clientConn :: WS.Connection
  }

data Game = Game
  { id :: Text
  } deriving (Show)

data Client = Client
  { email :: Text
  , chips :: Int
  , conn :: WS.Connection
  }

instance Show Client where
  show Client {..} = show email

data ServerState = ServerState
  { clients :: Map Username Client
  , games :: Map Text Game
  }

instance Show ServerState where
  show ServerState {..} =
    show games ++ show (M.map (\Client {..} -> email) clients)

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
