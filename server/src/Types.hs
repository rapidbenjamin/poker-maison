{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Types where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time.Clock
import Database.Redis (ConnectInfo)
import GHC.Generics (Generic)
import Servant

type RedisConfig = ConnectInfo

type Password = Text

data Login = Login
  { loginUsername :: Text
  , loginPassword :: Text
  } deriving (Eq, Show, Generic, FromJSON)

data Register = Register
  { newUserEmail :: Text
  , newUsername :: Username
  , newUserPassword :: Text
  } deriving (Eq, Show, Generic, FromJSON)

newtype Username =
  Username Text
  deriving (Generic, Show, Read, Eq, Ord)


unUsername :: Username -> Text
unUsername (Username username) = username

instance ToJSON Username

instance FromJSON Username

type UserID = Text

data UserProfile = UserProfile
  { proUsername :: Username
  , proEmail :: Text
  , proAvailableChips :: Int
  , proChipsInPlay :: Int
  , proUserCreatedAt :: UTCTime
  } deriving (Eq, Show, Generic, ToJSON)

data ReturnToken = ReturnToken
  { access_token :: Text
  , refresh_token :: Text
  , expiration :: Int --seconds to expire
  } deriving (Generic, ToJSON)

newtype Token =
  Token Text deriving Generic

instance FromHttpApiData Token where
  parseQueryParam t =
    let striped = T.strip t
        ls = T.words striped
     in case ls of
          "Bearer":r:_ -> Right $ Token r
          _ -> Left "Invalid Token"

      