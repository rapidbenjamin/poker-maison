{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import Data.Text (Text)
import Database.Redis (ConnectInfo)
import GHC.Generics (Generic)
import Servant

type RedisConfig = ConnectInfo

type Password = Text

data Login = Login
  { loginEmail :: Text
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

instance ToJSON Username

instance FromJSON Username

type UserID = Text

data UserProfile = UserProfile
  { proUsername :: Username
  , proEmail :: Text
  , proChips :: Int
  } deriving (Eq, Show, Generic, ToJSON)

data ReturnToken = ReturnToken
  { access_token :: Text
  , refresh_token :: Text
  , expiration :: Int --seconds to expire
  } deriving (Generic, ToJSON)

newtype Token =
  Token Text

instance FromHttpApiData Token where
  parseQueryParam t =
    let striped = T.strip t
        ls = T.words striped
     in case ls of
          "Bearer":r:_ -> Right $ Token r
          _ -> Left "Invalid Token"
