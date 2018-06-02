{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import Data.Aeson (decode)
import Data.Aeson.Types
  ( FromJSON
  , ToJSON
  , (.:)
  , (.=)
  , defaultOptions
  , fieldLabelModifier
  , genericParseJSON
  , genericToJSON
  , object
  , parseJSON
  , toJSON
  , withObject
  )
import Data.Aeson.Types
import Data.Aeson.Types
import Data.Char (toLower)
import Data.IntMap
import Data.Map.Lazy (Map)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Clock
import Data.Time.Clock
import GHC.Generics

import GHC.Generics (Generic)
import GHC.Int (Int64)
import Servant

type Password = Text

data Login = Login
  { loginEmail :: Text
  , loginPassword :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON Login where
  parseJSON = genericParseJSON defaultOptions

data Register = Register
  { newUserEmail :: Text
  , newUsername :: Username
  , newUserPassword :: Text
  } deriving (Eq, Show, Generic)

newtype Username =
  Username Text
  deriving (Generic, FromJSON, ToJSON, Show, Eq, Ord)

instance FromJSON Register where
  parseJSON = genericParseJSON defaultOptions

type UserID = Text

data UserProfile = UserProfile
  { proUsername :: Username
  , proEmail :: Text
  , proChips :: Int
  } deriving (Eq, Show, Generic)

instance ToJSON UserProfile where
  toJSON = genericToJSON defaultOptions

data ReturnToken = ReturnToken
  { access_token :: Text
  , refresh_token :: Text
  , expiration :: Int --seconds to expire
  } deriving (Generic)

instance ToJSON ReturnToken

newtype Token =
  Token Text

instance FromHttpApiData Token where
  parseQueryParam t =
    let striped = T.strip t
        ls = T.words striped
     in case ls of
          "Bearer":r:_ -> Right $ Token r
          _ -> Left "Invalid Token"
