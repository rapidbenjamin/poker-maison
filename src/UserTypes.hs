{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UserTypes where

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
import Data.Char (toLower)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)

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
  , newUserName :: Text
  , newUserPassword :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON Register where
  parseJSON = genericParseJSON defaultOptions

type UserID = Text

data UserProfile = UserProfile
  { proName :: Text
  , proEmail :: Text
  , proUsername :: Text
  , proChips :: Int
  } deriving (Eq, Show, Generic)

instance ToJSON UserProfile where
  toJSON = genericToJSON defaultOptions

data DBUser = DBUser
  { usrId :: Int64
  , usrEmail :: Text
  , usrUsername :: Text
  , usrPassword :: Text
  } deriving (Eq, Show, Generic)

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
