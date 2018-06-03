{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Schema where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import qualified Database.Persist.TH as PTH
import Servant.Docs

PTH.share
  [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"]
  [PTH.persistLowerCase|
  User sql=users
    username Text
    email Text
    password Text
    chips Int
    UniqueEmail email
    UniqueUsername username
    deriving Show Read
|]

instance FromJSON User where
  parseJSON (Object v) =
    User <$> v .: "username" <*> v .: "email" <*> v .: "chips" <*>
    v .: "password"
  parseJSON _ = mzero

instance ToJSON User where
  toJSON p =
    object
      [ "username" .= userUsername p
      , "email" .= userEmail p
      , "chips" .= userChips p
      , "password" .= userPassword p
      ]

parseUser :: Object -> Parser User
parseUser o = do
  uUsername <- o .: "username"
  uEmail <- o .: "email"
  uChips <- o .: "chips"
  uPassword <- o .: "password"
  return
    User
      { userUsername = uUsername
      , userPassword = uPassword
      , userEmail = uEmail
      , userChips = uChips
      }

instance ToSample User where
  toSamples _ = [("Sample User", g)]
    where
      g =
        User
          { userChips = 2000
          , userUsername = "Tom"
          , userEmail = "gooby@g.com"
          , userPassword = "n84!@R5G"
          }
