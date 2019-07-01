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

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text                      ( Text )
import           Data.Time.Clock
import           Database.Persist.TH
import           Servant.Docs

import           Poker.Types

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  UserEntity json sql=users
    username Text
    email Text
    password Text
    availableChips Int
    chipsInPlay Int
    createdAt UTCTime default=now()
    UniqueEmail email
    UniqueUsername username
    deriving Show Read
  TableEntity json sql=tables
    name Text
    UniqueName name
    deriving Show Read
  GameEntity json sql=games
    tableID TableEntityId
    createdAt UTCTime default=now()
    players [Player]
    minBuyInChips Int
    maxBuyInChips Int
    maxPlayers Int
    board [Card]
    winners Winners
    waitlist [PlayerName]
    deck [Card]
    smallBlind Int
    bigBlind Int
    street Street
    pot Int
    maxBet Bet
    dealer Int
    currentPosToAct Int Maybe
    deriving Show Read
|]

{-
instance FromJSON User where
  parseJSON (Object obj) =
    User <$> obj .: "username" <*> obj .: "email" <*> obj .: "userTotalChips" <*>
    obj .: "userChipsInPlay" <*>
    obj .: "password"
  parseJSON _ = mzero

instance ToJSON User where
  toJSON p =
    object
      [ "username" .= userUsername p
      , "email" .= userEmail p
      , "availableChips" .= userAvailableChips p
      , "chipsInPlay" .= userChipsInPlay p
      , "password" .= userPassword p
      ]

parseUser :: Object -> Parser User
parseUser o = do
  uUsername <- o .: "username"
  uEmail <- o .: "email"
  uTotalChips <- o .: "totalChips"
  uChipsInPlay <- o .: "chipsInPlay"
  uPassword <- o .: "password"
  return
    User
      { userUsername = uUsername
      , userPassword = uPassword
      , userEmail = uEmail
      , userAvailableChips = uTotalChips
      , userChipsInPlay = uChipsInPlay
      }
-}
instance ToSample UserEntity where
  toSamples _ = [("Sample User", g)]
   where
    g = UserEntity { userEntityAvailableChips = 2000
                   , userEntityChipsInPlay    = 0
                   , userEntityUsername       = "Tom"
                   , userEntityEmail          = "gooby@g.com"
                   , userEntityPassword       = "n84!@R5G"
                   , userEntityCreatedAt      = read "" --"2013 - 12 - 15 19 : 12 : 20.841326 UTC"
                   }
