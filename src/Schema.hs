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
import Data.Time.Clock
import Database.Persist.TH
import Servant.Docs

import Poker.Types

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  User json sql=users
    UniqueEmail email
    UniqueUsername username
    password Text
    createdAt UTCTime default=CURRENT_TIME
    availableChips Int
    chipsInPlay Int
    deriving Show Read
  TableE json sql=tables
    name Text
    deriving Show Read
  GameE json sql=games
    tableID TableEId
    created UTCTime default=CURRENT_TIME
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
    street Text
    pot Int
    maxBet Bet
    dealer Int
    currentPosToAct
    deriving Show Read
|]

instance ToSample User where
  toSamples _ = [("Sample User", g)]
    where
      g =
        User
          { userAvailableChips = 2000
          , userChipsInPlay = 0
          , userUsername = "Tom"
          , userEmail = "gooby@g.com"
          , userPassword = "n84!@R5G"
          , userCreatedAt = read "2013 - 12 - 15 19 : 12 : 20.841326 UTC"
          }
