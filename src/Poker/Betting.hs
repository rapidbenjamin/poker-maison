{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.Betting where

import Control.Lens

------------------------------------------------------------------------------
import Control.Monad.State hiding (state)
import Data.Char (toLower)
import Data.List
import qualified Data.List.Safe as Safe
import Data.Maybe
import Text.Read (readMaybe)

import Control.Lens
import Poker.ActionValidation

------------------------------------------------------------------------------
import Poker.Types
import Poker.Utils
import Prelude

-- | Betting has concluded is there is only one or less active players remaining
-- or if the all active player have posted a bet equal to the max bet or dont have 
-- an In state
hasBettingFinished :: Game -> Bool
hasBettingFinished Game {..} =
  length activePlayers <= 1 || not awaitingPlayerAction
  where
    activePlayers = getActivePlayers _players
    maxBet = maximum $ flip (^.) bet <$> activePlayers
    awaitingPlayerAction =
      any (\Player {..} -> _playerState == In || _bet /= maxBet) activePlayers
