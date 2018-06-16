{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.Actions where

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

placeBet :: Int -> Player -> Player
placeBet value = (chips -~ value) . (bet +~ value) . (committed +~ value)

makeBet :: Int -> PlayerName -> Game -> Game
makeBet = undefined

raise :: Int -> PlayerName -> Game -> Game
raise = undefined

fold :: PlayerName -> Game
fold = undefined

call :: PlayerName -> Game
call = undefined
