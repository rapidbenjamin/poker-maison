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

markActed = (actedThisTurn .~ True)

makeBet :: Int -> PlayerName -> Game -> Game
makeBet amount pName game@Game {..} =
  ((players .~ newPlayers) . (currentPosToAct .~ nextPosToAct)) game
  where
    newPlayers =
      (\p@Player {..} ->
         if _playerName == pName
           then let newPlayer = (markActed . placeBet amount) p
                 in if (newPlayer ^. chips) == 0
                      then (playerState .~ Out AllIn) newPlayer
                      else newPlayer
           else p) <$>
      _players
    nextPosToAct = incPosToAct game

foldCards :: PlayerName -> Game -> Game
foldCards pName game@Game {..} =
  ((players .~ newPlayers) . (currentPosToAct .~ nextPosToAct)) game
  where
    newPlayers =
      (\p@Player {..} ->
         if _playerName == pName
           then (markActed . (playerState .~ Out Folded)) p
           else p) <$>
      _players
    nextPosToAct = incPosToAct game

call :: PlayerName -> Game
call = undefined

incPosToAct :: Game -> Int
incPosToAct Game {..} = _currentPosToAct `modInc` numActivePlayers
  where
    numActivePlayers = length $ getActivePlayers _players
