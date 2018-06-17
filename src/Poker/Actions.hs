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

markAllIn :: Player -> Player
markAllIn = (playerState .~ Out AllIn)

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
                      then markAllIn newPlayer
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

call :: PlayerName -> Game -> Game
call pName game@Game {..} =
  ((players .~ newPlayers) . (currentPosToAct .~ nextPosToAct)) game
  where
    maxBet = getMaxBet _players
    newPlayers =
      (\p@Player {..} ->
         if _playerName == pName
           then let maxBetShortfall = maxBet - _committed
                    callAmount =
                      if _chips > maxBetShortfall
                        then maxBetShortfall
                        else _chips
                    newPlayer = (markActed . placeBet callAmount) p
                 in if (newPlayer ^. chips) == 0
                      then markAllIn newPlayer
                      else newPlayer
           else p) <$>
      _players
    nextPosToAct = incPosToAct game

incPosToAct :: Game -> Int
incPosToAct Game {..} = _currentPosToAct `modInc` numActivePlayers
  where
    numActivePlayers = length $ getActivePlayers _players
