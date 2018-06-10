{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.Betting where

import Control.Lens

------------------------------------------------------------------------------
import Control.Monad.State hiding (state)
import Data.Char (toLower)
import Data.Maybe
import Text.Read (readMaybe)

------------------------------------------------------------------------------
import Poker.Types
import Poker.Utils
import Prelude

postBlind :: Game -> PlayerName -> Blind -> Game
postBlind game@Game {..} pName blind =
  case getGamePlayer game pName of
    Nothing -> game
    Just Player {..} ->
      let betAmount =
            case blind of
              Small -> _smallBlind
              Big -> _bigBlind
          newPlayerState =
            if betAmount == _chips
              then Out AllIn
              else In
          newPlayer =
            Player
              { _playerState = newPlayerState
              , _chips = _chips - betAmount
              , _bet = betAmount
              , _committed = betAmount
              , ..
              }
          newPlayers =
            (\p ->
               if p ^. playerName == _playerName
                 then newPlayer
                 else p) <$>
            _players
       in Game {_players = newPlayers, ..}

-- after a player posts his blind if there are blinds left to post we increment the current player to act position
-- Alternatively if all blinds have been posted we move the game to the next stage (PreFlop)
progressBlindBetting :: Game -> Game
progressBlindBetting game@Game {..} =
  case incBlindPos _currentPosToAct requiredBlinds of
    Nothing -> Game {_street = PreDeal, ..}
    Just nextPosAwaitingBlind ->
      Game {_currentPosToAct = nextPosAwaitingBlind, ..}
  where
    requiredBlinds = getRequiredBlinds game

-- return nothing if there is no next blind position as all blinds have been posted
incBlindPos :: Int -> [Maybe Blind] -> Maybe Int
incBlindPos currentPosToAct [] = Nothing
incBlindPos currentPosToAct requiredBlinds
  | isNothing $ requiredBlinds !! nextBlindPos =
    incBlindPos nextBlindPos requiredBlinds
  | otherwise = Just nextBlindPos
  where
    nextBlindPos = currentPosToAct `modInc` length requiredBlinds

-- if player not in blind position and playerState is none
-- then this means the player is sat out and needs to post big blind
-- otherwise no blind is needed and the incrementing of the position will skip them
getRequiredBlinds :: Game -> [Maybe Blind]
getRequiredBlinds game@Game {..}
  | _street /= PreDeal = []
  | otherwise = blindRequiredByPlayer game <$> getPlayerNames _players
