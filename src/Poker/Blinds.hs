{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.Blinds where

import Control.Lens

------------------------------------------------------------------------------
import Control.Monad.State hiding (state)
import Data.Char (toLower)
import Data.List
import qualified Data.List.Safe as Safe
import Data.Maybe
import Text.Read (readMaybe)

import Poker.ActionValidation

import Poker.Game

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
       in progressBlindBetting
            Game {_players = newPlayers, _pot = _pot + betAmount, ..}

--sets PlayerState To In When No Blind is Required for a given player
progressBlindBetting :: Game -> Game
progressBlindBetting game@Game {..} =
  if haveRequiredBlindsBeenPosted requiredBlinds _players _smallBlind
    then progressToPreFlop game requiredBlinds
    else game
  where
    requiredBlinds = getRequiredBlinds game

haveRequiredBlindsBeenPosted requiredBlinds players smallBlindValue =
  all (== True) $
  zipWith
    (\requiredBlind Player {..} ->
       case requiredBlind of
         Nothing -> True
         Just Big -> _committed == 2 * smallBlindValue
         Just Small -> _committed == smallBlindValue)
    requiredBlinds
    players

getRequiredBlinds :: Game -> [Maybe Blind]
getRequiredBlinds game@Game {..}
  | _street /= PreDeal = []
  | otherwise = blindRequiredByPlayer game <$> getPlayerNames _players
