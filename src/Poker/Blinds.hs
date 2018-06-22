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

------------------------------------------------------------------------------
import Poker.Types
import Poker.Utils
import Prelude

haveRequiredBlindsBeenPosted game@Game {..} =
  all (== True) $
  zipWith
    (\requiredBlind Player {..} ->
       case requiredBlind of
         Nothing -> True
         Just Big -> _committed == _bigBlind
         Just Small -> _committed == _smallBlind)
    requiredBlinds
    _players
  where
    requiredBlinds = getRequiredBlinds game

getRequiredBlinds :: Game -> [Maybe Blind]
getRequiredBlinds game@Game {..}
  | _street /= PreDeal = []
  | otherwise = blindRequiredByPlayer game <$> getPlayerNames _players

-- We use the list of required blinds to calculate if a player has posted 
-- chips sufficient to be "In" for this hand.
activatePlayersWhenNoBlindNeeded :: [Player] -> [Maybe Blind] -> [Player]
activatePlayersWhenNoBlindNeeded plyrs requiredBlinds =
  zipWith updatePlayer requiredBlinds plyrs
  where
    updatePlayer blindReq Player {..} =
      Player
        { _playerState =
            if isNothing blindReq
              then In
              else _playerState
        , _bet = 0
        , ..
        }

-- Sets player state to in if they don't need to
updatePlayersInHand :: Game -> Game
updatePlayersInHand game =
  (players %~ flip activatePlayersWhenNoBlindNeeded requiredBlinds) game
  where
    requiredBlinds = getRequiredBlinds game
