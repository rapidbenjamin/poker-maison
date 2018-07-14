{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.Blinds where

import Control.Lens

import Control.Monad.State hiding (state)
import Data.Char (toLower)
import Data.List
import qualified Data.List.Safe as Safe
import Data.Maybe
import Data.Text (Text)
import Text.Read (readMaybe)

import Poker.Types
import Poker.Utils
import Prelude

validateBlindAction :: Game -> PlayerName -> Blind -> Maybe GameErr
validateBlindAction game@Game {..} playerName blind
  | _street /= PreDeal =
    Just $ InvalidMove playerName $ CannotPostBlindOutsidePreDeal
  | otherwise =
    case getGamePlayer game playerName of
      Nothing -> Just $ PlayerNotAtTable playerName
      Just p@Player {..} ->
        case blindRequired of
          Just Small ->
            if blind == Small
              then if _committed >= _smallBlind
                     then Just $
                          InvalidMove playerName $ BlindAlreadyPosted Small
                     else Nothing
              else Just $ InvalidMove playerName $ BlindRequired Small
          Just Big ->
            if blind == Big
              then if _committed >= bigBlindValue
                     then Just $ InvalidMove playerName $ BlindAlreadyPosted Big
                     else Nothing
              else Just $ InvalidMove playerName $ BlindRequired Big
          Nothing -> Just $ InvalidMove playerName $ NoBlindRequired
        where blindRequired = blindRequiredByPlayer game playerName
              bigBlindValue = _smallBlind * 2

haveRequiredBlindsBeenPosted :: Game -> Bool
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

getSmallBlindPosition :: [Text] -> Int -> Int
getSmallBlindPosition playersSatIn dealerPos =
  if length playersSatIn == 2
    then dealerPos
    else modInc dealerPos ((length playersSatIn) - 1)

-- if a player does not post their blind at the appropriate time then their state will be changed to 
-- None signifying that they have a seat but are now sat out
-- blind is required either if player is sitting in bigBlind or smallBlind position relative to dealer
-- or if their current playerState is set to Out 
-- If no blind is required for the player to remain In for the next hand then we will return Nothing
blindRequiredByPlayer :: Game -> Text -> Maybe Blind
blindRequiredByPlayer game playerName = do
  if playerPosition == smallBlindPos
    then Just Small
    else if playerPosition == bigBlindPos
           then Just Big
           else Nothing
  where
    player = fromJust $ getGamePlayer game playerName
    playerNames = getPlayerNames (_players game)
    playerPosition = fromJust $ getPlayerPosition playerNames playerName
    smallBlindPos = getSmallBlindPosition playerNames (_dealer game)
    bigBlindPos = smallBlindPos `modInc` (length playerNames - 1)
