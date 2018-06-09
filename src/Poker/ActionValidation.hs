{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.ActionValidation where

------------------------------------------------------------------------------
import Control.Monad.State.Lazy
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (Text)

------------------------------------------------------------------------------
import Poker.Betting
import Poker.Game
import Poker.Hands
import Poker.Types
import Poker.Utils

-- a Nothing signifies the absence of an error in which case the action is valid
validateAction :: Game -> PlayerName -> PlayerAction -> Maybe GameErr
validateAction game@Game {..} playerName action@(PostBlind blind) =
  case checkPlayerSatAtTable game playerName of
    err@(Just _) -> err
    Nothing ->
      case isPlayerActingOutOfTurn game playerName of
        err@(Just _) -> err
        Nothing ->
          case validateBlindAction game playerName blind of
            err@(Just _) -> err
            Nothing -> Nothing

isPlayerActingOutOfTurn :: Game -> PlayerName -> Maybe GameErr
isPlayerActingOutOfTurn game playerName =
  if currentPlayerToAct == playerName
    then Nothing
    else Just $
         InvalidMove playerName $
         OutOfTurn $ CurrentPlayerToActErr currentPlayerToAct
  where
    currentPlayerToAct = (getGamePlayerNames game) !! _currentPosToAct game

checkPlayerSatAtTable :: Game -> PlayerName -> Maybe GameErr
checkPlayerSatAtTable game@Game {..} playerName
  | not atTable = Just $ NotAtTable playerName
  | otherwise = Nothing
  where
    playerNames = getGamePlayerNames game
    atTable = playerName `elem` playerNames

validateBlindAction :: Game -> PlayerName -> Blind -> Maybe GameErr
validateBlindAction game@Game {..} playerName blind =
  case fromJust blindRequired of
    Small ->
      if blind == Small
        then Nothing
        else Just $ InvalidMove playerName $ BlindRequired Small
    Big ->
      if blind == Big
        then Nothing
        else Just $ InvalidMove playerName $ BlindRequired Big
  where
    blindRequired = blindRequiredByPlayer game playerName

-- if a player does not post their blind at the appropriate time then their state will be changed to 
--None signifying that they have a seat but are now sat out
-- blind is required either if player is sitting in bigBlind or smallBlind position relative to dealer
-- or if their current playerState is set to Out 
-- If no blind is required for the player to remain In for the next hand then we will return Nothing
blindRequiredByPlayer :: Game -> Text -> Maybe Blind
blindRequiredByPlayer game playerName = do
  player <- getGamePlayer game playerName
  case _playerState player of
    None -> Just Big
    _ -> do
      playerPosition <- getPlayerPosition (getGamePlayerNames game) playerName
      let playersSatIn = getPlayerNames $ getPlayersSatIn (_players game)
      let smallBlindPos = getSmallBlindPosition playersSatIn (_dealer game)
      let bigBlindPos = smallBlindPos `modInc` length playersSatIn
      if playerPosition == smallBlindPos
        then Just Small
        else if playerPosition == bigBlindPos
               then Just Big
               else Nothing
