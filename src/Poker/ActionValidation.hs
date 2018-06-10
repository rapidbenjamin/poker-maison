{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.ActionValidation where

------------------------------------------------------------------------------
import Control.Monad.State.Lazy
import Data.List
import qualified Data.List.Safe as Safe
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Debug.Trace

------------------------------------------------------------------------------
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
isPlayerActingOutOfTurn game playerName = do
  currentPlayerToAct <- gamePlayers Safe.!! _currentPosToAct game
  if currentPlayerToAct == playerName
    then Nothing
    else Just $
         InvalidMove playerName $
         OutOfTurn $ CurrentPlayerToActErr currentPlayerToAct
  where
    gamePlayers = getGamePlayerNames game

checkPlayerSatAtTable :: Game -> PlayerName -> Maybe GameErr
checkPlayerSatAtTable game@Game {..} playerName
  | not atTable = Just $ NotAtTable playerName
  | otherwise = Nothing
  where
    playerNames = getGamePlayerNames game
    atTable = playerName `elem` playerNames

validateBlindAction :: Game -> PlayerName -> Blind -> Maybe GameErr
validateBlindAction game@Game {..} playerName blind =
  case blindRequired of
    Just Small ->
      if blind == Small
        then Nothing
        else Just $ InvalidMove playerName $ BlindRequired Small
    Just Big ->
      if blind == Big
        then Nothing
        else Just $ InvalidMove playerName $ BlindRequired Big
    Nothing -> Just $ InvalidMove playerName $ NoBlindRequired
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
      let playersSatIn = getPlayerNames (_players game)
      playerPosition <- getPlayerPosition playersSatIn playerName
      let smallBlindPos = getSmallBlindPosition playersSatIn (_dealer game)
      let bigBlindPos = smallBlindPos `modInc` length playersSatIn
      if playerPosition == smallBlindPos
        then Just Small
        else if playerPosition == bigBlindPos
               then Just Big
               else Nothing

getSmallBlindPosition :: [Text] -> Int -> Int
getSmallBlindPosition playersSatIn dealerPos =
  if length playersSatIn == 2
    then dealerPos
    else modInc dealerPos (length playersSatIn)
