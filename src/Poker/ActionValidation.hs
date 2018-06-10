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
