{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Poker.ActionValidation where

import Control.Lens hiding (Fold)

import Control.Monad.State.Lazy
import Data.List
import qualified Data.List.Safe as Safe
import Data.Maybe
import Data.Monoid
import Data.Text (Text)

import qualified Data.Text as T
import Debug.Trace

import Poker.Blinds (validateBlindAction)
import Poker.Game (getWinners)
import Poker.Hands
import Poker.Types
import Poker.Utils

-- a Nothing signifies the absence of an error in which case the action is valid
-- TODO SHOULD BE Either GameErr () not Maybe GameErr as maybe monad is in wrong direction
validateAction :: Game -> PlayerName -> PlayerAction -> Either GameErr ()
validateAction game@Game {..} playerName =
  \case
    PostBlind blind ->
      checkPlayerSatAtTable game playerName >>
      validateBlindAction game playerName blind
    Check -> isPlayerActingOutOfTurn game playerName >> canCheck playerName game
    Fold -> isPlayerActingOutOfTurn game playerName >> canFold playerName game
    Bet amount ->
      isPlayerActingOutOfTurn game playerName >> canBet playerName amount game
    Raise amount ->
      isPlayerActingOutOfTurn game playerName >> canRaise playerName amount game
    Call -> isPlayerActingOutOfTurn game playerName >> canCall playerName game
    Timeout ->
      if _street == Showdown
        then Left $ InvalidMove playerName InvalidActionForStreet
        else isPlayerActingOutOfTurn game playerName
    ShowHand -> validateShowOrMuckHand game playerName ShowHand
    MuckHand -> validateShowOrMuckHand game playerName MuckHand

-- | The first player to post their blinds in the predeal stage  can do it from any position
-- Therefore the acting in turn rule wont apply for that first move.
isPlayerActingOutOfTurn :: Game -> PlayerName -> Either GameErr ()
isPlayerActingOutOfTurn game@Game {..} playerName =
  if _street == PreDeal && not haveBetsBeenMade -- first predeal blind bet exempt
    then Right ()
    else do
      let playerPosition = playerName `elemIndex` gamePlayerNames
      case playerPosition of
        Nothing -> Left $ NotAtTable playerName
        Just pos ->
          if _currentPosToAct == pos
            then Right ()
            else Left $
                 InvalidMove playerName $
                 OutOfTurn $
                 CurrentPlayerToActErr $ gamePlayerNames !! _currentPosToAct
  where
    haveBetsBeenMade = (sum $ (\Player {..} -> _bet) <$> _players) == 0
    gamePlayerNames = (\Player {..} -> _playerName) <$> _players

checkPlayerSatAtTable :: Game -> PlayerName -> Either GameErr ()
checkPlayerSatAtTable game@Game {..} pName
  | not atTable = Left $ NotAtTable pName
  | otherwise = Right ()
  where
    playerNames = getGamePlayerNames game
    atTable = pName `elem` playerNames

canBet :: PlayerName -> Int -> Game -> Either GameErr ()
canBet pName amount game@Game {..}
  | amount < _bigBlind = Left $ (InvalidMove pName) BetLessThanBigBlind
  | _maxBet > 0 = Left $ (InvalidMove pName) CannotBetShouldRaiseInstead
  | amount > chipCount = Left $ (InvalidMove pName) NotEnoughChipsForAction
  | (_street == Showdown) || (_street == PreDeal) =
    Left $ (InvalidMove pName) InvalidActionForStreet
  | otherwise = Right ()
  where
    chipCount = _chips $ fromJust (getGamePlayer game pName)

-- Keep in mind that a player can always raise all in,
-- even if their total chip count is less than what 
-- a min-bet or min-raise would be. 
canRaise :: PlayerName -> Int -> Game -> Either GameErr ()
canRaise pName amount game@Game {..}
  | (_street == Showdown) || (_street == PreDeal) =
    Left $ (InvalidMove pName) InvalidActionForStreet
  | _maxBet == 0 = Left $ (InvalidMove pName) CannotRaiseShouldBetInstead
  | (amount < minRaise) && (amount /= chipCount) =
    Left $ (InvalidMove pName) $ RaiseAmountBelowMinRaise minRaise
  | amount > chipCount = Left $ (InvalidMove pName) NotEnoughChipsForAction
  | otherwise = Right ()
  where
    minRaise = 2 * _maxBet
    chipCount = _chips $ fromJust (getGamePlayer game pName)

canCheck :: PlayerName -> Game -> Either GameErr ()
canCheck pName game@Game {..}
  | _street == PreFlop && _committed < _bigBlind =
    Left $ (InvalidMove pName) CannotCheckShouldCallRaiseOrFold
  | (_street == Showdown) || (_street == PreDeal) =
    Left $ (InvalidMove pName) InvalidActionForStreet
  | _maxBet /= 0 = Left $ (InvalidMove pName) CannotCheckShouldCallRaiseOrFold
  | otherwise = Right ()
  where
    Player {..} =
      fromJust $ find (\Player {..} -> _playerName == pName) _players

canFold :: PlayerName -> Game -> Either GameErr ()
canFold pName game@Game {..}
  | (_street == Showdown) || (_street == PreDeal) =
    Left $ (InvalidMove pName) InvalidActionForStreet
  | otherwise = Right ()

canCall :: PlayerName -> Game -> Either GameErr ()
canCall pName game@Game {..}
  | (_street == Showdown) || (_street == PreDeal) =
    Left $ (InvalidMove pName) InvalidActionForStreet
  | _maxBet == 0 && _street /= PreFlop =
    Left $ (InvalidMove pName) CannotCallZeroAmountCheckOrBetInstead
  | otherwise = Right ()
  where
    minRaise = 2 * _maxBet
    p = fromJust (getGamePlayer game pName)
    chipCount = _chips p
    amountNeededToCall = _maxBet - _bet p

validateShowOrMuckHand ::
     Game -> PlayerName -> PlayerAction -> Either GameErr ()
validateShowOrMuckHand game@Game {..} pName action =
  checkPlayerSatAtTable game pName

-- Should Tell us if everyone has folded to the given playerName 
-- and the hand is over
canShowOrMuckHand :: PlayerName -> Game -> Either GameErr ()
canShowOrMuckHand pName game@Game {..}
  | _street /= Showdown = Left $ (InvalidMove pName) InvalidActionForStreet
  | otherwise =
    case _winners of
      SinglePlayerShowdown winningPlayerName ->
        if winningPlayerName == pName
          then Right ()
          else Left $
               (InvalidMove pName) $
               CannotShowHandOrMuckHand "Not winner of hand"
      MultiPlayerShowdown _ ->
        Left $
        (InvalidMove pName) $
        CannotShowHandOrMuckHand
          "Can only show or muck cards if winner of single player pot during showdown"
