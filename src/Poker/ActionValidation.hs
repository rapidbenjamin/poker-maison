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
validateAction :: Game -> PlayerName -> PlayerAction -> Maybe GameErr
validateAction game@Game {..} playerName =
  \case
    PostBlind blind ->
      case checkPlayerSatAtTable game playerName of
        err@(Just _) -> err
        Nothing ->
          case validateBlindAction game playerName blind of
            err@(Just _) -> err
            Nothing -> Nothing
    Check ->
      case isPlayerActingOutOfTurn game playerName of
        err@(Just _) -> err
        Nothing -> do
          err <- canCheck playerName game
          return $ InvalidMove playerName err
    Fold ->
      case isPlayerActingOutOfTurn game playerName of
        err@(Just _) -> err
        Nothing -> do
          err <- canFold playerName game
          return $ InvalidMove playerName err
    Bet amount ->
      case isPlayerActingOutOfTurn game playerName of
        err@(Just _) -> err
        Nothing -> do
          err <- canBet playerName amount game
          return $ InvalidMove playerName err
    Raise amount ->
      case isPlayerActingOutOfTurn game playerName of
        err@(Just _) -> err
        Nothing -> do
          err <- canRaise playerName amount game
          return $ InvalidMove playerName err
    Call ->
      case isPlayerActingOutOfTurn game playerName of
        err@(Just _) -> err
        Nothing -> do
          err <- canCall playerName game
          return $ InvalidMove playerName err
    Timeout ->
      if _street == Showdown
        then Just $ InvalidMove playerName InvalidActionForStreet
        else case isPlayerActingOutOfTurn game playerName of
               err@(Just _) -> err
               Nothing -> Nothing
    ShowHand -> validateShowOrMuckHand game playerName ShowHand
    MuckHand -> validateShowOrMuckHand game playerName MuckHand

-- | The first player to post their blinds in the predeal stage  can do it from any position
-- Therefore the acting in turn rule wont apply for that first move.
isPlayerActingOutOfTurn :: Game -> PlayerName -> Maybe GameErr
isPlayerActingOutOfTurn game@Game {..} playerName =
  if _street == PreDeal && not haveBetsBeenMade -- first predeal blind bet exempt
    then Nothing
    else do
      let playerPosition = playerName `elemIndex` gamePlayerNames
      case playerPosition of
        Nothing -> Just $ NotAtTable playerName
        Just pos ->
          if _currentPosToAct == pos
            then Nothing
            else Just $
                 InvalidMove playerName $
                 OutOfTurn $
                 CurrentPlayerToActErr $ gamePlayerNames !! _currentPosToAct
  where
    haveBetsBeenMade = (sum $ (\Player {..} -> _bet) <$> _players) == 0
    gamePlayerNames = (\Player {..} -> _playerName) <$> _players

checkPlayerSatAtTable :: Game -> PlayerName -> Maybe GameErr
checkPlayerSatAtTable game@Game {..} playerName
  | not atTable = Just $ NotAtTable playerName
  | otherwise = Nothing
  where
    playerNames = getGamePlayerNames game
    atTable = playerName `elem` playerNames

canBet :: PlayerName -> Int -> Game -> Maybe InvalidMoveErr
canBet pName amount game@Game {..} =
  if amount < _bigBlind
    then Just BetLessThanBigBlind
    else if _maxBet > 0
           then Just CannotBetShouldRaiseInstead
           else if amount > chipCount
                  then Just NotEnoughChipsForAction
                  else if (_street == Showdown) || (_street == PreDeal)
                         then Just InvalidActionForStreet
                         else Nothing
  where
    chipCount = _chips $ fromJust (getGamePlayer game pName)

-- Keep in mind that a player can always raise all in,
-- even if their total chip count is less than what 
-- a min-bet or min-raise would be. 
canRaise :: PlayerName -> Int -> Game -> Maybe InvalidMoveErr
canRaise pName amount game@Game {..} =
  if (_street == Showdown) || (_street == PreDeal)
    then Just InvalidActionForStreet
    else if _maxBet == 0
           then Just CannotRaiseShouldBetInstead
           else if (amount < minRaise) && (amount /= chipCount)
                  then Just $ RaiseAmountBelowMinRaise minRaise
                  else if amount > chipCount
                         then Just NotEnoughChipsForAction
                         else Nothing
  where
    minRaise = 2 * _maxBet
    chipCount = _chips $ fromJust (getGamePlayer game pName)

canCheck :: PlayerName -> Game -> Maybe InvalidMoveErr
canCheck pName game@Game {..} =
  if (_street == PreFlop && _committed < _bigBlind)
    then Just CannotCheckShouldCallRaiseOrFold
    else if (_street == Showdown) || (_street == PreDeal)
           then Just InvalidActionForStreet
           else if _maxBet /= 0
                  then Just CannotCheckShouldCallRaiseOrFold
                  else Nothing
  where
    Player {..} =
      fromJust $ find (\Player {..} -> _playerName == pName) _players

canFold :: PlayerName -> Game -> Maybe InvalidMoveErr
canFold pName game@Game {..} =
  if (_street == Showdown) || (_street == PreDeal)
    then Just InvalidActionForStreet
    else Nothing

canCall :: PlayerName -> Game -> Maybe InvalidMoveErr
canCall pName game@Game {..} =
  if (_street == Showdown) || (_street == PreDeal)
    then Just InvalidActionForStreet
    else if (_maxBet == 0 && _street /= PreFlop)
           then Just CannotCallZeroAmountCheckOrBetInstead
           else Nothing
  where
    minRaise = 2 * _maxBet
    p = fromJust (getGamePlayer game pName)
    chipCount = _chips p
    amountNeededToCall = _maxBet - (_bet p)

validateShowOrMuckHand :: Game -> PlayerName -> PlayerAction -> Maybe GameErr
validateShowOrMuckHand game@Game {..} pName action =
  case checkPlayerSatAtTable game pName of
    err@(Just _) -> err
    Nothing -> do
      err <- canShowOrMuckHand pName game
      return $ InvalidMove pName err

-- Should Tell us if everyone has folded to the given playerName 
-- and the hand is over
canShowOrMuckHand :: PlayerName -> Game -> Maybe InvalidMoveErr
canShowOrMuckHand pName game@Game {..} =
  if (_street /= Showdown)
    then Just InvalidActionForStreet
    else case _winners of
           SinglePlayerShowdown winningPlayerName ->
             if winningPlayerName == pName
               then Nothing
               else Just $ CannotShowHandOrMuckHand "Not winner of hand"
           MultiPlayerShowdown _ ->
             Just $
             CannotShowHandOrMuckHand
               "Can only show or muck cards if winner of single player pot during showdown"
