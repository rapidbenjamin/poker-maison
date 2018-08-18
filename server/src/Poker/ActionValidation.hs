--  TODO - should factor out the hasEnoughChips check for each action and then just sequence it 
--  inside the parent validateAction function with >>
--
-- Second TODo - remove use of fromJust
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Poker.ActionValidation where

import qualified Data.List.Safe as Safe
import qualified Data.Text as T

import Control.Lens hiding (Fold)
import Control.Monad.State.Lazy

import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (Text)

import Poker.Game.Blinds
import Poker.Game.Game (getWinners, isPlayerToAct)
import Poker.Game.Hands
import Poker.Game.Utils
import Poker.Types

validateAction :: Game -> PlayerName -> PlayerAction -> Either GameErr ()
validateAction game@Game {..} playerName =
  \case
    PostBlind blind ->
      checkPlayerSatAtTable game playerName >>
      canPostBlind game playerName blind >>
      validateBlindAction game playerName blind
    Check -> isPlayerActingOutOfTurn game playerName >> canCheck playerName game
    Fold -> isPlayerActingOutOfTurn game playerName >> canFold playerName game
    Bet amount ->
      isPlayerActingOutOfTurn game playerName >> canBet playerName amount game
    Raise amount ->
      isPlayerActingOutOfTurn game playerName >> canRaise playerName amount game
    Call -> isPlayerActingOutOfTurn game playerName >> canCall playerName game
    Timeout -> canTimeout playerName game
    LeaveSeat' -> canLeaveSeat playerName game
    SitDown plyr -> canSit plyr game
    ShowHand -> validateShowOrMuckHand game playerName ShowHand
    MuckHand -> validateShowOrMuckHand game playerName MuckHand

canPostBlind :: Game -> PlayerName -> Blind -> Either GameErr ()
canPostBlind game@Game {..} pName blind
  | length _players < 2 =
    Left $
    InvalidMove pName $
    CannotPostBlind
      "Cannot post blind unless at least two players are sat at table"
  | otherwise =
    case blind of
      Big ->
        if chipCount < _bigBlind
          then notEnoughChipsErr
          else Right ()
      Small ->
        if chipCount < _smallBlind
          then notEnoughChipsErr
          else Right ()
      NoBlind -> Left $ InvalidMove pName CannotPostNoBlind
  where
    chipCount = _chips $ fromJust $ getGamePlayer game pName
    notEnoughChipsErr = Left $ InvalidMove pName NotEnoughChipsForAction

-- | The first player to post their blinds in the predeal stage  can do it from any position
-- Therefore the acting in turn rule wont apply for that first move.
isPlayerActingOutOfTurn :: Game -> PlayerName -> Either GameErr ()
isPlayerActingOutOfTurn game@Game {..} playerName
  | _street == PreDeal && not haveBetsBeenMade = Right () -- first predeal blind bet can be done from any position
  | otherwise =
    case playerName `elemIndex` gamePlayerNames of
      Nothing -> Left $ NotAtTable playerName
      Just pos ->
        if isPlayerToAct playerName game
          then Right ()
          else Left $
               InvalidMove playerName $
               OutOfTurn $
               CurrentPlayerToActErr $ gamePlayerNames !! _currentPosToAct
  where
    haveBetsBeenMade = sum ((\Player {..} -> _bet) <$> _players) == 0
    gamePlayerNames = getGamePlayerNames game

checkPlayerSatAtTable :: Game -> PlayerName -> Either GameErr ()
checkPlayerSatAtTable game@Game {..} pName
  | not atTable = Left $ NotAtTable pName
  | otherwise = Right ()
  where
    playerNames = getGamePlayerNames game
    atTable = pName `elem` playerNames

canTimeout :: PlayerName -> Game -> Either GameErr ()
canTimeout playerName game@Game {..}
  | _street == Showdown = Left $ InvalidMove playerName InvalidActionForStreet
  | otherwise = isPlayerActingOutOfTurn game playerName

canBet :: PlayerName -> Int -> Game -> Either GameErr ()
canBet pName amount game@Game {..}
  | amount < _bigBlind = Left $ InvalidMove pName BetLessThanBigBlind
  | amount > chipCount = Left $ InvalidMove pName NotEnoughChipsForAction
  | _street == Showdown || _street == PreDeal =
    Left $ InvalidMove pName InvalidActionForStreet
  | _maxBet > 0 =
    Left $
    InvalidMove pName $
    CannotBetShouldRaiseInstead
      "A bet can only be carried out if no preceding player has bet"
  | otherwise = Right ()
  where
    chipCount = _chips $ fromJust $ getGamePlayer game pName

-- Keep in mind that a player can always raise all in,
-- even if their total chip count is less than what 
-- a min-bet or min-raise would be. 
canRaise :: PlayerName -> Int -> Game -> Either GameErr ()
canRaise pName amount game@Game {..}
  | _street == Showdown || _street == PreDeal =
    Left $ InvalidMove pName InvalidActionForStreet
  | _maxBet == 0 = Left $ InvalidMove pName CannotRaiseShouldBetInstead
  | amount < minRaise && amount /= chipCount =
    Left $ InvalidMove pName $ RaiseAmountBelowMinRaise minRaise
  | amount > chipCount = Left $ InvalidMove pName NotEnoughChipsForAction
  | otherwise = Right ()
  where
    minRaise = 2 * _maxBet
    chipCount = _chips $ fromJust $ getGamePlayer game pName

canCheck :: PlayerName -> Game -> Either GameErr ()
canCheck pName Game {..}
  | _street == PreFlop && _committed < _bigBlind =
    Left $ InvalidMove pName CannotCheckShouldCallRaiseOrFold
  | _street == Showdown || _street == PreDeal =
    Left $ InvalidMove pName InvalidActionForStreet
  | _committed < _maxBet =
    Left $ InvalidMove pName CannotCheckShouldCallRaiseOrFold
  | otherwise = Right ()
  where
    Player {..} =
      fromJust $ find (\Player {..} -> _playerName == pName) _players

canFold :: PlayerName -> Game -> Either GameErr ()
canFold pName Game {..}
  | _street == Showdown || _street == PreDeal =
    Left $ InvalidMove pName InvalidActionForStreet
  | otherwise = Right ()

canCall :: PlayerName -> Game -> Either GameErr ()
canCall pName game@Game {..}
  | _street == Showdown || _street == PreDeal =
    Left $ InvalidMove pName InvalidActionForStreet
  | _maxBet == 0 && _street /= PreFlop =
    Left $ InvalidMove pName CannotCallZeroAmountCheckOrBetInstead
  | otherwise = Right ()
  where
    minRaise = 2 * _maxBet
    p = fromJust (getGamePlayer game pName)
    chipCount = _chips p
    amountNeededToCall = _maxBet - _bet p

canSit :: Player -> Game -> Either GameErr ()
canSit player@Player {..} game@Game {..}
  | _street /= PreDeal =
    Left $ InvalidMove _playerName CannotSitDownOutsidePreDeal
  | _playerName `elem` getPlayerNames _players =
    Left $ AlreadySatAtTable _playerName
  | _chips < _minBuyInChips = Left $ NotEnoughChips _playerName
  | _chips > _maxBuyInChips = Left $ OverMaxChipsBuyIn _playerName
  | length _players < _maxPlayers = Right ()
  | otherwise = Left $ CannotSitAtFullTable _playerName

canLeaveSeat :: PlayerName -> Game -> Either GameErr ()
canLeaveSeat playerName game@Game {..}
  | _street /= PreDeal =
    Left $ InvalidMove playerName CannotLeaveSeatOutsidePreDeal
  | playerName `notElem` getPlayerNames _players = Left $ NotAtTable playerName
  | otherwise = Right ()

canJoinWaitList :: Player -> Game -> Either GameErr ()
canJoinWaitList player@Player {..} game@Game {..}
  | _playerName `elem` _waitlist = Left $ AlreadyOnWaitlist _playerName
  | otherwise = Right ()

validateBlindAction :: Game -> PlayerName -> Blind -> Either GameErr ()
validateBlindAction game@Game {..} playerName blind
  | _street /= PreDeal =
    Left $ InvalidMove playerName CannotPostBlindOutsidePreDeal
  | otherwise =
    case getGamePlayer game playerName of
      Nothing -> Left $ PlayerNotAtTable playerName
      Just p@Player {..} ->
        case blindRequired of
          Small ->
            if blind == Small
              then if _committed >= _smallBlind
                     then Left $
                          InvalidMove playerName $ BlindAlreadyPosted Small
                     else Right ()
              else Left $ InvalidMove playerName $ BlindRequired Small
          Big ->
            if blind == Big
              then if _committed >= bigBlindValue
                     then Left $ InvalidMove playerName $ BlindAlreadyPosted Big
                     else Right ()
              else Left $ InvalidMove playerName $ BlindRequired Big
          NoBlind -> Left $ InvalidMove playerName NoBlindRequired
        where blindRequired = blindRequiredByPlayer game playerName
              bigBlindValue = _smallBlind * 2

validateShowOrMuckHand ::
     Game -> PlayerName -> PlayerAction -> Either GameErr ()
validateShowOrMuckHand game@Game {..} pName action =
  checkPlayerSatAtTable game pName

-- Should Tell us if everyone has folded to the given playerName 
-- and the hand is over
canShowOrMuckHand :: PlayerName -> Game -> Either GameErr ()
canShowOrMuckHand pName game@Game {..}
  | _street /= Showdown = Left $ InvalidMove pName InvalidActionForStreet
  | otherwise =
    case _winners of
      SinglePlayerShowdown winningPlayerName ->
        if winningPlayerName == pName
          then Right ()
          else Left $
               InvalidMove pName $ CannotShowHandOrMuckHand "Not winner of hand"
      MultiPlayerShowdown _ ->
        Left $
        InvalidMove pName $
        CannotShowHandOrMuckHand
          "Can only show or muck cards if winner of single player pot during showdown"
