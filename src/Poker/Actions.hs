{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.Actions where

import Control.Lens

------------------------------------------------------------------------------
import Control.Monad.State hiding (state)
import Data.Char (toLower)
import Data.List
import qualified Data.List.Safe as Safe
import Data.Maybe
import Text.Read (readMaybe)

import Control.Lens
import Poker.ActionValidation

------------------------------------------------------------------------------
import Poker.Types
import Poker.Utils
import Prelude

placeBet :: Int -> Player -> Player
placeBet value =
  (chips -~ value) . (bet +~ value) . (committed +~ value) . (playerState .~ In)

markAllIn :: Player -> Player
markAllIn = (playerState .~ Out AllIn)

markActed = (actedThisTurn .~ True)

updateMaxBet :: Int -> Game -> Game
updateMaxBet amount = maxBet %~ max amount

markInForHand = (playerState .~ In)

postBlind :: Blind -> PlayerName -> Game -> Game
postBlind blind pName game@Game {..} =
  ((players .~ newPlayers) .
   (pot +~ blindValue) .
   (currentPosToAct .~ nextPositionToAct) .
   (maxBet %~
    (\currBet ->
       if blindValue > currBet
         then blindValue
         else currBet)))
    game
  where
    newPlayers =
      (\p@Player {..} ->
         if _playerName == pName
           then let newPlayer =
                      (markInForHand . markActed . placeBet blindValue) p
                 in if (newPlayer ^. chips) == 0
                      then markAllIn newPlayer
                      else newPlayer
           else p) <$>
      _players
    isFirstBlind = (sum $ (\Player {..} -> _bet) <$> _players) == 0
    gamePlayerNames = (\Player {..} -> _playerName) <$> _players
    playerPosition = fromJust $ pName `elemIndex` gamePlayerNames
    haveBetsBeenMade = (sum $ (\Player {..} -> _bet) <$> _players) == 0
    nextPositionToAct =
      if not haveBetsBeenMade
        then playerPosition
        else _currentPosToAct `modInc` (length _players - 1) -- give all players a chance to post blind not just actives
    blindValue =
      if blind == Small
        then _smallBlind
        else _bigBlind

makeBet :: Int -> PlayerName -> Game -> Game
makeBet amount pName game@Game {..} =
  ((players .~ newPlayers) . (currentPosToAct .~ nextPosToAct) . (pot +~ amount))
    (updateMaxBet amount game)
  where
    newPlayers =
      (\p@Player {..} ->
         if _playerName == pName
           then let newPlayer = (markActed . placeBet amount) p
                 in if (newPlayer ^. chips) == 0
                      then markAllIn newPlayer
                      else newPlayer
           else p) <$>
      _players
    nextPosToAct = incPosToAct game

foldCards :: PlayerName -> Game -> Game
foldCards pName game@Game {..} =
  ((players .~ newPlayers) . (currentPosToAct .~ nextPosToAct)) game
  where
    newPlayers =
      (\p@Player {..} ->
         if _playerName == pName
           then (markActed . (playerState .~ Out Folded)) p
           else p) <$>
      _players
    nextPosToAct = incPosToAct game

call :: PlayerName -> Game -> Game
call pName game@Game {..} =
  ((players .~ newPlayers) .
   (currentPosToAct .~ nextPosToAct) . (pot +~ callAmount))
    game
  where
    player = fromJust $ find (\Player {..} -> _playerName == pName) _players --horrible performance use map for players
    callAmount =
      let maxBetShortfall = _maxBet - (player ^. bet)
          playerChips = (player ^. chips)
       in if maxBetShortfall > playerChips
            then playerChips
            else maxBetShortfall
    newPlayers =
      (\p@Player {..} ->
         if _playerName == pName
           then let newPlayer = (markActed . placeBet callAmount) p
                 in if (newPlayer ^. chips) == 0
                      then markAllIn newPlayer
                      else newPlayer
           else p) <$>
      _players
    nextPosToAct = incPosToAct game

check :: PlayerName -> Game -> Game
check pName game@Game {..} =
  ((players .~ newPlayers) . (currentPosToAct .~ nextPosToAct)) game
  where
    newPlayers =
      (\p@Player {..} ->
         if _playerName == pName
           then markActed p
           else p) <$>
      _players
    nextPosToAct = incPosToAct game

incPosToAct :: Game -> Int
incPosToAct Game {..} = _currentPosToAct `modInc` numActivePlayers
  where
    numActivePlayers = (length $ getActivePlayers _players) - 1
