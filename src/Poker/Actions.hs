{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.Actions where

import Control.Lens
import Control.Monad.State
import Data.Char (toLower)
import Data.List
import qualified Data.List.Safe as Safe
import Data.Maybe
import Data.Monoid
import Text.Pretty.Simple (pPrint)

import Debug.Trace
import Poker.ActionValidation
import Text.Read (readMaybe)

import Poker.Types
import Poker.Utils
import Data.Bool
import Prelude


-- Update table maxBet and pot as well as player state and chip count
placeBet :: Int -> Player -> Player
placeBet value plyr = let
  chips' = plyr ^. chips 
  hasEnoughChips = chips' > value
  betAmount = bool chips' value hasEnoughChips in
   ((chips -~ betAmount) . (bet +~ betAmount) . (committed +~ betAmount) 
   . (playerState .~ bool (Out AllIn) In hasEnoughChips)) plyr

markAllIn :: Player -> Player
markAllIn = playerState .~ Out AllIn

markActed :: Player -> Player
markActed = actedThisTurn .~ True

updateMaxBet :: Int -> Game -> Game
updateMaxBet amount = maxBet %~ max amount

markInForHand = playerState .~ In

postBlind :: Blind -> PlayerName -> Game -> Game
postBlind blind pName game@Game {..} =
  ((players .~ newPlayers) .
   (pot +~ blindValue) . (currentPosToAct .~ nextPositionToAct))
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

-- should only ever return the position of a player who has playerState equal to In
-- As a player in any other state cannot perform an action
--TODO REMOVE USE OF FROMJUST
incPosToAct :: Game -> Int
incPosToAct Game {..} = nextIx
  where
    iplayers = zip [0 ..] _players
    iplayers' =
      let (a, b) = splitAt _currentPosToAct iplayers
       in b <> a
    (nextIx, nextPlayer) =
      fromJust $ find (\(_, p) -> _playerState p == In) (tail iplayers')
