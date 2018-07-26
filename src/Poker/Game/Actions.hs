{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.Game.Actions where

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

import Data.Bool
import Poker.Game.Utils
import Poker.Types
import Prelude

-- Update table maxBet and pot as well as player state and chip count
placeBet :: Int -> Player -> Player
placeBet value plyr =
  let chips' = plyr ^. chips
      hasEnoughChips = chips' > value
      betAmount = bool chips' value hasEnoughChips
   in ((chips -~ betAmount) .
       (bet +~ betAmount) . (committed +~ betAmount) . (playerState .~ In))
        plyr

markActed :: Player -> Player
markActed = actedThisTurn .~ True

updateMaxBet :: Int -> Game -> Game
updateMaxBet amount = maxBet %~ max amount

markInForHand = playerState .~ In

postBlind :: Blind -> PlayerName -> Game -> Game
postBlind blind pName game@Game {..} =
  game &
  ((players .~ newPlayers) .
   (pot +~ blindValue) . (currentPosToAct .~ nextPositionToAct))
  where
    newPlayers =
      (\p@Player {..} ->
         if _playerName == pName
           then (markInForHand . markActed . placeBet blindValue) p
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
  updateMaxBet amount game &
  ((players .~ newPlayers) . (currentPosToAct .~ nextPosToAct) . (pot +~ amount))
  where
    newPlayers =
      (\p@Player {..} ->
         if _playerName == pName
           then (markActed . placeBet amount) p
           else p) <$>
      _players
    nextPosToAct = incPosToAct game

foldCards :: PlayerName -> Game -> Game
foldCards pName game@Game {..} =
  game & ((players .~ newPlayers) . (currentPosToAct .~ nextPosToAct))
  where
    newPlayers =
      (\p@Player {..} ->
         if _playerName == pName
           then (markActed . (playerState .~ Folded)) p
           else p) <$>
      _players
    nextPosToAct = incPosToAct game

call :: PlayerName -> Game -> Game
call pName game@Game {..} =
  game &
  ((players .~ newPlayers) .
   (currentPosToAct .~ nextPosToAct) . (pot +~ callAmount))
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
           then (markActed . placeBet callAmount) p
           else p) <$>
      _players
    nextPosToAct = incPosToAct game

check :: PlayerName -> Game -> Game
check pName game@Game {..} =
  game & ((players .~ newPlayers) . (currentPosToAct .~ nextPosToAct))
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
