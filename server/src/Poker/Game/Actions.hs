{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.Game.Actions where

import qualified Data.List.Safe                as Safe

import           Control.Lens
import           Control.Monad.State

import           Data.Bool
import           Data.Char                      ( toLower )
import           Data.List
import           Data.Maybe
import           Data.Monoid

import           Poker.ActionValidation
import           Poker.Game.Blinds
import           Poker.Game.Utils
import           Poker.Game.Game
import           Poker.Types

import           Text.Read                      ( readMaybe )

import           Prelude

-- Update table maxBet and pot as well as player state and chip count
placeBet :: Int -> Player -> Player
placeBet value plyr =
  let chips'         = plyr ^. chips
      hasEnoughChips = chips' > value
      betAmount      = bool chips' value hasEnoughChips
  in plyr
        & (chips -~ betAmount)
        . (bet +~ betAmount)
        . (committed +~ betAmount)
        . (playerState .~ In)

markActed :: Player -> Player
markActed = actedThisTurn .~ True

updateMaxBet :: Int -> Game -> Game
updateMaxBet amount = maxBet %~ max amount

markInForHand :: Player -> Player
markInForHand = playerState .~ In

-- Will increment the game's current position to act to the next position
-- where a blind is required. Skipping players that do not have to post blinds
-- during the PreDeal phase of the game is desirable as by definition 
-- the only possible players actions during the PreDeal phase are to either:
--   1. Sit out of the game
--   2. Post a blind.
postBlind :: Blind -> PlayerName -> Game -> Game
postBlind blind pName game@Game {..} =
  game
    & (players .~ newPlayers)
    . (pot +~ blindValue)
    . (currentPosToAct .~ pure nextRequiredBlindPos)
    . (maxBet .~ newMaxBet)
 where
  newPlayers =
    (\p@Player {..} -> if _playerName == pName
        then (markInForHand . markActed . placeBet blindValue) p
        else p
      )
      <$> _players
  isFirstBlind    = sum ((\Player {..} -> _bet) <$> _players) == 0
  gamePlayerNames = (\Player {..} -> _playerName) <$> _players
  blindValue      = if blind == Small then _smallBlind else _bigBlind
  newMaxBet       = if blindValue > _maxBet then blindValue else _maxBet
  positionOfBlindPoster = fromJust $ findIndex ((== pName) . (^. playerName)) _players
  nextRequiredBlindPos = getPosNextBlind positionOfBlindPoster game

makeBet :: Int -> PlayerName -> Game -> Game
makeBet amount pName game@Game {..} =
  updateMaxBet amount game
    & (players %~ markPlayerActed)
    . (currentPosToAct .~ nextPosToAct game)
    . (pot +~ amount)
 where
  updatePlayerState p@Player{..} = 
    if _playerName == pName 
      then (markActed . placeBet amount) p
      else p
  markPlayerActed = (<$>) updatePlayerState

--markPlayerActedAndPlaceBet = 
--  newPlayers =
--    (\p@Player {..} -> if _playerName == pName
--        then (markInForHand . markActed . placeBet blindValue) p
--        else p
--      )
--      <$> _players

foldCards :: PlayerName -> Game -> Game
foldCards pName game@Game {..} =
  game & (players .~ newPlayers) . (currentPosToAct .~ nextPosToAct game)
 where
  newPlayers =
    (\p@Player {..} -> if _playerName == pName
        then (markActed . (playerState .~ Folded)) p
        else p
      )
      <$> _players

call :: PlayerName -> Game -> Game
call pName game@Game {..} =
  game
    & (players .~ newPlayers)
    . (currentPosToAct .~ nextPosToAct game)
    . (pot +~ callAmount)
 where
  player = fromJust $ find (\Player {..} -> _playerName == pName) _players --horrible performance use map for players
  callAmount =
    let maxBetShortfall = _maxBet - (player ^. bet)
        playerChips     = player ^. chips
    in if maxBetShortfall > playerChips then playerChips else maxBetShortfall
  newPlayers =
    (\p@Player {..} ->
        if _playerName == pName then (markActed . placeBet callAmount) p else p
      )
      <$> _players

check :: PlayerName -> Game -> Game
check pName game@Game {..} =
  game & (players .~ newPlayers) . (currentPosToAct .~ nextPosToAct game)
 where
  newPlayers =
    (\p@Player {..} -> if _playerName == pName then markActed p else p)
      <$> _players


-- Sets state of a given player to SatOut (sat-out)
-- In order to sit in again the player must post a blind
sitOut :: PlayerName -> Game -> Game
sitOut plyrName = players %~ (<$>)
  (\p@Player {..} -> if _playerName == plyrName
    then Player { _playerState = SatOut, _actedThisTurn = True, .. }
    else p
  )

sitIn :: PlayerName -> Game -> Game
sitIn plyrName = players %~ (<$>)
  (\p@Player {..} -> if _playerName == plyrName
    then Player { _playerState = In, _actedThisTurn = False, .. }
    else p
  )

seatPlayer :: Player -> Game -> Game
seatPlayer plyr = players <>~ [plyr]

joinWaitlist :: Player -> Game -> Game
joinWaitlist plyr = waitlist %~ (:) (plyr ^. playerName)

leaveSeat :: PlayerName -> Game -> Game
leaveSeat plyrName =
  players %~ filter (\Player {..} -> plyrName /= _playerName)
