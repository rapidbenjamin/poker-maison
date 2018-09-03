{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Poker.Game.Game where

import Control.Arrow
import Control.Lens
import Control.Monad.Random.Class
import Control.Monad.State

import Data.List
import qualified Data.List.Safe as Safe
import Data.List.Split
import Data.Maybe
import Data.Monoid
import Data.Text (Text)

import Poker.Game.Blinds
import Poker.Game.Hands
import Poker.Game.Utils
import Poker.Types

-- | Returns both the dealt players and remaining cards left in deck.
-- We need to have the remaining cards in the deck for dealing
-- board cards over the next stages.
dealToPlayers :: [Card] -> [Player] -> ([Card], [Player])
dealToPlayers =
  mapAccumL
    (\cards player ->
       if player ^. playerState == In
         then (drop 2 cards, (pockets .~ take 2 cards) player)
         else (cards, player))

dealBoardCards :: Int -> Game -> Game
dealBoardCards n game@Game {..} =
  Game {_board = _board <> boardCards, _deck = newDeck, ..}
  where
    (boardCards, newDeck) = splitAt n _deck

deal :: Game -> Game
deal game@Game {..} = Game {_players = dealtPlayers, _deck = remainingDeck, ..}
  where
    (remainingDeck, dealtPlayers) = dealToPlayers _deck _players

getNextStreet :: Street -> Street
getNextStreet Showdown = minBound
getNextStreet _street = succ _street

-- Unless in the scenario where everyone is all in 
-- if no further player actions are possible (i.e betting has finished)
-- then actedThisTurn should be set to True for all active players in Hand.
-- This scenario occurs when all players or all but one players are all in. 
resetPlayers :: Game -> Game
resetPlayers game@Game {..} = (players .~ newPlayers) game
  where
    newPlayers = (bet .~ 0) . (actedThisTurn .~ False) <$> _players

-- When there are only two players in game (Heads Up) then the first player
-- to act PreFlop is the player at the dealer position.
-- First player to act is the player sitting to the right of the player
-- in the big blind position
progressToPreFlop :: Game -> Game
progressToPreFlop game@Game {..} =
  game &
  (street .~ PreFlop) .
  (currentPosToAct .~ firstPosToAct) .
  (players %~ (<$>) (actedThisTurn .~ False)) . deal . updatePlayersInHand
  where
    playerCount = length $ getActivePlayers _players
    incAmount = 2
    firstPosToAct =
      if playerCount == 3
        then _dealer
        else modInc incAmount _dealer (playerCount - 1)

progressToFlop :: Game -> Game
progressToFlop game
  | isEveryoneAllIn game = game & (street .~ Flop) . dealBoardCards 3
  | otherwise =
    game &
    (street .~ Flop) .
    (currentPosToAct .~ incPosToAct (_dealer game) game) .
    (maxBet .~ 0) . dealBoardCards 3 . resetPlayers

progressToTurn :: Game -> Game
progressToTurn game
  | isEveryoneAllIn game = game & (street .~ Turn) . dealBoardCards 1
  | otherwise =
    game &
    (street .~ Turn) .
    (maxBet .~ 0) .
    (currentPosToAct .~ incPosToAct (_dealer game) game) .
    dealBoardCards 1 . resetPlayers

progressToRiver :: Game -> Game
progressToRiver game@Game {..}
  | isEveryoneAllIn game = game & (street .~ River) . dealBoardCards 1
  | otherwise =
    game &
    (street .~ River) .
    (maxBet .~ 0) .
    (currentPosToAct .~ incPosToAct _dealer game) .
    dealBoardCards 1 . resetPlayers

progressToShowdown :: Game -> Game
progressToShowdown game@Game {..} =
  game &
  (street .~ Showdown) . (winners .~ winners') . (players .~ awardedPlayers)
  where
    winners' = getWinners game
    awardedPlayers = awardWinners _players _pot winners'

-- need to give players the chips they are due and split pot if necessary
-- if only one active player then this is a result of everyone else folding 
-- and they are awarded the entire pot
--
-- If only one player is active during the showdown stage then this means all other players
-- folded to him. The winning player then has the choice of whether to "muck"
-- (not show) his cards or not.
-- SinglePlayerShowdown occurs when everyone folds to one player
awardWinners :: [Player] -> Int -> Winners -> [Player]
awardWinners _players pot' =
  \case
    MultiPlayerShowdown winners' ->
      let chipsPerPlayer = pot' `div` length winners'
          playerNames = snd <$> winners'
       in (\p@Player {..} ->
             if _playerName `elem` playerNames
               then Player {_chips = _chips + chipsPerPlayer, ..}
               else p) <$>
          _players
    SinglePlayerShowdown _ ->
      (\p@Player {..} ->
         if p `elem` getActivePlayers _players
           then Player {_chips = _chips + pot', ..}
           else p) <$>
      _players

isEveryoneAllIn :: Game -> Bool
isEveryoneAllIn game@Game {..}
  | _street == PreDeal = False
  | _street == Showdown = False
  | numPlayersIn < 2 = False
  | haveAllPlayersActed game = (numPlayersIn - numPlayersAllIn) <= 1
  | otherwise = False
  where
    numPlayersIn = length $ getActivePlayers _players
    numPlayersAllIn =
      length $
      filter (\Player {..} -> _playerState == In && _chips == 0) _players

-- TODO move players from waitlist to players list
-- TODO need to send msg to players on waitlist when a seat frees up to inform them 
-- to choose a seat and set limit for them t pick one
-- TODO - have newBlindNeeded field which new players will initially be put into in order to 
-- ensure they cant play without posting a blind before the blind position comes round to them
-- new players can of course post their blinds early. In the case of an early posting the initial
-- blind must be the big blind. After this 'early' blind or the posting of a normal blind in turn the 
-- new player will be removed from the newBlindNeeded field and can play normally.
getNextHand :: Game -> [Card] -> Game
getNextHand Game {..} newDeck =
  Game
    { _waitlist = newWaitlist
    , _maxBet = _bigBlind
    , _players = newPlayers
    , _board = []
    , _deck = newDeck
    , _winners = NoWinners
    , _street = PreDeal
    , _dealer = newDealer
    , _pot = 0
    , _currentPosToAct = nextPlayerToAct
    , ..
    }
  where
    incAmount = 1
    newDealer = modInc incAmount _dealer (length (getPlayersSatIn _players) - 1)
    freeSeatsNo = _maxPlayers - length _players
    newPlayers = resetPlayerCardsAndBets <$> _players
    newWaitlist = drop freeSeatsNo _waitlist
    nextPlayerToAct = modInc incAmount newDealer (length newPlayers - 1)

-- | If all players have acted and their bets are equal 
-- to the max bet then we can move to the next stage
haveAllPlayersActed :: Game -> Bool
haveAllPlayersActed game@Game {..}
  | _street == Showdown = True
  | _street == PreDeal = haveRequiredBlindsBeenPosted game
  | otherwise = not awaitingPlayerAction
  where
    activePlayers = getActivePlayers _players
    awaitingPlayerAction =
      any
        (\Player {..} ->
           not _actedThisTurn ||
           (_playerState == In && (_bet < _maxBet && _chips /= 0)))
        activePlayers

-- If all players have folded apart from a remaining player then the mucked boolean 
-- inside the player value will determine if we show the remaining players hand to the 
-- table. 
--
-- Otherwise we just get the handrankings of all active players.
getWinners :: Game -> Winners
getWinners game@Game {..} =
  if allButOneFolded game
    then SinglePlayerShowdown $
         head $
         flip (^.) playerName <$>
         filter (\Player {..} -> _playerState == In) _players
    else MultiPlayerShowdown $ maximums $ getHandRankings _players _board

-- Get the best hand for each active player (AllIn or In)/
--
-- If more than one plays holds the same winning hand then the second part of the tuple
-- will consist of all the players holding the hand
getHandRankings :: [Player] -> [Card] -> [((HandRank, [Card]), PlayerName)]
getHandRankings plyrs boardCards =
  (\(cs, Player {..}) -> (cs, _playerName)) <$>
  map (value . (++ boardCards) . view pockets &&& id) remainingPlayersInHand
  where
    remainingPlayersInHand =
      filter
        (\Player {..} ->
           (_playerState /= Folded) || (_playerState /= None) || null _pockets)
        plyrs

-- PlayerState reset to In if Folded. However we don't reset None
-- states as this gives the information that the player needs to post a big blind
-- if they dont want to wait for the dealer button to pass - this should probably be refactored
-- if it needs a comment to explain it
resetPlayerCardsAndBets :: Player -> Player
resetPlayerCardsAndBets Player {..} =
  Player
    { _pockets = []
    , _playerState = newPlayerState
    , _bet = 0
    , _committed = 0
    , _actedThisTurn = False
    , ..
    }
  where
    newPlayerState =
      if _playerState == Folded || _playerState == In
        then In
        else None

allButOneFolded :: Game -> Bool
allButOneFolded game@Game {..} = _street /= PreDeal && length playersInHand <= 1
  where
    playersInHand = filter ((== In) . (^. playerState)) _players

-- initially a players state is set to None to denote that they havent posted their blinds yet
getPlayer :: Text -> Int -> Player
getPlayer playerName chips =
  Player
    { _pockets = []
    , _bet = 0
    , _playerState = None
    , _playerName = playerName
    , _committed = 0
    , _actedThisTurn = False
    , _chips = chips
    }

isPlayerToAct :: Text -> Game -> Bool
isPlayerToAct playerName game
  | isNothing maybePlyr = False
  | otherwise =
    let plyr = fromJust maybePlyr
     in (_street game /= PreDeal && _street game /= Showdown) &&
        _playerName plyr == playerName &&
        _playerState plyr == In && not (isEveryoneAllIn game)
  where
    maybePlyr = _players game Safe.!! _currentPosToAct game
