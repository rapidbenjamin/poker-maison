{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Poker.Game where

import Control.Arrow

import Control.Monad.Random.Class
import Control.Monad.State
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Text (Text)

import Data.Monoid
import Debug.Trace
import Poker.Blinds
import System.Random.Shuffle (shuffleM)
import System.Timeout

import Poker.Hands
import Poker.Types
import Poker.Utils

import Control.Lens

-- | A standard deck of cards.
initialDeck :: [Card]
initialDeck = Card <$> [minBound ..] <*> [minBound ..]

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

-- | Betting is over if only one players or no player can bet.
-- Note that an all in player is still active they just don't have any more chips
-- to bet so if all players go all in then the rounds will still progress as normal 
-- until the final showdown stage.
hasBettingFinished :: Game -> Bool
hasBettingFinished game@Game{..}
  | _street == PreDeal || _street == Showdown = False
  | not allPlayersActed = False
  | otherwise = numPlayersIn <= 1 && numPlayersAllIn > 0
  where numPlayersAllIn = length $
              filter (\ Player{..} -> _playerState == Out AllIn) _players
        numPlayersIn = length $ filter (\ Player{..} -> _playerState == In) _players
        allPlayersActed = haveAllPlayersActed game

-- Unless in the scenario where everyone is all in 
-- if no further player actions are possible (i.e betting has finished)
-- then actedThisTurn should be set to True for all active players in Hand.
-- This scenario occurs when all players or all but one players are all in. 
resetPlayers :: Game -> Game
resetPlayers game@Game {..} = (players .~ newPlayers) game
  where
    bettingOver = hasBettingFinished game
    newPlayers =
      (\Player {..} -> Player {_bet = 0, _actedThisTurn = bettingOver, ..}) <$>
      _players

setWinners :: Game -> Game
setWinners game@Game {..} = game

progressToPreFlop :: Game -> Game
progressToPreFlop =
  (street .~ PreFlop) .
  (players %~ (<$>) (actedThisTurn .~ False)) . deal . updatePlayersInHand

progressToFlop :: Game -> Game
progressToFlop game
  | isEveryoneAllIn game = ((street .~ Flop) . dealBoardCards 3) game
  | otherwise =
    ((street .~ Flop) . (maxBet .~ 0) . dealBoardCards 3 . resetPlayers) game

progressToTurn :: Game -> Game
progressToTurn game
  | isEveryoneAllIn game = ((street .~ Turn) . dealBoardCards 1) game
  | otherwise =
    ((street .~ Turn) . (maxBet .~ 0) . dealBoardCards 1 . resetPlayers) game

progressToRiver :: Game -> Game
progressToRiver game
  | isEveryoneAllIn game = ((street .~ River) . dealBoardCards 1) game
  | otherwise =
    ((street .~ River) . (maxBet .~ 0) . dealBoardCards 1 . resetPlayers) game

progressToShowdown :: Game -> Game
progressToShowdown game@Game {..} =
  Game {_street = Showdown, _winners = winners', _players = awardedPlayers, ..}
  where
    winners' = getWinners game
    awardedPlayers = awardWinners _players _pot winners'

-- | Just get the identity function if not all players acted otherwise we return 
-- the function necessary to progress the game to the next stage.
progressGame :: Game -> IO Game
progressGame game@Game{..}
  | haveAllPlayersActed game && not (allButOneFolded game) ||
      _street == Showdown || isEveryoneAllIn game =
    case getNextStreet _street of
        PreFlop -> return $ progressToPreFlop game
        Flop -> return $ progressToFlop game
        Turn -> return $ progressToTurn game
        River -> return $ progressToRiver game
        Showdown -> return $ progressToShowdown game
        PreDeal -> getNextHand game <$> shuffle initialDeck
  | allButOneFolded game && (_street /= PreDeal || _street /= Showdown) =
      return $ progressToShowdown game
  | otherwise = return game

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
    SinglePlayerShowdown pName ->
      (\p@Player {..} ->
         if p `elem` getActivePlayers _players
           then Player {_chips = _chips + pot', ..}
           else p) <$>
      _players

isEveryoneAllIn :: Game -> Bool
isEveryoneAllIn game@Game {..}
  | _street == PreDeal = False
  | numPlayersIn < 2 = False
  | allPlayersActed =
    traceShow
      (show numPlayersIn ++ show numPlayersAllIn)
      ((numPlayersIn - numPlayersAllIn) <= 1)
  | otherwise = False
  where
    numPlayersIn = length $ getActivePlayers _players
    numPlayersAllIn =
      length $ (\Player {..} -> _playerState == Out AllIn) `filter` _players
    allPlayersActed = haveAllPlayersActed game

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
    , _currentPosToAct = nextPlayerToAct
    , ..
    }
  where
    newDealer = _dealer `modInc` length (getPlayersSatIn _players) - 1
    freeSeatsNo = _maxPlayers - length _players
    newPlayers = resetPlayerCardsAndBets <$> _players
    newWaitlist = drop freeSeatsNo _waitlist
    nextPlayerToAct = _dealer `modInc` (length newPlayers - 1)

-- | If all players have acted and their bets are equal 
-- to the max bet then we can move to the next stage
haveAllPlayersActed :: Game -> Bool
haveAllPlayersActed game@Game {..} =
  if _street == PreDeal
    then haveRequiredBlindsBeenPosted game
    else not awaitingPlayerAction
  where
    activePlayers = getActivePlayers _players
    awaitingPlayerAction =
      any
        (\Player {..} ->
           not _actedThisTurn || (_playerState == In && _bet < _maxBet))
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
         filter
           (\Player {..} -> _playerState == In || _playerState == Out AllIn)
           _players
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
           (_playerState /= Out Folded) ||
           (_playerState /= None) || null _pockets)
        plyrs

-- PlayerState reset to In if Out Folded or Out AllIn. However we don't reset None
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
      if _playerState == Out AllIn || _playerState == Out Folded
        then In
        else None

allButOneFolded :: Game -> Bool
allButOneFolded game@Game {..} = 
  _street /= PreDeal && length playersInHand <= 1
  where
    playersInHand =
      filter
        (\Player {..} -> (_playerState == In) || (_playerState == Out AllIn))
        _players
