{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Poker.Game where

import Control.Arrow

import Control.Monad.Random.Class
import Control.Monad.State hiding (state)
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

-- | Returns a standard deck of cards.
initialDeck :: [Card]
initialDeck = Card <$> [minBound ..] <*> [minBound ..]

-- | Returns both the dealt players and remaining cards left in deck.
-- We need to have the remaining cards in the deck for dealing
-- board cards over the next stages.
dealToPlayers :: [Card] -> [Player] -> ([Card], [Player])
dealToPlayers deck players =
  mapAccumL
    (\cards player ->
       if player ^. playerState == In
         then (drop 2 cards, (pockets .~ (take 2 cards)) player)
         else (cards, player))
    deck
    players

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
hasBettingFinished game@Game {..} =
  if _street == PreDeal || _street == Showdown
    then False
    else if not allPlayersActed
           then False
           else numPlayersIn <= 1 && numPlayersAllIn > 0
  where
    numPlayersAllIn =
      length $ filter (\Player {..} -> _playerState == Out AllIn) _players
    numPlayersIn = length $ filter (\Player {..} -> _playerState == In) _players
    allPlayersActed =
      null $
      filter
        (\Player {..} ->
           (not _actedThisTurn) &&
           (_playerState == Out AllIn || _playerState == In))
        _players

-- if no further player actions are possible (i.e betting has finished)
-- then actedThisTurn should be set to True for all active players in Hand.
-- This scenario occurs when all players or all but one players are all in. 
resetPlayers :: Game -> Game
resetPlayers game@Game {..} = Game {_players = newPlayers, ..}
  where
    bettingOver = hasBettingFinished game
    newPlayers =
      (\Player {..} -> Player {_bet = 0, _actedThisTurn = bettingOver, ..}) <$>
      _players

setWinners :: Game -> Game
setWinners game@Game {..} = game

progressToPreFlop =
  (street .~ PreFlop) . resetPlayers . deal . updatePlayersInHand

progressToFlop =
  (street .~ Flop) . (maxBet .~ 0) . (dealBoardCards 3) . resetPlayers

progressToTurn =
  (street .~ Turn) . (maxBet .~ 0) . (dealBoardCards 1) . resetPlayers

progressToRiver =
  (street .~ River) . (maxBet .~ 0) . (dealBoardCards 1) . resetPlayers

progressToShowdown game@Game {..} =
  Game {_street = Showdown, _winners = winners', _players = awardedPlayers, ..}
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
          playerNames = (snd <$> winners')
       in (\p@Player {..} ->
             if _playerName `elem` playerNames
               then Player {_chips = _chips + chipsPerPlayer, ..}
               else p) <$>
          _players
    SinglePlayerShowdown pName ->
      (\p@Player {..} ->
         if p `elem` (getActivePlayers _players)
           then Player {_chips = _chips + pot', ..}
           else p) <$>
      _players

-- | Just get the identity function if not all players acted otherwise we return 
-- the function necessary to progress the game to the next stage.
progressGame :: Game -> IO Game
progressGame game@Game {..} =
  if (haveAllPlayersActed game && not (allButOneFolded game)) ||
     _street == Showdown
    then case getNextStreet _street of
           PreFlop -> return $ progressToPreFlop game
           Flop -> return $ progressToFlop game
           Turn -> return $ progressToTurn game
           River -> return $ progressToRiver game
           Showdown -> return $ progressToShowdown game
           PreDeal -> do
             shuffledDeck <- shuffle initialDeck
             return $ getNextHand game shuffledDeck
    else if allButOneFolded game && (_street /= PreDeal || _street /= Showdown)
           then do
             print "ALL BUT ONE FOLDED IS TRUE"
             return $ progressToShowdown game
           else return game

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
    newDealer = _dealer `modInc` length (getPlayersSatIn _players)
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
           (_actedThisTurn == False) || (_playerState == In && _bet < _maxBet))
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
           (_playerState /= None) || (_pockets == []))
        plyrs

resetPlayerCardsAndBets :: Player -> Player
resetPlayerCardsAndBets Player {..} =
  Player {_pockets = [], _bet = 0, _committed = 0, _actedThisTurn = False, ..}

allButOneFolded :: Game -> Bool
allButOneFolded game@Game {..} =
  if _street == PreDeal
    then False
    else (length playersInHand) <= 1
  where
    playersInHand =
      filter
        (\Player {..} -> (_playerState == In) || (_playerState == Out AllIn))
        _players
