{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.Game where

import Control.Arrow

------------------------------------------------------------------------------
import Control.Monad.Random.Class
import Control.Monad.State hiding (state)
import Data.List
import Data.List.Split
import Data.Maybe

import Data.Monoid
import Debug.Trace
import System.Random.Shuffle (shuffleM)

import Poker.Blinds

------------------------------------------------------------------------------
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

resetPlayers :: Game -> Game
resetPlayers Game {..} = Game {_players = newPlayers, ..}
  where
    newPlayers =
      (\Player {..} -> Player {_bet = 0, _actedThisTurn = False, ..}) <$>
      _players

setWinners :: Game -> Game
setWinners game@Game {..} = game

progressToPreDeal = getNextHand

progressToPreFlop =
  (street .~ PreFlop) . resetPlayers . deal . determineWhichPlayersAreInHand

progressToFlop = (street .~ Flop) . (dealBoardCards 3) . resetPlayers

progressToTurn = (street .~ Turn) . (dealBoardCards 1) . resetPlayers

progressToRiver = (street .~ River) . (dealBoardCards 1) . resetPlayers

-- need to give players the chips they are due and split pot if necessary
progressToShowdown game@Game {..} =
  Game
    { _street = Showdown
    , _winners = winners
    , _players = awardWinners _players chipsPerPlayer
    , ..
    }
  where
    winners = getWinners game
    winningPlayers = snd <$> winners
    chipsPerPlayer = _pot `div` (length winningPlayers)
    awardWinners _players chipsToAward =
      (\p@Player {..} ->
         if p `elem` winningPlayers
           then Player {_chips = _chips + chipsToAward, ..}
           else p) <$>
      _players

-- | Just get the identity function if not all players acted otherwise we return 
-- the function necessary to progress the game to the next stage.
progressGame :: Game -> IO Game
progressGame game@Game {..} =
  if haveAllPlayersActed game || _street == Showdown
    then case getNextStreet _street of
           PreFlop -> return $ progressToPreFlop game
           Flop -> return $ progressToFlop game
           Turn -> return $ progressToTurn game
           River -> return $ progressToRiver game
           Showdown -> return $ progressToShowdown game
           PreDeal -> progressToPreDeal game
    else if allButOneFolded game
           then return $ progressToShowdown game
           else return game

-- TODO move players from waitlist to players list
-- TODO need to send msg to players on waitlist when a seat frees up to inform them 
-- to choose a seat and set limit for them t pick one
-- TODO - have newBlindNeeded field which new players will initially be put into in order to 
-- ensure they cant play without posting a blind before the blind position comes round to them
-- new players can of course post their blinds early. In the case of an early posting the initial
-- blind must be the big blind. After this 'early' blind or the posting of a normal blind in turn the 
-- new player will be removed from the newBlindNeeded field and can play normally.
getNextHand :: Game -> IO Game
getNextHand Game {..} = do
  shuffledDeck <- shuffle initialDeck
  return
    Game
      { _waitlist = newWaitlist
      , _maxBet = 0
      , _players = newPlayers
      , _board = []
      , _deck = shuffledDeck
      , _winners = []
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

-- | Betting has concluded is there is only one or less players not either Out AllIn or In.
-- Note that an all in player is still active they just don't have any more chips
-- to bet so if all players go all in then the rounds will still progress as normal 
-- until the final showdown stage.
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

-- | If all players have acted and their bets are equal 
-- to the max bet then we can move to the next stage
haveAllPlayersActed :: Game -> Bool
haveAllPlayersActed game@Game {..} =
  if _street == PreDeal
    then haveRequiredBlindsBeenPosted game
    else not awaitingPlayerAction
  where
    activePlayers = getActivePlayers _players
    maxBet = maximum $ flip (^.) bet <$> activePlayers
    awaitingPlayerAction =
      any
        (\Player {..} ->
           (_actedThisTurn == False) || (_playerState == In && _bet /= maxBet))
        activePlayers

-- | If more than one plays holds the same winning hand then a list will be returned of length
-- equal to winning players
getWinners :: Game -> [((HandRank, [Card]), Player)]
getWinners Game {..} = maximums $ getHandRankings _players _board

getHandRankings plyrs boardCards =
  map (value . (++ boardCards) . view pockets &&& id) filteredPlyrs
  where
    filteredPlyrs =
      filter
        (\Player {..} ->
           (_playerState /= Out Folded) ||
           (_playerState /= None) || (_pockets == []))
        plyrs

resetPlayerCardsAndBets :: Player -> Player
resetPlayerCardsAndBets Player {..} =
  Player {_pockets = [], _bet = 0, _committed = 0, _actedThisTurn = False, ..}
