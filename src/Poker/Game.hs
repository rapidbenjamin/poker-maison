{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.Game where

------------------------------------------------------------------------------
import Control.Monad.Random.Class
import Control.Monad.State hiding (state)
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace
import System.Random.Shuffle (shuffleM)

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
  Game {_board = boardCards, _deck = newDeck, ..}
  where
    (boardCards, newDeck) = splitAt n _deck

-- | Move game from the PreDeal (blinds betting) stage to the PreFlop stage
-- First we determine the players that are then we deal them their hands 
-- and reset all bets.
--
-- We use the list of required blinds to calculate if a player has posted 
-- chips sufficient to be "In" for this hand.
progressToPreFlop :: Game -> [Maybe Blind] -> Game
progressToPreFlop game@Game {..} requiredBlinds =
  let newPlayers = zipWith updatePlayer requiredBlinds _players
      (remainingDeck, dealtPlayers) = dealToPlayers _deck newPlayers
   in Game
        {_street = PreDeal, _players = dealtPlayers, _deck = remainingDeck, ..}
  where
    updatePlayer blindReq Player {..} =
      Player
        { _playerState =
            if isNothing blindReq
              then In
              else _playerState
        , _bet = 0
        , ..
        }

getNextStreet :: Street -> Street
getNextStreet River = minBound
getNextStreet _street = succ _street

clearBets :: Game -> Game
clearBets Game {..} = Game {_players = newPlayers, ..}
  where
    newPlayers = (\Player {..} -> Player {_bet = 0, ..}) <$> _players

setWinners :: Game -> Game
setWinners game@Game {..} = game

-- | Just get the identity function if not all players acted otherwise we return 
-- the function necessary to progress the game to the next stage.
progressGame :: Game -> IO Game
progressGame game@Game {..} =
  if not $ hasBettingFinished game
    then return game
    else case getNextStreet _street of
           PreFlop -> return $ dealBoardCards 3 (clearBets game)
           Flop -> return $ dealBoardCards 1 (clearBets game)
           Turn -> return $ dealBoardCards 1 (clearBets game)
           Showdown -> return $ setWinners (clearBets game)
           PreDeal -> getNextHand (clearBets game)

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

-- | Betting has concluded is there is only one or less active players remaining
-- or if the all active player have posted a bet equal to the max bet or dont have 
-- an In state
hasBettingFinished :: Game -> Bool
hasBettingFinished Game {..} =
  length activePlayers <= 1 || not awaitingPlayerAction
  where
    activePlayers = getActivePlayers _players
    maxBet = maximum $ flip (^.) bet <$> activePlayers
    awaitingPlayerAction =
      any
        (\Player {..} -> _actedThisTurn == False || _bet /= maxBet)
        activePlayers

resetPlayerCardsAndBets :: Player -> Player
resetPlayerCardsAndBets Player {..} =
  Player {_pockets = [], _bet = 0, _committed = 0, ..}
