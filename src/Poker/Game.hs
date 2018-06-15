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
-- community cards over the next stages.
deal :: [Card] -> [Player] -> ([Card], [Player])
deal deck players =
  mapAccumL
    (\cards player ->
       if player ^. playerState == In
         then (drop 2 cards, (pockets .~ (take 2 cards)) player)
         else (cards, player))
    deck
    players

-- | Move game from the PreDeal (blinds betting) stage to the PreFlop stage
-- First we determine the players that are then we deal them their hands 
-- and reset all bets.
--
-- We use the list of required blinds to calculate if a player has posted 
-- chips sufficient to be "In" for this hand.
progressToPreFlop :: Game -> [Maybe Blind] -> Game
progressToPreFlop game@Game {..} requiredBlinds =
  let newPlayers = zipWith updatePlayer requiredBlinds _players
      (remainingDeck, dealtPlayers) = deal _deck newPlayers
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
        , _committed = 0
        , ..
        }
