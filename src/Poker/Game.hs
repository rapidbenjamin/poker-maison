{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.Game where

------------------------------------------------------------------------------
import Control.Monad.Random.Class
import Control.Monad.State hiding (state)
import Data.List
import Data.List.Split
import System.Random.Shuffle (shuffleM)

------------------------------------------------------------------------------
import Poker.Hands
import Poker.Types
import Poker.Utils

------------------------------------------------------------------------------
initialDeck :: [Card]
initialDeck = Card <$> [minBound ..] <*> [minBound ..]

-- | returns both the dealt players and remaining cards left in deck for
--   dealing future community cards.
deal :: [Card] -> [Player] -> ([Card], [Player])
deal deck players =
  mapAccumL
    (\cards p@Player {..} ->
       if _playerState == In
         then (drop 2 cards, Player {_pockets = take 2 cards, ..})
         else (cards, p))
    deck
    players

progressToPreFlop :: Game -> Game
progressToPreFlop Game {..} = undefined
