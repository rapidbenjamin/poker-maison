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

import Control.Lens

------------------------------------------------------------------------------
initialDeck :: [Card]
initialDeck = Card <$> [minBound ..] <*> [minBound ..]

-- | returns both the dealt players and remaining cards left in deck for
--   dealing future community cards.
deal :: [Card] -> [Player] -> ([Card], [Player])
deal deck players =
  mapAccumL
    (\cards player ->
       if player ^. playerState == In
         then (drop 2 cards, (pockets .~ (take 2 cards)) player)
         else (cards, player))
    deck
    players

progressToPreFlop :: Game -> Game
progressToPreFlop Game {..} = undefined
