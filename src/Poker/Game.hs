{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Poker.Game where

import Control.Arrow

------------------------------------------------------------------------------
import Control.Monad.Random.Class
import Control.Monad.State hiding (state)
import Data.List.Split
import System.Random.Shuffle (shuffleM)

------------------------------------------------------------------------------
import Poker.Hands
import Poker.Types
import Poker.Utils

------------------------------------------------------------------------------
initialDeck :: [Card]
initialDeck = Card <$> [minBound ..] <*> [minBound ..]

takePocketCards :: [Card] -> Int -> ([[Card]], [Card])
takePocketCards deck n =
  let splitDeck = splitAt (n * 2) deck
   in (chunksOf 2 $ fst $ splitDeck, snd splitDeck)
