{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Poker.Game where

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
