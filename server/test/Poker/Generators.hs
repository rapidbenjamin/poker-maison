{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}



module Poker.Generators where
    
import           Control.Monad

import qualified Data.List as List
import qualified Data.Text as Text
import Data.Singletons
import Data.Singletons.TH 
import Data.Proxy
import Data.Maybe

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Vector as V

import Debug.Trace

import Prelude 
import Data.Either

import Poker.ActionValidation
import Poker.Game.Blinds
import Poker.Game.Game
import Poker.Game.Utils
import Poker.Poker
import Poker.Types
import Poker.Game.Hands

genShuffledCards :: Int -> Gen [Card]
genShuffledCards n = Gen.shuffle (unDeck initialDeck) >>= \cs -> return $ take n cs

genSuit :: Gen Suit
genSuit = Gen.enum Diamonds Spades

genSameSuitCards :: Int -> Gen [Card]
genSameSuitCards n = do
     suit' <- genSuit
     cs <- Gen.shuffle $ filter ((== suit') . suit) (unDeck initialDeck)
     return $ take n cs
