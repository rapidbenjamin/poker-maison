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

import Data.Proxy
import Data.Maybe

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Vector as V

import Debug.Trace

import Prelude 
import Data.Text (Text)
import qualified Data.Text as T
import Data.Either


import Poker.ActionValidation
import Poker.Game.Blinds
import Poker.Game.Game
import Poker.Game.Utils
import Poker.Poker
import Poker.Types
import Poker.Game.Hands

genShuffledCards :: Int -> Gen [Card]
genShuffledCards n = do 
     cs <- Gen.shuffle $ unDeck initialDeck
     return $ take n cs

genShuffledDeck :: Gen Deck
genShuffledDeck = do 
    cs <- Gen.shuffle $ unDeck initialDeck 
    return $ Deck cs

genSuit :: Gen Suit
genSuit = Gen.enum Diamonds Spades

genSameSuitCards :: Int -> Gen [Card]
genSameSuitCards n = do
     suit' <- genSuit
     cs <- Gen.shuffle $ filter ((== suit') . suit) (unDeck initialDeck)
     return $ take n cs

genPockets :: Gen (Maybe PocketCards)
genPockets = do
     Deck cards <- genShuffledDeck
     let [c1, c2] = take 2 cards
     return $ Just $ PocketCards c1 c2

genNoPockets :: Gen (Maybe PocketCards)
genNoPockets = return Nothing

genPlayer :: [PlayerState] -> Int -> Gen Player
genPlayer possibleStates position = do
  _playerState <- Gen.element possibleStates
  _actedThisTurn <- Gen.bool
  _chips <- Gen.int $ Range.linear 0 10000
  _committed <- Gen.int $ Range.linear 0 10000
  _bet <- Gen.int $ Range.linear 0 _committed
  _actedThisTurn <- Gen.bool
  _pockets <- if _playerState == SatOut then genNoPockets else genPockets
  return Player {..}
    where _playerName = "player" <> (T.pack $ show position) 

--    data Player = Player
--  { _pockets :: Maybe PocketCards
--  , _chips :: Int
--  , _bet :: Bet
--  , _playerState :: PlayerState
--  , _playerName :: Text
--  , _committed :: Bet
--  , _actedThisTurn :: Bool