{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}


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
import Control.Monad.State
import Data.Traversable
import Control.Lens

import Poker.ActionValidation
import Poker.Game.Blinds
import Poker.Game.Game
import Poker.Game.Utils
import Poker.Poker
import Poker.Types
import Poker.Game.Hands
import GHC.Enum

allPStates :: [PlayerState]
allPStates = [SatOut, Folded, In]

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

genDealPockets :: [Card] -> Gen (Maybe PocketCards, [Card])
genDealPockets cs = do
     let ([c1, c2], remainingCs) = splitAt 2 cs
     return (Just $ PocketCards c1 c2, remainingCs)

genNoPockets :: [Card] -> Gen (Maybe PocketCards, [Card])
genNoPockets cs = return (Nothing, cs)


--mapAccumR :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c) Source#

--The mapAccumR function behaves like a combination of fmap and foldr; 
--it applies a function to each element of a structure, 
--  passing an accumulating parameter from right to left,
--     and returning a final value of this accumulator together with the new structure.
genPlayers :: [PlayerState] -> Int -> [Card] -> Gen ([Player], [Card])
genPlayers possibleStates playerCount cards =
     (flip . (runStateT .) . traverse . (StateT .) . flip) f cards []
        >>= \(ps, cs) -> return $ (concat ps, cs) 
       where
          f :: [Card] -> [Player] -> Gen ([Player], [Card])
          f cs ps = do
               (p, remainingCs) <- genPlayer' possibleStates (length ps + 1) cs 
               return (p : ps, remainingCs)


genPlayer' :: [PlayerState] -> Int -> [Card] -> Gen (Player, [Card])
genPlayer' possibleStates position cs = do
     pState <- Gen.element possibleStates
     let shouldDeal = pState /= SatOut
     (pocketCs, remainingCs) <- if shouldDeal then genDealPockets cs else genNoPockets cs
     p <- genPlayer pState pName pocketCs
     return (p, remainingCs)
     where 
       pName = "player" <> (T.pack $ show position) 


-- if given Just cards then will deal player (as long as not sat out) and return the remaining cards
genPlayer :: PlayerState -> PlayerName -> Maybe PocketCards -> Gen Player
genPlayer _playerState _playerName _pockets = do
  _actedThisTurn <- Gen.bool
  _chips <- Gen.int $ Range.linear 0 10000
  _committed <- Gen.int $ Range.linear 0 10000
  _bet <- Gen.int $ Range.linear 0 _committed
  _actedThisTurn <- Gen.bool
  return Player {..}

genGame :: Gen Game
genGame = do
    let _street = Flop
    d@(Deck cs) <- genShuffledDeck
    let 
       boardCount = numBoardCards _street
       (boardCards, remainingCs) = splitAt boardCount cs
    _smallBlind <- Gen.int $ Range.linear 1 100
    _maxPlayers <- Gen.int $ Range.linear 2 9
    playerCount <- Gen.int $ Range.linear 2 _maxPlayers
    (_players, remainingCs') <- genPlayers allPStates playerCount remainingCs
    let
      _waitlist = []
      _bigBlind = _smallBlind * 2
      _maxBuyInChips = _bigBlind * 200
      _minBuyInChips = _bigBlind * 100
      (_board, remainingCs'') = splitAt (numBoardCards _street) remainingCs'
      _deck = Deck remainingCs''
      betsThisRound = (^. bet) <$> _players
      _maxBet = maximum betsThisRound
      betSum = sum betsThisRound
      playerCount = length _players
      _winners = if _street == River then getWinners Game{..} else NoWinners
    _pot <- Gen.int $ Range.linear betSum (betSum * fromEnum _street)
    _dealer <- Gen.int $ Range.linear 0 (playerCount - 1)
    _currentPosToAct <- Gen.int $ Range.linear 0 (playerCount - 1)
    return Game {..}

numBoardCards :: Street -> Int
numBoardCards = 
     \case
       PreDeal -> 0
       PreFlop -> 0
       Flop    -> 3
       Turn    -> 4
       River   -> 5
