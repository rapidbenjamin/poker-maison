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
import Data.Tuple

allPStates :: [PlayerState]
allPStates = [SatOut ..]


allPStreets :: [Street]
allPStreets = [PreDeal ..] 


numBoardCards :: Street -> Int
numBoardCards = 
     \case
       PreDeal  -> 0
       PreFlop  -> 0
       Flop     -> 3
       Turn     -> 4
       River    -> 5
       Showdown -> 5
       

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


genPlayers :: Int -> [PlayerState] -> Int -> [Card] -> Gen ([Player], [Card])
genPlayers requiredInPlayers possibleStates playerCount cs = do
     ps <- replicateM playerCount $ do 
          pState <- Gen.element possibleStates
          genPlayer pState "player" Nothing
     if (length $ getActivePlayers ps) < requiredInPlayers then Gen.discard else
       return $ swap $ dealPlayersGen ps cs


dealPlayersGen :: [Player] -> [Card] -> ([Card], [Player])
dealPlayersGen ps cs = _2 %~ nameByPos $ mapAccumR (\cs p ->
     let (maybeDealtP, remainingCs') = dealPlayer cs p in 
       (remainingCs', maybeDealtP)) cs ps
   where 
     newName pos = playerName .~ ("player" <> (T.pack $ show pos))
     nameByPos ps = imap (\pos p -> newName pos p) ps
     dealPlayer cs plyr@Player{..} 
         | _playerState == SatOut = (plyr, cs)
         | otherwise = (,) Player {_pockets = Just $ PocketCards c1 c2, .. } remainingCs'
             where ([c1, c2], remainingCs') = splitAt 2 cs

     
genPlayer' :: [PlayerState] -> Int -> [Card] -> Gen (Player, [Card])
genPlayer' possibleStates position cs = do
     pState <- Gen.element possibleStates
     let shouldDeal = pState /= SatOut || null cs
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


genGame :: [Street] -> Gen Game
genGame possibleStreets = do
    _street <- Gen.element possibleStreets
    let d@(Deck cs) = initialDeck
    let 
       boardCount = numBoardCards _street
       (boardCards, remainingCs) = splitAt boardCount cs
    _smallBlind <- Gen.int $ Range.linear 1 100
    _maxPlayers <- Gen.int $ Range.linear 2 9
    playerCount <- Gen.int $ Range.linear 2 _maxPlayers
    let requiredInPlyrs = if _street == Showdown then 2 else 0
    (_players, remainingCs') <- genPlayers requiredInPlyrs allPStates playerCount remainingCs
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
      _winners = if _street == Showdown then getWinners Game{..} else NoWinners
    _pot <- Gen.int $ Range.linear betSum (betSum * fromEnum _street)
    _dealer <- Gen.int $ Range.linear 0 (playerCount - 1)
    _currentPosToAct <- Gen.int $ Range.linear 0 (playerCount - 1)
    return Game {..}
