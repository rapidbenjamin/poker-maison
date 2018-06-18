{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module GameSpec where

import Control.Lens
import Data.List
import Data.Text (Text)
import Test.Hspec
import Test.QuickCheck hiding (Big, Small)

import Poker
import Poker.ActionValidation
import Poker.Game
import Poker.Types

import Control.Lens
import Control.Monad
import Control.Monad.State hiding (state)
import Data.List.Lens
import Data.List.Split
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import GHC.Generics
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Gen

instance Arbitrary Card where
  arbitrary = genericArbitrary

instance Arbitrary PlayerState where
  arbitrary = genericArbitrary

instance Arbitrary Out where
  arbitrary = genericArbitrary

instance Arbitrary Street where
  arbitrary = genericArbitrary

instance Arbitrary Rank where
  arbitrary = genericArbitrary

instance Arbitrary Suit where
  arbitrary = genericArbitrary

-- this instance allows us to create random game values that can be used for property based testing
instance Arbitrary Game where
  arbitrary = do
    _maxPlayers <- choose ((0, 10) :: (Integer, Integer))
    let x = fromInteger _maxPlayers
    noPlayers <- choose ((0, x) :: (Integer, Integer))
    let z = fromInteger noPlayers
    _players <- resize z arbitrary
    _waitlist <- arbitrary
    commSize <- choose ((0, 5) :: (Integer, Integer))
    let y = fromInteger commSize
    _board <- resize y arbitrary
    _deck <- resize 52 arbitrary
    _currentPosToAct <- arbitrary
    _dealer <- choose (0, length _players)
    _street <- arbitrary
    _smallBlind <- suchThat chooseAny (>= 0)
    let _bigBlind = _smallBlind * 2
    _pot <- suchThat chooseAny (\x -> x >= 0 && x >= _bigBlind)
    _maxBet <- suchThat chooseAny (>= 0)
    return Game {_maxPlayers = fromInteger x, ..}

instance Arbitrary Player where
  arbitrary = do
    _chips <- suchThat chooseAny (>= 0)
    _committed <- suchThat chooseAny (>= 0)
    _bet <-
      suchThat chooseAny (\x -> (x >= 0) && x <= _chips && x <= _committed)
    _playerName <- suchThat arbitrary (\n -> T.length n > 0)
    _pockets <- suchThat arbitrary (\cards -> (null cards || length cards == 2))
    _playerState <-
      suchThat arbitrary (\s -> (s == None && (_committed > 0)) || s /= None)
    _actedThisTurn <- arbitrary
    return Player {..}

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary
  shrink xs = T.pack <$> shrink (T.unpack xs)

player1 =
  Player
    { _pockets = []
    , _chips = 2000
    , _bet = 50
    , _playerState = In
    , _playerName = "player1"
    , _committed = 50
    , _actedThisTurn = True
    }

player2 =
  Player
    { _pockets = []
    , _chips = 2000
    , _bet = 0
    , _playerState = Out AllIn
    , _playerName = "player2"
    , _committed = 50
    , _actedThisTurn = False
    }

player3 =
  Player
    { _pockets = []
    , _chips = 2000
    , _bet = 0
    , _playerState = In
    , _playerName = "player3"
    , _committed = 50
    , _actedThisTurn = False
    }

player4 =
  Player
    { _pockets = []
    , _chips = 2000
    , _bet = 0
    , _playerState = None
    , _playerName = "player4"
    , _committed = 0
    , _actedThisTurn = False
    }

initPlayers = [player1, player2, player3]

main :: IO ()
main =
  hspec $ describe "Poker.Game" $ do
    describe "dealToPlayers" $ do
      it "should deal correct number of cards" $ do
        let (_, newPlayers) = dealToPlayers initialDeck initPlayers
        (all
           (\Player {..} ->
              if _playerState == In
                then length _pockets == 2
                else null _pockets)
           newPlayers)
      it "should preserve ordering of players" $ do
        property $ \(players) -> do
          length players <= 21 ==> do
            let players' = players :: [Player]
            let (remainingDeck, players) = dealToPlayers initialDeck players'
            (_playerName <$> players) == (_playerName <$> players')
      it "the resulting set of cards should contain no duplicates" $ do
        property $ \(players) -> do
          length players <= 21 ==> do
            let players' -- deal to players that have no pocket cards already
                 = (players :: [Player]) & traverse . pockets .~ ([] :: [Card])
            let (remainingDeck, players) = dealToPlayers initialDeck players'
            let playerCards = concat $ _pockets <$> players
            null $ playerCards `intersect` remainingDeck
    describe "dealBoardCards" $ do
      it "should deal correct number of cards to board" $ do
        property $ \(Positive n) -> do
          n < 52 ==> do
            let newGame = dealBoardCards n initialGameState
            length (newGame ^. board) `shouldBe` n
      it "should remove dealt cards from deck" $ do
        property $ \(Positive n) -> do
          n < 52 ==> do
            let newGame = dealBoardCards n initialGameState
            length (newGame ^. deck) `shouldBe` (length initialDeck - n)
    describe "haveAllPlayersActed" $ do
      it "should return True when all players have acted" $ do
        let game =
              (street .~ PreFlop) . (players .~ [player1, player2]) $
              initialGameState
        haveAllPlayersActed game `shouldBe` True
      it "should return False when not all players acted" $ do
        let unfinishedBlindsGame =
              (street .~ PreDeal) . (players .~ [player1, player4]) $
              initialGameState
        haveAllPlayersActed unfinishedBlindsGame `shouldBe` False
    describe "isRoundOver" $ do
      it "should return True when all but one player " $ do
        let game =
              (street .~ PreFlop) . (players .~ [player1, player2]) $
              initialGameState
        haveAllPlayersActed game `shouldBe` True
      it "should return False when not all players acted" $ do
        let unfinishedBlindsGame =
              (street .~ PreDeal) . (players .~ [player1, player4]) $
              initialGameState
        haveAllPlayersActed unfinishedBlindsGame `shouldBe` False
