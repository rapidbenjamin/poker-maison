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

module ActionSpec where

import Control.Lens
import Data.List
import Data.Text (Text)
import Test.Hspec
import Test.QuickCheck hiding (Big, Small)

import Poker
import Poker.ActionValidation
import Poker.Actions
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
    , _bet = 0
    , _playerState = In
    , _playerName = "player1"
    , _committed = 100
    , _actedThisTurn = False
    }

player2 =
  Player
    { _pockets = []
    , _chips = 2000
    , _bet = 0
    , _playerState = Out Folded
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
    , _playerState = In
    , _playerName = "player3"
    , _committed = 0
    , _actedThisTurn = False
    }

player5 =
  Player
    { _pockets = []
    , _chips = 4000
    , _bet = 4000
    , _playerState = In
    , _playerName = "player5"
    , _committed = 4000
    , _actedThisTurn = True
    }

player6 =
  Player
    { _pockets = []
    , _chips = 2000
    , _bet = 200
    , _playerState = In
    , _playerName = "player6"
    , _committed = 250
    , _actedThisTurn = True
    }

bettingFinishedGame =
  ((players .~ [player1, player2]) . (street .~ PreFlop)) initialGameState

bettingNotFinishedGame =
  ((players .~ [player1, player2, player3, player4]) . (street .~ PreFlop))
    initialGameState

main :: IO ()
main =
  hspec $ describe "Poker.Actions" $ do
    describe "bet" $ do
      it "should update player attributes correctly" $ do
        let game =
              (street .~ PreFlop) . (players .~ [player1, player2, player3]) $
              initialGameState
        let betValue = 200
        let pName = "player1"
        let expectedPlayers = [player1, player2, player3]
        let newGame = makeBet betValue pName game
        let playerWhoBet = newGame ^? players . ix 0
        let expectedPlayer =
              Player
                { _pockets = []
                , _chips = 2000 - betValue
                , _bet = betValue
                , _playerState = In
                , _playerName = "player1"
                , _committed = 100 + betValue
                , _actedThisTurn = True
                }
        playerWhoBet `shouldBe` Just expectedPlayer
      it "should update player attributes correctly when bet all in" $ do
        let game =
              (street .~ PreFlop) . (players .~ [player1, player2, player3]) $
              initialGameState
        let betValue = player1 ^. chips
        let pName = "player1"
        let expectedPlayers = [player1, player2, player3]
        let newGame = makeBet betValue pName game
        let playerWhoBet = newGame ^? players . ix 0
        let expectedPlayer =
              Player
                { _pockets = []
                , _chips = 2000 - betValue
                , _bet = betValue
                , _playerState = Out AllIn
                , _playerName = "player1"
                , _committed = 100 + betValue
                , _actedThisTurn = True
                }
        playerWhoBet `shouldBe` Just expectedPlayer
      it "should increment position to act" $ do
        let game =
              (street .~ PreFlop) . (currentPosToAct .~ 0) .
              (players .~ [player1, player2, player3]) $
              initialGameState
        let betValue = 200
        let pName = "player1"
        let expectedPlayers = [player1, player2, player3]
        let newGame = makeBet betValue pName game
        let newPositionToAct = newGame ^. currentPosToAct
        let expectedNewPositionToAct = 1
        newPositionToAct `shouldBe` expectedNewPositionToAct
    describe "foldCards" $ do
      it "should update player attributes correctly" $ do
        let game =
              (street .~ PreFlop) . (players .~ [player1, player2, player3]) $
              initialGameState
        let pName = "player1"
        let expectedPlayers = [player1, player2, player3]
        let newGame = foldCards pName game
        let playerWhoFolded = newGame ^? players . ix 0
        let expectedPlayer =
              Player
                { _pockets = []
                , _chips = 2000
                , _bet = 0
                , _playerState = Out Folded
                , _playerName = "player1"
                , _committed = 100
                , _actedThisTurn = True
                }
        playerWhoFolded `shouldBe` Just expectedPlayer
      it "should increment position to act" $ do
        let game =
              (street .~ PreFlop) . (currentPosToAct .~ 0) .
              (players .~ [player1, player2, player3]) $
              initialGameState
        let pName = "player1"
        let expectedPlayers = [player1, player2, player3]
        let newGame = foldCards pName game
        let newPositionToAct = newGame ^. currentPosToAct
        let expectedNewPositionToAct = 1
        newPositionToAct `shouldBe` expectedNewPositionToAct
    describe "call" $ do
      it "should update player attributes correctly" $ do
        let game =
              (street .~ PreFlop) . (players .~ [player6, player1]) $
              initialGameState
        let betValue = 200
        let pName = "player1"
        let expectedPlayers = [player1, player2, player3]
        let newGame = call pName game
        let playerWhoCalled = newGame ^? players . ix 1
        let expectedPlayer =
              Player
                { _pockets = []
                , _chips = 1900
                , _bet = 100
                , _playerState = In
                , _playerName = "player1"
                , _committed = 200
                , _actedThisTurn = True
                }
        playerWhoCalled `shouldBe` Just expectedPlayer
      it "should update player attributes correctly when called all in" $ do
        let game =
              (street .~ PreFlop) . (players .~ [player5, player1]) $
              initialGameState
        let betValue = 200
        let pName = "player1"
        let expectedPlayers = [player1, player2, player3]
        let newGame = call pName game
        let playerWhoCalled = newGame ^? players . ix 1
        let expectedPlayer =
              Player
                { _pockets = []
                , _chips = 0
                , _bet = 2000
                , _playerState = Out AllIn
                , _playerName = "player1"
                , _committed = 2100
                , _actedThisTurn = True
                }
        playerWhoCalled `shouldBe` Just expectedPlayer
      it "should increment position to act" $ do
        let game =
              (street .~ PreFlop) . (currentPosToAct .~ 0) .
              (players .~ [player1, player2, player3]) $
              initialGameState
        let betValue = 200
        let pName = "player1"
        let expectedPlayers = [player1, player2, player3]
        let newGame = call pName game
        let newPositionToAct = newGame ^. currentPosToAct
        let expectedNewPositionToAct = 1
        newPositionToAct `shouldBe` expectedNewPositionToAct
