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

module ActionValidationSpec where

import Control.Lens
import Data.List
import Data.Text (Text)
import Test.Hspec
import Test.QuickCheck hiding (Big, Small)

import Poker
import Poker.ActionValidation
import Poker.Types

import Control.Lens
import Control.Monad.State hiding (state)
import Data.List.Lens
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
    return Player {..}

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary
  shrink xs = T.pack <$> shrink (T.unpack xs)

player1 =
  Player
    { _pockets = []
    , _chips = 2000
    , _bet = 200
    , _playerState = In
    , _playerName = "player1"
    , _committed = 250
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
    , _chips = 300
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

player5 =
  Player
    { _pockets = []
    , _chips = 2000
    , _bet = 0
    , _playerState = In
    , _playerName = "player5"
    , _committed = 50
    , _actedThisTurn = False
    }

playerFixtures = [player1, player2, player3, player4]

playerFixtures2 = [player3, player5]

main :: IO ()
main =
  hspec $
  describe "ActionValidation" $ do
    describe "Player Acting in Turn Validation" $ do
      let game =
            (currentPosToAct .~ 0) .
            (street .~ PreFlop) . (players .~ playerFixtures) $
            initialGameState
      it
        "returns Just OutOfTurn Error if given player is not in current position to act" $ do
        let expectedErr =
              Just $
              InvalidMove "player3" $
              OutOfTurn $ CurrentPlayerToActErr "player1"
        isPlayerActingOutOfTurn game "player3" `shouldBe` expectedErr
      it "return no Error if player is acting in turn" $ do
        isPlayerActingOutOfTurn game "player1" `shouldBe` Nothing
      it
        "returns Just NotAtTable Error if no player with playerName is sat at table" $ do
        let expectedErr = Just $ NotAtTable "MissingPlayer"
        checkPlayerSatAtTable game "MissingPlayer" `shouldBe` expectedErr
    describe "blinds" $ do
      describe "getSmallBlindPosition" $ do
        it "returns correct small blind position in three player game" $ do
          let dealerPos = 0
          getSmallBlindPosition ["Player1", "Player2", "Player3"] dealerPos `shouldBe`
            1
        it "returns correct small blind position in two player game" $ do
          let dealerPos = 0
          getSmallBlindPosition ["Player1", "Player2"] dealerPos `shouldBe` 0
      describe "blindRequiredByPlayer" $ do
        it
          "returns Just Small if player position is dealer + 1 for three players" $ do
          let testPlayers =
                (playerState .~ In) <$>
                (getPlayer <$> ["Player1", "Player2", "Player3"] <*> [100])
          let game = players .~ testPlayers $ initialGameState
          blindRequiredByPlayer game "Player2" `shouldBe` Just Small
        it "returns Just Big if player position is dealer + 2 for three players" $ do
          let testPlayers =
                (playerState .~ In) <$>
                (getPlayer <$> ["Player1", "Player2", "Player3"] <*> [100])
          let game = players .~ testPlayers $ initialGameState
          blindRequiredByPlayer game "Player3" `shouldBe` Just Big
        it
          "returns Nothing if player position is dealer for three players and playerState is In" $ do
          let testPlayers =
                (playerState .~ In) <$>
                (getPlayer <$> ["Player1", "Player2", "Player3"] <*> [100])
          let game = players .~ testPlayers $ initialGameState
          blindRequiredByPlayer game "Player1" `shouldBe` Nothing
        it
          "returns Just Big if player position is dealer for three players and playerState is None" $ do
          let testPlayers =
                (playerState .~ None) <$>
                (getPlayer <$> ["Player1", "Player2", "Player3"] <*> [100])
          let game = players .~ testPlayers $ initialGameState
          blindRequiredByPlayer game "Player1" `shouldBe` Nothing
        it "returns Just Small if player position is dealer for two players" $ do
          let testPlayers =
                (playerState .~ In) <$>
                (getPlayer <$> ["Player1", "Player2"] <*> [100])
          let game = players .~ testPlayers $ initialGameState
          blindRequiredByPlayer game "Player1" `shouldBe` Just Small
        it "returns Just Big if player position is dealer + 1 for two players" $ do
          let testPlayers = getPlayer <$> ["Player1", "Player2"] <*> [100]
          let game = players .~ testPlayers $ initialGameState
          blindRequiredByPlayer game "Player2" `shouldBe` Just Big
      context "Players with PlayerState set to None" $
        it "should always require bigBlind" $
        property $ \game@Game {..} playerName -> do
          let player =
                (\Player {..} -> _playerName == playerName) `find` _players
          case player of
            Just Player {..} -> do
              let result = blindRequiredByPlayer game playerName
              (_playerState == None && result == Just Big) ||
                _playerState /= None
            Nothing -> True
    describe "canBet" $ do
      it
        "should return NotEnoughChipsForAction InvalidMoveErr if raise value is greater than remaining chips" $ do
        let game2 =
              (players .~ playerFixtures2) . (street .~ PreFlop) $
              initialGameState
        let playerName = "player3"
        let amount = 10000
        let expectedErr = NotEnoughChipsForAction
        canBet playerName amount game2 `shouldBe` Just expectedErr
      it
        "should return CannotBetShouldRaiseInstead InvalidMoveErr if players have already bet or raised already" $ do
        let game2 =
              (players .~ playerFixtures) . (street .~ PreFlop) $
              initialGameState
        let playerName = "player3"
        let amount = 50
        let expectedErr = CannotBetShouldRaiseInstead
        canBet playerName amount game2 `shouldBe` Just expectedErr
      it
        "should return BetLessThanBigBlind InvalidMoveErr if bet is less than the current big blind" $ do
        let game2 =
              (players .~ playerFixtures2) . (street .~ PreFlop) $
              initialGameState
        let playerName = "player3"
        let amount = 2
        let expectedErr = BetLessThanBigBlind
        canBet playerName amount game2 `shouldBe` Just expectedErr
      it "should not return an error if player can bet" $ do
        let game2 =
              (players .~ playerFixtures2) . (street .~ PreFlop) $
              initialGameState
        let playerName = "player3"
        let amount = 100
        canBet playerName amount game2 `shouldBe` Nothing
      it
        "should return InvalidActionForStreet InvalidMoveErr if game stage is PreDeal" $ do
        let preDealGame =
              (street .~ PreDeal) . (players .~ playerFixtures2) $
              initialGameState
        let playerName = "player3"
        let amount = 100
        let expectedErr = InvalidActionForStreet
        canBet playerName amount preDealGame `shouldBe` Just expectedErr
      it
        "should return InvalidActionForStreet InvalidMoveErr if game stage is Showdown" $ do
        let showdownGame =
              (street .~ Showdown) . (players .~ playerFixtures2) $
              initialGameState
        let playerName = "player3"
        let amount = 100
        let expectedErr = InvalidActionForStreet
        canBet playerName amount showdownGame `shouldBe` Just expectedErr
    describe "canRaise" $ do
      it
        "should return InvalidActionForStreet InvalidMoveErr if game stage is PreDeal" $ do
        let preDealGame =
              (street .~ PreDeal) . (players .~ playerFixtures2) $
              initialGameState
        let playerName = "player3"
        let amount = 100
        let expectedErr = InvalidActionForStreet
        canBet playerName amount preDealGame `shouldBe` Just expectedErr
      it
        "should return RaiseAmountBelowMinRaise InvalidMoveErr if game stage is PreDeal" $ do
        let game =
              (street .~ PreFlop) . (players .~ playerFixtures) $
              initialGameState
        let playerName = "player3"
        let amount = 50
        let minRaise = 400
        let expectedErr = RaiseAmountBelowMinRaise minRaise
        canRaise playerName amount game `shouldBe` Just expectedErr
      it
        "should be able to raise all in when chip count is less than minimum raise amount" $ do
        let game =
              (street .~ PreFlop) . (players .~ playerFixtures) $
              initialGameState
        let playerName = "player3"
        let amount = 300
        canRaise playerName amount game `shouldBe` Nothing
      it
        "should return InvalidActionForStreet InvalidMoveErr if game stage is PreDeal" $ do
        let preDealGame =
              (street .~ PreDeal) . (players .~ playerFixtures2) $
              initialGameState
        let playerName = "player3"
        let amount = 100
        let expectedErr = InvalidActionForStreet
        canRaise playerName amount preDealGame `shouldBe` Just expectedErr
      it
        "should return InvalidActionForStreet InvalidMoveErr if game stage is Showdown" $ do
        let showdownGame =
              (street .~ Showdown) . (players .~ playerFixtures2) $
              initialGameState
        let playerName = "player3"
        let amount = 100
        let expectedErr = InvalidActionForStreet
        canRaise playerName amount showdownGame `shouldBe` Just expectedErr
    describe "canCheck" $ do
      it
        "should return CannotCheckShouldCallRaiseOrFold InvalidMoveErr if maxBet is greater than zero" $ do
        let game =
              (street .~ PreFlop) . (players .~ playerFixtures) $
              initialGameState
        let playerName = "player3"
        let expectedErr = CannotCheckShouldCallRaiseOrFold
        canCheck playerName game `shouldBe` Just expectedErr
      it
        "should return CannotCheckShouldCallRaiseOrFold InvalidMoveErr if maxBet is greater than zero" $ do
        let game =
              (street .~ PreFlop) . (players .~ playerFixtures) $
              initialGameState
        let playerName = "player3"
        let expectedErr = CannotCheckShouldCallRaiseOrFold
        canCheck playerName game `shouldBe` Just expectedErr
      it
        "should return InvalidActionForStreet InvalidMoveErr if game stage is PreDeal" $ do
        let preDealGame =
              (street .~ PreDeal) . (players .~ playerFixtures2) $
              initialGameState
        let playerName = "player3"
        let amount = 100
        let expectedErr = InvalidActionForStreet
        canCheck playerName preDealGame `shouldBe` Just expectedErr
      it
        "should return InvalidActionForStreet InvalidMoveErr if game stage is Showdown" $ do
        let showdownGame =
              (street .~ Showdown) . (players .~ playerFixtures2) $
              initialGameState
        let playerName = "player3"
        let amount = 100
        let expectedErr = InvalidActionForStreet
        canCheck playerName showdownGame `shouldBe` Just expectedErr
    describe "canCall" $ do
      it
        "should return InvalidActionForStreet InvalidMoveErr if game stage is PreDeal" $ do
        let preDealGame =
              (street .~ PreDeal) . (players .~ playerFixtures2) $
              initialGameState
        let playerName = "player3"
        let amount = 100
        let expectedErr = InvalidActionForStreet
        canCall playerName preDealGame `shouldBe` Just expectedErr
      it
        "should return InvalidActionForStreet InvalidMoveErr if game stage is Showdown" $ do
        let showdownGame =
              (street .~ Showdown) . (players .~ playerFixtures2) $
              initialGameState
        let playerName = "player3"
        let amount = 100
        let expectedErr = InvalidActionForStreet
        canCall playerName showdownGame `shouldBe` Just expectedErr
      it
        "should return CannotCallZeroAmountCheckOrBetInstead InvalidMoveErr if game stage is not Preflop" $ do
        let game =
              (street .~ Flop) . (players .~ playerFixtures2) $ initialGameState
        let playerName = "player5"
        let expectedErr = CannotCallZeroAmountCheckOrBetInstead
        canCall playerName game `shouldBe` Just expectedErr
      it "should not return error if call bigBlind during Preflop" $ do
        let game =
              (street .~ PreFlop) . (players .~ playerFixtures2) $
              initialGameState
        let playerName = "player5"
        let expectedErr = CannotCallZeroAmountCheckOrBetInstead
        canCall playerName game `shouldBe` Nothing
    describe "canFold" $ do
      it
        "should return InvalidActionForStreet InvalidMoveErr if game stage is PreDeal" $ do
        let preDealGame =
              (street .~ PreDeal) . (players .~ playerFixtures2) $
              initialGameState
        let playerName = "player3"
        let amount = 100
        let expectedErr = InvalidActionForStreet
        canFold playerName preDealGame `shouldBe` Just expectedErr
      it
        "should return InvalidActionForStreet InvalidMoveErr if game stage is Showdown" $ do
        let showdownGame =
              (street .~ Showdown) . (players .~ playerFixtures2) $
              initialGameState
        let playerName = "player3"
        let amount = 100
        let expectedErr = InvalidActionForStreet
        canFold playerName showdownGame `shouldBe` Just expectedErr
