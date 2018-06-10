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
    _community <- resize y arbitrary
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
    _playerName <- suchThat arbitrary (not . null . show)
    _pockets <- suchThat arbitrary (\cards -> (null cards || length cards == 2))
    _playerState <-
      suchThat arbitrary (\s -> (s == None && (_committed > 0)) || s /= None)
    return Player {..}

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary
  shrink xs = T.pack <$> shrink (T.unpack xs)

main :: IO ()
main =
  hspec $
  describe "ActionValidation" $ do
    describe "getSmallBlindPosition" $ do
      it "returns correct small blind position in three player game" $ do
        let dealerPos = 0
        getSmallBlindPosition ["Player1", "Player2", "Player3"] dealerPos `shouldBe`
          1
      it "returns correct small blind position in two player game" $ do
        let dealerPos = 0
        getSmallBlindPosition ["Player1", "Player2"] dealerPos `shouldBe` 0
    describe "IsPlayerActingOutOfTurn" $ do
      let testPlayers =
            getPlayer <$> ["Player1", "Player2", "Player3"] <*> replicate 3 100
      let game = players .~ testPlayers $ initialGameState
      it
        "returns Just OutOfTurn Error if given player is not in current position to act" $ do
        let expectedErr =
              Just $
              InvalidMove "Player2" $
              OutOfTurn $ CurrentPlayerToActErr "Player1"
        isPlayerActingOutOfTurn game "Player2" `shouldBe` expectedErr
      let testPlayers =
            getPlayer <$> ["Player1", "Player2", "Player3"] <*> replicate 3 100
      let game = players .~ testPlayers $ initialGameState
      it
        "returns Just OutOfTurn Error if given player is not in current position to act" $ do
        let expectedErr =
              Just $
              InvalidMove "Player2" $
              OutOfTurn $ CurrentPlayerToActErr "Player1"
        isPlayerActingOutOfTurn game "Player2" `shouldBe` expectedErr
      it "returns Nothing if playerName is current player to act" $ do
        isPlayerActingOutOfTurn game "Player1" `shouldBe` Nothing
      it
        "returns Just MissingPlayer Error if no player with playerName is sat at table" $ do
        let expectedErr =
              Just $
              InvalidMove "MissingPlayer" $
              OutOfTurn $ CurrentPlayerToActErr "Player1"
        isPlayerActingOutOfTurn game "MissingPlayer" `shouldBe` expectedErr
    describe "blindRequiredByPlayer" $ do
      it "returns Just Small if player position is dealer + 1 for three players" $ do
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
        blindRequiredByPlayer game "Player1" `shouldBe` Just Big
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
        let player = (\Player {..} -> _playerName == playerName) `find` _players
        case player of
          Just Player {..} -> do
            let result = blindRequiredByPlayer game playerName
            (_playerState == None && result == Just Big) || _playerState /= None
          Nothing -> True
