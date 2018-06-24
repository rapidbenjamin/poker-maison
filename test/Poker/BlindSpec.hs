{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Lens
import Data.List
import Data.Text (Text)
import Test.Hspec
import Test.QuickCheck hiding (Big, Small)

import Poker
import Poker.ActionValidation
import Poker.Actions
import Poker.Blinds
import Poker.Types
import Poker.Utils

import Control.Lens
import Control.Monad.State hiding (state)
import Data.List.Lens
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import GHC.Generics
import GHC.Generics
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Gen
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
    let _winners = NoWinners
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

twoPlayerGame =
  Game
    { _players =
        [ Player
            { _pockets = []
            , _chips = 1950
            , _bet = 50
            , _playerState = In
            , _playerName = "player1"
            , _committed = 50
            , _actedThisTurn = False
            }
        , Player
            { _pockets = []
            , _chips = 2000
            , _bet = 0
            , _playerState = None
            , _playerName = "player2"
            , _committed = 0
            , _actedThisTurn = False
            }
        ]
    , _maxPlayers = 5
    , _board = []
    , _waitlist = []
    , _deck = []
    , _smallBlind = 25
    , _bigBlind = 50
    , _street = PreDeal
    , _pot = 0
    , _maxBet = 0
    , _dealer = 0
    , _currentPosToAct = 1
    , _winners = NoWinners
    }

twoPlayerGameAllBlindsPosted =
  Game
    { _players =
        [ Player
            { _pockets = []
            , _chips = 1950
            , _bet = 25
            , _playerState = In
            , _playerName = "player1"
            , _committed = 25
            , _actedThisTurn = False
            }
        , Player
            { _pockets = []
            , _chips = 2000
            , _bet = 50
            , _playerState = None
            , _playerName = "player2"
            , _committed = 50
            , _actedThisTurn = False
            }
        ]
    , _maxPlayers = 5
    , _board = []
    , _waitlist = []
    , _deck = []
    , _smallBlind = 25
    , _bigBlind = 50
    , _street = PreDeal
    , _pot = 0
    , _winners = NoWinners
    , _maxBet = 0
    , _dealer = 0
    , _currentPosToAct = 1
    }

threePlayerGame =
  Game
    { _players =
        [ Player
            { _pockets = []
            , _chips = 1950
            , _bet = 0
            , _playerState = None
            , _playerName = "player1"
            , _committed = 0
            , _actedThisTurn = False
            }
        , Player
            { _pockets = []
            , _chips = 2000
            , _bet = 0
            , _playerState = None
            , _playerName = "player2"
            , _committed = 0
            , _actedThisTurn = False
            }
        , Player
            { _pockets = []
            , _chips = 2000
            , _bet = 0
            , _playerState = None
            , _playerName = "player3"
            , _committed = 0
            , _actedThisTurn = False
            }
        ]
    , _maxPlayers = 5
    , _board = []
    , _waitlist = []
    , _deck = []
    , _smallBlind = 25
    , _bigBlind = 50
    , _street = PreDeal
    , _pot = 0
    , _winners = NoWinners
    , _maxBet = 0
    , _dealer = 0
    , _currentPosToAct = 1
    }

threePlayerGameAllBlindsPosted =
  Game
    { _players =
        [ Player
            { _pockets = []
            , _chips = 1950
            , _bet = 0
            , _playerState = None
            , _playerName = "player1"
            , _committed = 0
            , _actedThisTurn = False
            }
        , Player
            { _pockets = []
            , _chips = 2000
            , _bet = 25
            , _playerState = In
            , _playerName = "player2"
            , _committed = 25
            , _actedThisTurn = False
            }
        , Player
            { _pockets = []
            , _chips = 2000
            , _bet = 50
            , _playerState = In
            , _playerName = "player3"
            , _committed = 50
            , _actedThisTurn = False
            }
        ]
    , _maxPlayers = 5
    , _board = []
    , _waitlist = []
    , _deck = []
    , _smallBlind = 25
    , _bigBlind = 50
    , _street = PreDeal
    , _pot = 0
    , _winners = NoWinners
    , _maxBet = 0
    , _dealer = 0
    , _currentPosToAct = 1
    }

twoPlayerNames = getGamePlayerNames twoPlayerGame

twoPlayers = _players twoPlayerGame

threePlayerNames = getGamePlayerNames threePlayerGame

threePlayers = _players threePlayerGame

main :: IO ()
main =
  hspec $
  describe "Poker.Blinds" $ do
    describe "blind required by player" $
      it "should return correct blind" $
      blindRequiredByPlayer twoPlayerGame "player2" `shouldBe` Just Big
    describe "getSmallBlindPosition" $
      it "small blind position should be correct for a two player game" $
      getSmallBlindPosition twoPlayerNames 0 `shouldBe` (0 :: Int)
    describe "getRequiredBlinds" $ do
      it "should return correct blinds for two player game" $
        getRequiredBlinds twoPlayerGame `shouldBe` [Just Small, Just Big]
      it "should return correct blinds for three player game" $
        getRequiredBlinds threePlayerGame `shouldBe`
        [Nothing, Just Small, Just Big]
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
    describe "haveRequiredBlindsBeenPosted" $ do
      it
        "should return False when not all players have posted blinds in 2 player game" $ do
        haveRequiredBlindsBeenPosted twoPlayerGame `shouldBe` False
      it
        "should return True when all players have posted blinds in 2 player game" $ do
        haveRequiredBlindsBeenPosted twoPlayerGameAllBlindsPosted `shouldBe`
          True
      it
        "should return False when not all players have posted blinds in 3 player game" $ do
        haveRequiredBlindsBeenPosted threePlayerGame `shouldBe` False
      it
        "should  return True when all players have posted blinds in 3 player game" $ do
        haveRequiredBlindsBeenPosted threePlayerGameAllBlindsPosted `shouldBe`
          True
    describe "updatePlayersInHand" $ do
      it "should set players that are not in blind position to In" $ do
        let newGame = updatePlayersInHand threePlayerGameAllBlindsPosted
        let playerStates = (\Player {..} -> _playerState) <$> (_players newGame)
        playerStates `shouldBe` [In, In, In]
