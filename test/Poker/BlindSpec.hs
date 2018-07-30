{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.BlindSpec where

import Control.Lens
import Control.Lens
import Data.Either
import Data.List
import Data.List.Lens
import Data.Text (Text)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck hiding (Big, Small)

import Arbitrary ()
import Data.Aeson
import Poker.ActionValidation
import Poker.Game.Actions
import Poker.Game.Blinds
import Poker.Game.Utils
import Poker.Poker
import Poker.Types

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
    , _minBuyInChips = 1000
    , _maxBuyInChips = 3000
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
            , _actedThisTurn = True
            }
        , Player
            { _pockets = []
            , _chips = 2000
            , _bet = 50
            , _playerState = In
            , _playerName = "player2"
            , _committed = 50
            , _actedThisTurn = True
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
    , _minBuyInChips = 1000
    , _maxBuyInChips = 3000
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
    , _minBuyInChips = 1000
    , _maxBuyInChips = 3000
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
    , _minBuyInChips = 1000
    , _maxBuyInChips = 3000
    , _winners = NoWinners
    , _maxBet = 0
    , _dealer = 0
    , _currentPosToAct = 1
    }

twoPlayerNames = getGamePlayerNames twoPlayerGame

twoPlayers = _players twoPlayerGame

threePlayerNames = getGamePlayerNames threePlayerGame

threePlayers = _players threePlayerGame

spec =
  describe "Poker.Blinds" $ do
    describe "blind required by player" $
      it "should return correct blind" $
      blindRequiredByPlayer twoPlayerGame "player2" `shouldBe` Just Big
    describe "getSmallBlindPosition" $ do
      it "small blind position should be correct for a two player game" $ do
        let dealerPos = 0
        getSmallBlindPosition twoPlayerNames dealerPos `shouldBe` (0 :: Int)
      it "small blind position should be correct for a three player game" $ do
        let dealerPos = 2
        getSmallBlindPosition threePlayerNames dealerPos `shouldBe` (0 :: Int)
    describe "getRequiredBlinds" $ do
      it "should return correct blinds for two player game" $
        getRequiredBlinds twoPlayerGame `shouldBe` [Just Small, Just Big]
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
        "should return False when not all players have posted blinds in 2 player game" $
        haveRequiredBlindsBeenPosted twoPlayerGame `shouldBe` False
      it
        "should return True when all players have posted blinds in 2 player game" $
        haveRequiredBlindsBeenPosted twoPlayerGameAllBlindsPosted `shouldBe`
        True
      it
        "should return False when not all players have posted blinds in 3 player game" $
        haveRequiredBlindsBeenPosted threePlayerGame `shouldBe` False
      it
        "should  return True when all players have posted blinds in 3 player game" $
        haveRequiredBlindsBeenPosted threePlayerGameAllBlindsPosted `shouldBe`
        True
    describe "updatePlayersInHand" $ do
      it
        "should set players that are not in blind position to In for three players" $ do
        let newGame = updatePlayersInHand threePlayerGameAllBlindsPosted
        let playerStates = (\Player {..} -> _playerState) <$> _players newGame
        playerStates `shouldBe` [In, In, In]
      it
        "should return correct player states for two players when all blinds posted" $ do
        let newGame = updatePlayersInHand twoPlayerGameAllBlindsPosted
        let playerStates = (\Player {..} -> _playerState) <$> _players newGame
        playerStates `shouldBe` [In, In]
      it
        "should return correct player states for two players when not all blinds posted" $ do
        let newGame = updatePlayersInHand twoPlayerGame
        let playerStates = (\Player {..} -> _playerState) <$> _players newGame
        playerStates `shouldBe` [In, None]
