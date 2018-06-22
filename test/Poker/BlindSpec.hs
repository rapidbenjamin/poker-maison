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
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Gen

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
  describe "Poker.Betting" $ do
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
