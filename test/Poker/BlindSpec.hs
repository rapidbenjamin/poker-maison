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
import Poker.Betting
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
            }
        , Player
            { _pockets = []
            , _chips = 2000
            , _bet = 0
            , _playerState = None
            , _playerName = "player2"
            , _committed = 0
            }
        ]
    , _maxPlayers = 5
    , _community = []
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
            }
        , Player
            { _pockets = []
            , _chips = 2000
            , _bet = 50
            , _playerState = None
            , _playerName = "player2"
            , _committed = 50
            }
        ]
    , _maxPlayers = 5
    , _community = []
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
            }
        , Player
            { _pockets = []
            , _chips = 2000
            , _bet = 0
            , _playerState = None
            , _playerName = "player2"
            , _committed = 0
            }
        , Player
            { _pockets = []
            , _chips = 2000
            , _bet = 0
            , _playerState = None
            , _playerName = "player3"
            , _committed = 0
            }
        ]
    , _maxPlayers = 5
    , _community = []
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
            }
        , Player
            { _pockets = []
            , _chips = 2000
            , _bet = 25
            , _playerState = In
            , _playerName = "player2"
            , _committed = 25
            }
        , Player
            { _pockets = []
            , _chips = 2000
            , _bet = 50
            , _playerState = In
            , _playerName = "player3"
            , _committed = 50
            }
        ]
    , _maxPlayers = 5
    , _community = []
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
        let players = _players twoPlayerGame
        let reqBs = getRequiredBlinds twoPlayerGame
        let smallBlindValue = 25
        haveRequiredBlindsBeenPosted reqBs players smallBlindValue `shouldBe`
          False
      it
        "should return True when all players have posted blinds in 2 player game" $ do
        let players = _players twoPlayerGameAllBlindsPosted
        let requiredBlindValues = getRequiredBlinds twoPlayerGameAllBlindsPosted
        let smallBlindValue = 25
        haveRequiredBlindsBeenPosted
          requiredBlindValues
          (_players twoPlayerGameAllBlindsPosted)
          smallBlindValue `shouldBe`
          True
      it
        "should return False when not all players have posted blinds in 3 player game" $ do
        let requiredBlindValues = getRequiredBlinds threePlayerGame
        let smallBlindValue = 25
        haveRequiredBlindsBeenPosted
          requiredBlindValues
          threePlayers
          smallBlindValue `shouldBe`
          False
      it
        "should  return True when all players have posted blinds in 3 player game" $ do
        let requiredBlindValues = getRequiredBlinds threePlayerGame
        let smallBlindValue = 25
        haveRequiredBlindsBeenPosted
          requiredBlindValues
          (_players threePlayerGameAllBlindsPosted)
          smallBlindValue `shouldBe`
          True
    describe "progressBlindBetting" $ do
      it "should return next same game if not all blinds posted" $ do
        let Game {..} = progressBlindBetting threePlayerGame
        _street `shouldBe` PreDeal
      it "should progress game to next stage if all blinds posted" $ do
        let Game {..} = progressBlindBetting threePlayerGameAllBlindsPosted
        let chipsCommittedReset =
              (sum $ (\Player {..} -> _committed) <$> _players) == 0
        _street `shouldBe` PreFlop
        chipsCommittedReset `shouldBe` True
      it
        "should update state of players that dont require blinds to In when move to PreFlop stage " $ do
        let Game {..} = progressBlindBetting threePlayerGameAllBlindsPosted
        let playerStates = (\Player {..} -> _playerState) <$> _players
        all (== In) playerStates `shouldBe` True
