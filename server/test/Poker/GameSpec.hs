{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Poker.GameSpec where

import Control.Lens
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Either
import Data.List
import Data.List.Lens
import Data.List.Split
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import GHC.Generics
import System.IO.Unsafe
import Test.Hspec


import Poker.ActionValidation
import Poker.Game.Blinds
import Poker.Game.Game
import Poker.Game.Utils
import Poker.Poker
import Poker.Types

initialGameState' = initialGameState initialDeck

player1 =
  Player
    { _pockets =
        PocketCards
          [ Card {rank = Three, suit = Diamonds}
          , Card {rank = Four, suit = Spades}
          ]
    , _chips = 2000
    , _bet = 50
    , _playerState = In
    , _playerName = "player1"
    , _committed = 50
    , _actedThisTurn = True
    }

player2 =
  Player
    { _pockets =
        PocketCards
          [Card {rank = Three, suit = Clubs}, Card {rank = Four, suit = Hearts}]
    , _chips = 2000
    , _bet = 0
    , _playerState = In
    , _playerName = "player2"
    , _committed = 50
    , _actedThisTurn = False
    }

player3 =
  Player
    { _pockets = PocketCards []
    , _chips = 2000
    , _bet = 0
    , _playerState = In
    , _playerName = "player3"
    , _committed = 50
    , _actedThisTurn = False
    }

player4 =
  Player
    { _pockets = PocketCards []
    , _chips = 2000
    , _bet = 0
    , _playerState = None
    , _playerName = "player4"
    , _committed = 0
    , _actedThisTurn = False
    }

player5 =
  Player
    { _pockets =
        PocketCards
          [ Card {rank = King, suit = Diamonds}
          , Card {rank = Four, suit = Spades}
          ]
    , _chips = 2000
    , _bet = 50
    , _playerState = In
    , _playerName = "player1"
    , _committed = 50
    , _actedThisTurn = True
    }

player6 =
  Player
    { _pockets = PocketCards []
    , _chips = 2000
    , _bet = 0
    , _playerState = None
    , _playerName = "player6"
    , _committed = 0
    , _actedThisTurn = False
    }

initPlayers = [player1, player2, player3]

spec = do
  describe "dealToPlayers" $ do
    it "should deal correct number of cards" $ do
      let (_, newPlayers) = dealToPlayers initialDeck [player1, player3]
      (all
         (\Player {..} ->
            if _playerState == In
              then length (unPocketCards _pockets) == 2
              else null $ unPocketCards _pockets)
         newPlayers) `shouldBe`
        True
  describe "haveAllPlayersActed" $ do
    it
      "should return True when all players have acted during PreDeal for Three Players" $ do
      let game =
            (street .~ PreDeal) . (maxBet .~ 0) .
            (players .~
             [ ((playerState .~ In) . (actedThisTurn .~ False) . (bet .~ 0) .
                (committed .~ 0))
                 player1
             , ((playerState .~ In) . (actedThisTurn .~ True) . (bet .~ 0) .
                (committed .~ 25))
                 player2
             , ((playerState .~ In) . (actedThisTurn .~ True) . (bet .~ 0) .
                (committed .~ 50))
                 player6
             ]) $
            initialGameState'
      haveAllPlayersActed game `shouldBe` True
    it
      "should return False when not all players acted during PreDeal for Three Players" $ do
      let unfinishedBlindsGame =
            (street .~ PreDeal) . (players .~ [player1, player4, player6]) $
            initialGameState'
      haveAllPlayersActed unfinishedBlindsGame `shouldBe` False
    it
      "should return True when all players have acted during preFlop for Two Players" $ do
      let game =
            (street .~ PreFlop) . (maxBet .~ 0) .
            (players .~
             [ ((playerState .~ In) . (actedThisTurn .~ True) . (bet .~ 0))
                 player1
             , ((playerState .~ In) . (actedThisTurn .~ True) . (bet .~ 0))
                 player2
             ]) $
            initialGameState'
      haveAllPlayersActed game `shouldBe` True
    it
      "should return False when not all players acted during PreFlop for Two Players" $ do
      let unfinishedBlindsGame =
            (street .~ PreDeal) . (players .~ [player1, player4]) $
            initialGameState'
      haveAllPlayersActed unfinishedBlindsGame `shouldBe` False
  describe "allButOneFolded" $ do
    it "should return True when all but one player " $ do
      let game =
            (street .~ PreFlop) .
            (players .~ [((playerState .~ Folded) player1), player2]) $
            initialGameState'
      allButOneFolded game `shouldBe` True
    it "should return False when not all players acted" $ do
      let unfinishedBlindsGame =
            (street .~ PreFlop) . (players .~ [player1, player3]) $
            initialGameState'
      allButOneFolded unfinishedBlindsGame `shouldBe` False
    it "should always return False for PreDeal (blinds) stage" $ do
      let unfinishedBlindsGame =
            (street .~ PreDeal) .
            (players .~ [((playerState .~ Folded) player1), player2]) $
            initialGameState'
      allButOneFolded unfinishedBlindsGame `shouldBe` False
  describe "progressToPreFlop" $ do
    let preDealGame =
          (street .~ PreDeal) . (maxBet .~ 50) . (pot .~ 75) .
          (deck .~ initialDeck) .
          (players .~
           [ (((chips .~ 1000) . (committed .~ 25) . (bet .~ 25)) player5)
           , (((chips .~ 1000) . (committed .~ 50) . (bet .~ 50)) player2)
           ]) $
          initialGameState'
    let preFlopGame = progressToPreFlop preDealGame
    it "should update street to PreFlop" $ preFlopGame ^. street `shouldBe`
      PreFlop
    it "should not reset any player bet" $ do
      let playerBets = (^. bet) <$> _players preFlopGame
      playerBets `shouldBe` [25, 50]
  describe "progressToFlop" $ do
    let preFlopGame =
          (street .~ Flop) . (maxBet .~ 1000) . (pot .~ 1000) .
          (deck .~ initialDeck) .
          (players .~ [((chips .~ 1000) player5), ((chips .~ 1000) player2)]) $
          initialGameState'
    let flopGame = progressToFlop preFlopGame
    it "should update street to Turn" $ flopGame ^. street `shouldBe` Flop
    it "should reset maxBet" $ flopGame ^. maxBet `shouldBe` 0
    it "should reset all player bets" $ do
      let playerBets = (^. bet) <$> (_players flopGame)
      playerBets `shouldBe` [0, 0]
  describe "progressToTurn" $ do
    let flopGame =
          (street .~ Flop) . (maxBet .~ 1000) . (pot .~ 1000) .
          (deck .~ initialDeck) .
          (players .~ [((chips .~ 1000) player5), ((chips .~ 1000) player2)]) $
          initialGameState'
    let turnGame = progressToTurn flopGame
    it "should update street to Turn" $ turnGame ^. street `shouldBe` Turn
    it "should reset maxBet" $ turnGame ^. maxBet `shouldBe` 0
    it "should reset all player bets" $ do
      let playerBets = (^. bet) <$> _players turnGame
      playerBets `shouldBe` [0, 0]
  describe "progressToRiver" $ do
    let turnGame =
          (street .~ Turn) . (maxBet .~ 1000) . (pot .~ 1000) .
          (deck .~ initialDeck) .
          (players .~ [((chips .~ 1000) player5), ((chips .~ 1000) player2)]) $
          initialGameState'
    let riverGame = progressToRiver turnGame
    it "should update street to River" $ riverGame ^. street `shouldBe` River
    it "should reset maxBet" $ riverGame ^. maxBet `shouldBe` 0
    it "should reset all player bets" $ do
      let turnGame =
            (street .~ Turn) . (maxBet .~ 1000) . (pot .~ 1000) .
            (deck .~ initialDeck) .
            (players .~ [((chips .~ 1000) player5), ((chips .~ 1000) player2)]) $
            initialGameState'
      let riverGame = progressToRiver turnGame
      let playerBets = (^. bet) <$> _players riverGame
      playerBets `shouldBe` [0, 0]
  describe "progressToShowdown" $ do
    let riverGame =
          (street .~ River) . (pot .~ 1000) . (deck .~ initialDeck) .
          (players .~ [((chips .~ 1000) player5), ((chips .~ 1000) player2)]) $
          initialGameState'
    let showdownGame = progressToShowdown riverGame
    it "should update street to Turn" $ showdownGame ^. street `shouldBe`
      Showdown
    it "should award pot chips to winner of hand" $ do
      let playerChipCounts = (^. chips) <$> (_players showdownGame)
      playerChipCounts `shouldBe` [2000, 1000]
    it "should split pot if more than one player wins given pot" $ do
      let riverGame =
            (street .~ River) . (pot .~ 1000) . (deck .~ initialDeck) .
            (players .~ [((chips .~ 1000) player1), ((chips .~ 1000) player2)]) $
            initialGameState'
      let showdownGame = progressToShowdown riverGame
      let playerChipCounts =
            (\Player {..} -> _chips) <$> (_players showdownGame)
      playerChipCounts `shouldBe` [1500, 1500]
  describe "getNextHand" $ do
    let showdownGame =
          (street .~ Showdown) . (maxBet .~ 1000) . (pot .~ 1000) .
          (deck .~ initialDeck) .
          (dealer .~ 1) .
          (players .~ [((chips .~ 1000) player5), ((chips .~ 1000) player2)]) $
          initialGameState'
    let preDealGame = getNextHand showdownGame $ Deck []
    it "should update street to PreDeal" $ preDealGame ^. street `shouldBe`
      PreDeal
    it "should reset maxBet" $ preDealGame ^. maxBet `shouldBe` 0
    it "should reset all player bets" $ do
      let playerBets = (\Player {..} -> _bet) <$> _players preDealGame
      playerBets `shouldBe` [0, 0]
    it "should increment dealer position" $ preDealGame ^. dealer `shouldBe` 0
  describe "isEveryoneAllIn" $ do
    it "should return False for two player game if no one all in" $ do
      let preFlopGame' =
            (street .~ PreFlop) . (pot .~ 1000) . (deck .~ initialDeck) .
            (players .~
             [ (((playerState .~ In) . (actedThisTurn .~ False)) player1)
             , (((playerState .~ In) . (actedThisTurn .~ True)) player3)
             ]) $
            initialGameState'
      isEveryoneAllIn preFlopGame' `shouldBe` False
    it
      "should return True for two player game if a player has called the other player all in" $ do
      let preFlopGame =
            (street .~ PreFlop) . (maxBet .~ 1950) . (pot .~ 4000) .
            (deck .~ initialDeck) .
            (players .~
             [ ((actedThisTurn .~ True) . (playerState .~ In) . (bet .~ 1950) .
                (chips .~ 0) .
                (committed .~ 2000))
                 player1
             , ((playerState .~ In) . (actedThisTurn .~ True) . (bet .~ 1950) .
                (committed .~ 2000) .
                (chips .~ 3000))
                 player3
             ]) $
            initialGameState'
      isEveryoneAllIn preFlopGame `shouldBe` True
    it
      "should return False for two player game if a player bet all in and the other has folded" $ do
      let preFlopGame =
            (street .~ PreFlop) . (maxBet .~ 1950) . (pot .~ 4000) .
            (deck .~ initialDeck) .
            (players .~
             [ ((actedThisTurn .~ True) . (playerState .~ In) . (bet .~ 1950) .
                (chips .~ 0) .
                (committed .~ 2000))
                 player1
             , ((playerState .~ Folded) . (actedThisTurn .~ True) .
                (bet .~ 1950) .
                (committed .~ 2000) .
                (chips .~ 3000))
                 player3
             ]) $
            initialGameState'
      isEveryoneAllIn preFlopGame `shouldBe` False
    it
      "should return False for three player game if only one short stacked player all in" $ do
      let preFlopGame =
            (street .~ PreFlop) . (maxBet .~ 1950) . (pot .~ 1000) .
            (deck .~ initialDeck) .
            (players .~
             [ ((actedThisTurn .~ True) . (playerState .~ In) . (bet .~ 1950) .
                (chips .~ 0) .
                (committed .~ 2000))
                 player1
             , ((playerState .~ In) . (actedThisTurn .~ True) . (bet .~ 1950) .
                (committed .~ 2000) .
                (chips .~ 3000))
                 player3
             , ((playerState .~ In) . (actedThisTurn .~ True) . (bet .~ 1950) .
                (committed .~ 2000) .
                (chips .~ 3000))
                 player3
             ]) $
            initialGameState'
      isEveryoneAllIn preFlopGame `shouldBe` False
    it "should return True for three player game if everyone is all in" $ do
      let flopGame =
            (street .~ Flop) . (maxBet .~ 2000) . (pot .~ 10000) .
            (deck .~ initialDeck) .
            (players .~
             [ ((actedThisTurn .~ True) . (playerState .~ In) . (bet .~ 0) .
                (chips .~ 0) .
                (committed .~ 2000))
                 player1
             , ((actedThisTurn .~ True) . (playerState .~ In) . (bet .~ 2000) .
                (committed .~ 4000) .
                (chips .~ 0))
                 player3
             , ((playerState .~ In) . (actedThisTurn .~ True) . (bet .~ 2000) .
                (committed .~ 4000) .
                (chips .~ 0))
                 player3
             ]) $
            initialGameState'
      isEveryoneAllIn flopGame `shouldBe` True
    it "should return False for four player game if one player not all in" $ do
      let flopGame =
            (street .~ Flop) . (maxBet .~ 2000) . (pot .~ 10000) .
            (deck .~ initialDeck) .
            (players .~
             [ ((actedThisTurn .~ True) . (playerState .~ In) . (bet .~ 0) .
                (chips .~ 0) .
                (committed .~ 2000))
                 player1
             , ((actedThisTurn .~ True) . (playerState .~ In) . (bet .~ 2000) .
                (committed .~ 4000) .
                (chips .~ 0))
                 player3
             , ((playerState .~ In) . (actedThisTurn .~ True) . (bet .~ 2000) .
                (committed .~ 4000) .
                (chips .~ 0))
                 player3
             , ((playerState .~ In) . (actedThisTurn .~ False) . (bet .~ 0) .
                (committed .~ 2000) .
                (chips .~ 800))
                 player3
             ]) $
            initialGameState'
      isEveryoneAllIn flopGame `shouldBe` False
  describe "doesPlayerHaveToAct" $ do
    it "should return True for an active player in position" $ do
      let game =
            (street .~ Flop) . (dealer .~ 0) .
            (players .~ [((chips .~ 1000) player5), ((chips .~ 1000) player2)]) $
            initialGameState'
      doesPlayerHaveToAct (_playerName player2) game `shouldBe` True
      doesPlayerHaveToAct (_playerName player5) game `shouldBe` False
    it "should return False for non-active players" $ do
      let game =
            (street .~ Flop) . (dealer .~ 0) .
            (players .~
             [ ((chips .~ 1000) player5)
             , ((playerState .~ Folded) player4)
             , ((playerState .~ None) player3)
             , ((chips .~ 1000) player2)
             ]) $
            initialGameState'
      doesPlayerHaveToAct (_playerName player3) game `shouldBe` False
      doesPlayerHaveToAct (_playerName player4) game `shouldBe` False
    describe "Heads Up Game" $ do
      describe "PreDeal" $ do
        describe "When 0 players sat in" $ do
          let game' =
                (street .~ PreDeal) . (maxBet .~ 0) . (pot .~ 0) .
                (deck .~ initialDeck) .
                (currentPosToAct .~ 1) .
                (dealer .~ 0) .
                (players .~
                 [ ((actedThisTurn .~ False) . (playerState .~ None) .
                    (bet .~ 0) .
                    (chips .~ 2000) .
                    (committed .~ 0) .
                    (bet .~ 0))
                     player1
                 , ((actedThisTurn .~ False) . (playerState .~ None) .
                    (bet .~ 0) .
                    (committed .~ 0) .
                    (bet .~ 0) .
                    (chips .~ 2000))
                     player2
                 ]) $
                initialGameState'
          it "No player should have to act first" $ do
            doesPlayerHaveToAct (_playerName player1) game' `shouldBe` False
            doesPlayerHaveToAct (_playerName player2) game' `shouldBe` False
        describe "When 1 player is sat in" $ do
          let game' =
                (street .~ PreDeal) .
                (players .~
                 [ ((actedThisTurn .~ False) . (playerState .~ None) .
                    (bet .~ 0) .
                    (chips .~ 2000) .
                    (committed .~ 0))
                     player1
                 , ((actedThisTurn .~ False) . (playerState .~ In) . (bet .~ 0) .
                    (committed .~ 0) .
                    (chips .~ 2000))
                     player2
                 ]) $
                initialGameState'
          it "No player should have to act first" $ do
            doesPlayerHaveToAct (_playerName player1) game' `shouldBe` False
            doesPlayerHaveToAct (_playerName player2) game' `shouldBe` False
        describe
          "When 2 players are both sat in but no one has posted a blind yet" $ do
          let game' =
                (street .~ PreDeal) . (maxBet .~ 0) . (pot .~ 0) .
                (deck .~ initialDeck) .
                (currentPosToAct .~ 1) .
                (dealer .~ 0) .
                (players .~
                 [ ((actedThisTurn .~ False) . (playerState .~ In) . (bet .~ 0) .
                    (chips .~ 2000) .
                    (committed .~ 0))
                     player1
                 , ((actedThisTurn .~ False) . (playerState .~ In) . (bet .~ 0) .
                    (committed .~ 0) .
                    (chips .~ 2000))
                     player2
                 ]) $
                initialGameState'
          it "Player1 should not have to act" $
            doesPlayerHaveToAct (_playerName player1) game' `shouldBe`
            False
          it "Player2 should not have to act" $
            doesPlayerHaveToAct (_playerName player2) game' `shouldBe`
            False
        describe "When one player has already posted blinds" $ do
          let game' =
                (street .~ PreDeal) . (maxBet .~ 25) . (pot .~ 25) .
                (deck .~ initialDeck) .
                (currentPosToAct .~ 1) .
                (dealer .~ 0) .
                (players .~
                 [ ((actedThisTurn .~ True) . (playerState .~ In) . (bet .~ 0) .
                    (chips .~ 2000) .
                    (committed .~ 25))
                     player1
                 , ((actedThisTurn .~ False) . (playerState .~ In) . (bet .~ 0) .
                    (committed .~ 0) .
                    (chips .~ 1950))
                     player2
                 ]) $
                initialGameState'
          it "Player1 should not have to act" $
            doesPlayerHaveToAct (_playerName player1) game' `shouldBe`
            False
          it "Player2 should have to act" $
            doesPlayerHaveToAct (_playerName player2) game' `shouldBe`
            True
        describe "PreFlop" $ do
          describe "First Turn" $ do
            let game' =
                  (street .~ PreFlop) . (maxBet .~ 50) . (deck .~ initialDeck) .
                  (smallBlind .~ 25) .
                  (smallBlind .~ 50) .
                  (pot .~ 100) .
                  (currentPosToAct .~ 1) .
                  (dealer .~ 1) .
                  (players .~
                   [ ((actedThisTurn .~ False) . (playerState .~ In) .
                      (bet .~ 50) .
                      (committed .~ 50) .
                      (chips .~ 1950))
                       player1
                   , ((actedThisTurn .~ False) . (playerState .~ In) .
                      (bet .~ 50) .
                      (chips .~ 1975) .
                      (committed .~ 50))
                       player2
                   ]) $
                  initialGameState'
            it "Player1 should not have to act" $
              doesPlayerHaveToAct (_playerName player1) game' `shouldBe`
              False
            it "Player2 should have to act" $
              doesPlayerHaveToAct (_playerName player2) game' `shouldBe`
              True
          describe "Second Turn" $ do
            let game' =
                  (street .~ PreFlop) . (maxBet .~ 50) . (deck .~ initialDeck) .
                  (smallBlind .~ 25) .
                  (smallBlind .~ 50) .
                  (pot .~ 100) .
                  (currentPosToAct .~ 1) .
                  (dealer .~ 1) .
                  (players .~
                   [ ((actedThisTurn .~ True) . (playerState .~ In) .
                      (bet .~ 50) .
                      (committed .~ 50) .
                      (chips .~ 1950))
                       player1
                   , ((actedThisTurn .~ False) . (playerState .~ In) .
                      (bet .~ 50) .
                      (chips .~ 1975) .
                      (committed .~ 50))
                       player2
                   ]) $
                  initialGameState'
            it "Player1 should not have to act" $
              doesPlayerHaveToAct (_playerName player1) game' `shouldBe`
              False
            it "Player2 should have to act" $
              doesPlayerHaveToAct (_playerName player2) game' `shouldBe`
              True
          describe "Third Turn" $ do
            let game' =
                  (street .~ PreFlop) . (maxBet .~ 50) . (pot .~ 0) .
                  (deck .~ initialDeck) .
                  (currentPosToAct .~ 0) .
                  (dealer .~ 1) .
                  (players .~
                   [ ((playerState .~ In) . (bet .~ 0) . (committed .~ 50) .
                      (chips .~ 1950))
                       player1
                   , ((actedThisTurn .~ True) . (playerState .~ In) .
                      (bet .~ 25) .
                      (chips .~ 1950) .
                      (committed .~ 50) .
                      (actedThisTurn .~ False))
                       player2
                   ]) $
                  initialGameState'
            it "Player1 should have to act" $
              doesPlayerHaveToAct (_playerName player1) game' `shouldBe`
              True
            it "Player2 should not have to act" $
              doesPlayerHaveToAct (_playerName player2) game' `shouldBe`
              False
        describe "Flop" $ do
          describe "First turn" $ do
            let game' =
                  (street .~ Flop) . (maxBet .~ 0) . (pot .~ 100) .
                  (deck .~ initialDeck) .
                  (currentPosToAct .~ 1) .
                  (dealer .~ 0) .
                  (players .~
                   [ ((actedThisTurn .~ False) . (playerState .~ In) .
                      (bet .~ 0) .
                      (chips .~ 2000) .
                      (committed .~ 50))
                       player1
                   , ((actedThisTurn .~ False) . (playerState .~ In) .
                      (bet .~ 0) .
                      (committed .~ 50) .
                      (chips .~ 2000))
                       player2
                   ]) $
                  initialGameState'
            it "Player1 should not have to act" $
              doesPlayerHaveToAct (_playerName player1) game' `shouldBe`
              False
            it "Player2 should have to act" $
              doesPlayerHaveToAct (_playerName player2) game' `shouldBe`
              True
          describe "Second turn" $ do
            let game' =
                  (street .~ Flop) . (maxBet .~ 0) . (pot .~ 100) .
                  (deck .~ initialDeck) .
                  (currentPosToAct .~ 0) .
                  (dealer .~ 0) .
                  (players .~
                   [ ((actedThisTurn .~ False) . (playerState .~ In) .
                      (bet .~ 0) .
                      (chips .~ 2000) .
                      (committed .~ 50))
                       player1
                   , ((actedThisTurn .~ True) . (playerState .~ In) . (bet .~ 0) .
                      (committed .~ 50) .
                      (chips .~ 2000))
                       player2
                   ]) $
                  initialGameState'
            it "Player1 should have to act" $
              doesPlayerHaveToAct (_playerName player1) game' `shouldBe`
              True
            it "Player2 should not have to act" $
              doesPlayerHaveToAct (_playerName player2) game' `shouldBe`
              False
