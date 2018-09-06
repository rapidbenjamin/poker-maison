{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.ActionValidationSpec where

import Control.Lens
import Data.List
import Data.Text (Text)
import Test.Hspec
import Test.QuickCheck hiding (Big, Small)

import Arbitrary ()
import Control.Lens
import Control.Monad.State hiding (state)
import Data.List.Lens
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Poker.ActionValidation
import Poker.Game.Utils
import Poker.Poker
import Poker.Types

import Poker.Game.Utils

initialGameState' = initialGameState initialDeck

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
    , _playerState = Folded
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

spec = do
  describe "Player Acting in Turn Validation" $ do
    let game =
          (currentPosToAct .~ 0) .
          (street .~ PreFlop) . (players .~ playerFixtures) $
          initialGameState'
    it
      "returns Just OutOfTurn Error if given player is not in current position to act" $ do
      let playerName = "player3"
      let expectedErr =
            Left $
            InvalidMove playerName $ OutOfTurn $ CurrentPlayerToActErr "player1"
      isPlayerActingOutOfTurn game playerName `shouldBe` expectedErr
    it
      "returns return Right () when player is acting in turn during heads up game" $ do
      let game =
            (street .~ PreFlop) .
            (dealer .~ 0) .
            (currentPosToAct .~ 1) .
            (players .~
             [ ((playerState .~ In) .
                (actedThisTurn .~ False) . (bet .~ 25) . (committed .~ 25))
                 player1
             , ((playerState .~ In) .
                (actedThisTurn .~ False) . (bet .~ 50) . (committed .~ 50))
                 player2
             ]) $
            initialGameState'
      let playerName = "player1"
      isPlayerActingOutOfTurn game playerName `shouldBe` Right ()
      let game2 =
            (street .~ PreFlop) .
            (dealer .~ 1) .
            (players .~
             [ ((playerState .~ In) .
                (actedThisTurn .~ False) . (bet .~ 50) . (committed .~ 50))
                 player1
             , ((playerState .~ In) .
                (actedThisTurn .~ False) . (bet .~ 25) . (committed .~ 25))
                 player2
             ]) $
            initialGameState'
      let playerName2 = "player2"
      isPlayerActingOutOfTurn game2 playerName2 `shouldBe` Right ()
    it "return no Error if player is acting in turn" $
      isPlayerActingOutOfTurn game "player1" `shouldBe` Right ()
    it
      "returns Just NotAtTable Error if no player with playerName is sat at table" $ do
      let expectedErr = Left $ NotAtTable "MissingPlayer"
      checkPlayerSatAtTable game "MissingPlayer" `shouldBe` expectedErr
  describe "canBet" $ do
    it
      "should return NotEnoughChipsForAction InvalidMoveErr if raise value is greater than remaining chips" $ do
      let game2 =
            (players .~ playerFixtures2) . (street .~ Flop) $ initialGameState'
          playerName = "player3"
          amount = 10000
          expectedErr = Left $ InvalidMove playerName $ NotEnoughChipsForAction
      canBet playerName amount game2 `shouldBe` expectedErr
    it
      "should return CannotBetShouldRaiseInstead InvalidMoveErr if players have already bet or raised already" $ do
      let game2 =
            (players .~ playerFixtures) . (street .~ Flop) . (maxBet .~ 100) $
            initialGameState'
      let playerName = "player3"
      let amount = 50
      let errMsg =
            "A bet can only be carried out if no preceding player has bet"
      let expectedErr =
            Left $ InvalidMove playerName $ CannotBetShouldRaiseInstead errMsg
      canBet playerName amount game2 `shouldBe` expectedErr
    it
      "should return BetLessThanBigBlind InvalidMoveErr if bet is less than the current big blind" $ do
      let game2 =
            (players .~ playerFixtures2) . (street .~ Flop) $ initialGameState'
      let playerName = "player3"
      let amount = 2
      let expectedErr = Left $ InvalidMove playerName $ BetLessThanBigBlind
      canBet playerName amount game2 `shouldBe` expectedErr
    it "should not return an error if player can bet" $ do
      let game2 =
            (players .~ playerFixtures2) . (maxBet .~ 0) . (street .~ Flop) $
            initialGameState'
      let playerName = "player3"
      let amount = 100
      canBet playerName amount game2 `shouldBe` Right ()
    it
      "should return InvalidActionForStreet InvalidMoveErr if game stage is PreDeal" $ do
      let preDealGame =
            (street .~ PreDeal) . (players .~ playerFixtures2) $
            initialGameState'
      let playerName = "player3"
      let amount = 100
      let expectedErr = Left $ InvalidMove playerName $ InvalidActionForStreet
      canBet playerName amount preDealGame `shouldBe` expectedErr
    it
      "should return InvalidActionForStreet InvalidMoveErr if game stage is Showdown" $ do
      let showdownGame =
            (street .~ Showdown) . (players .~ playerFixtures2) $
            initialGameState'
      let playerName = "player3"
      let amount = 100
      let expectedErr = Left $ InvalidMove playerName $ InvalidActionForStreet
      canBet playerName amount showdownGame `shouldBe` expectedErr
  describe "canRaise" $ do
    it
      "should return InvalidActionForStreet InvalidMoveErr if game stage is PreDeal" $ do
      let preDealGame =
            (street .~ PreDeal) . (players .~ playerFixtures2) $
            initialGameState'
      let playerName = "player3"
      let amount = 100
      let expectedErr = Left $ InvalidMove playerName $ InvalidActionForStreet
      canBet playerName amount preDealGame `shouldBe` expectedErr
    it "should return InvalidActionForStreet if game stage is PreDeal" $ do
      let game =
            (street .~ PreDeal) . (players .~ playerFixtures) $
            initialGameState'
      let playerName = "player3"
      let amount = 50
      let minRaise = 400
      let expectedErr = Left $ InvalidMove playerName $ InvalidActionForStreet
      canRaise playerName amount game `shouldBe` expectedErr
    it
      "should be able to raise all in when chip count is less than minimum raise amount" $ do
      let game =
            (street .~ PreFlop) . (players .~ playerFixtures) . (maxBet .~ 200) $
            initialGameState'
      let playerName = "player3"
      let amount = 300
      canRaise playerName amount game `shouldBe` Right ()
    it
      "should return InvalidActionForStreet InvalidMoveErr if game stage is PreDeal" $ do
      let preDealGame =
            (street .~ PreDeal) . (players .~ playerFixtures2) $
            initialGameState'
      let playerName = "player3"
      let amount = 100
      let expectedErr = Left $ InvalidMove playerName $ InvalidActionForStreet
      canRaise playerName amount preDealGame `shouldBe` expectedErr
    it
      "should return InvalidActionForStreet InvalidMoveErr if game stage is Showdown" $ do
      let showdownGame =
            (street .~ Showdown) . (players .~ playerFixtures2) $
            initialGameState'
      let playerName = "player3"
      let amount = 100
      let expectedErr = Left $ InvalidMove playerName $ InvalidActionForStreet
      canRaise playerName amount showdownGame `shouldBe` expectedErr
  describe "canCheck" $ do
    it
      "should return CannotCheckShouldCallRaiseOrFold InvalidMoveErr if maxBet is greater than zero and player bet is not equal to maxBet" $ do
      let game =
            (street .~ PreFlop) . (players .~ playerFixtures) . (maxBet .~ 200) $
            initialGameState'
      let playerName = "player3"
      let expectedErr =
            Left $ InvalidMove playerName $ CannotCheckShouldCallRaiseOrFold
      canCheck playerName game `shouldBe` expectedErr
    it
      "should allow Big Blind player to check during PreFlop when no bets or raises have occurred" $ do
      let game =
            (street .~ PreFlop) .
            (players .~
             [ ((playerState .~ In) .
                (actedThisTurn .~ True) . (bet .~ 50) . (committed .~ 50))
                 player1
             , ((playerState .~ In) .
                (actedThisTurn .~ False) . (bet .~ 50) . (committed .~ 50))
                 player2
             ]) $
            initialGameState'
      let playerName = "player2"
      canCheck playerName game `shouldBe` Right ()
    it
      "should return InvalidActionForStreet InvalidMoveErr if game stage is PreDeal" $ do
      let preDealGame =
            (street .~ PreDeal) . (players .~ playerFixtures2) $
            initialGameState'
      let playerName = "player3"
      let amount = 100
      let expectedErr = Left $ InvalidMove playerName $ InvalidActionForStreet
      canCheck playerName preDealGame `shouldBe` expectedErr
    it
      "should return InvalidActionForStreet InvalidMoveErr if game stage is Showdown" $ do
      let showdownGame =
            (street .~ Showdown) . (players .~ playerFixtures2) $
            initialGameState'
      let playerName = "player3"
      let amount = 100
      let expectedErr = Left $ InvalidMove playerName $ InvalidActionForStreet
      canCheck playerName showdownGame `shouldBe` expectedErr
  describe "canCall" $ do
    it
      "should return InvalidActionForStreet InvalidMoveErr if game stage is PreDeal" $ do
      let preDealGame =
            (street .~ PreDeal) . (players .~ playerFixtures2) $
            initialGameState'
      let playerName = "player3"
      let amount = 100
      let expectedErr = Left $ InvalidMove playerName $ InvalidActionForStreet
      canCall playerName preDealGame `shouldBe` expectedErr
    it
      "should return InvalidActionForStreet InvalidMoveErr if game stage is Showdown" $ do
      let showdownGame =
            (street .~ Showdown) . (players .~ playerFixtures2) $
            initialGameState'
      let playerName = "player3"
      let amount = 100
      let expectedErr = Left $ InvalidMove playerName $ InvalidActionForStreet
      canCall playerName showdownGame `shouldBe` expectedErr
    it
      "should return CannotCallZeroAmountCheckOrBetInstead InvalidMoveErr if game stage is not Preflop" $ do
      let game =
            (street .~ Flop) . (maxBet .~ 0) . (players .~ playerFixtures2) $
            initialGameState'
      let playerName = "player5"
      let expectedErr =
            Left $
            InvalidMove playerName $ CannotCallZeroAmountCheckOrBetInstead
      canCall playerName game `shouldBe` expectedErr
    it "should not return error if call bigBlind during Preflop" $ do
      let game =
            (street .~ PreFlop) . (players .~ playerFixtures2) $
            initialGameState'
      let playerName = "player5"
      let expectedErr = CannotCallZeroAmountCheckOrBetInstead
      canCall playerName game `shouldBe` Right ()
  describe "canFold" $ do
    it
      "should return InvalidActionForStreet InvalidMoveErr if game stage is PreDeal" $ do
      let preDealGame =
            (street .~ PreDeal) . (players .~ playerFixtures2) $
            initialGameState'
      let playerName = "player3"
      let expectedErr = Left $ InvalidMove playerName $ InvalidActionForStreet
      canFold playerName preDealGame `shouldBe` expectedErr
    it
      "should return InvalidActionForStreet InvalidMoveErr if game stage is Showdown" $ do
      let showdownGame =
            (street .~ Showdown) . (players .~ playerFixtures2) $
            initialGameState'
      let playerName = "player3"
      let expectedErr = Left $ InvalidMove playerName $ InvalidActionForStreet
      canFold playerName showdownGame `shouldBe` expectedErr
  describe "canShowOrMuckHand" $ do
    it "should return InvalidMoveErr if game stage is not Showdown" $ do
      let preDealGame =
            (street .~ PreDeal) . (players .~ playerFixtures2) $
            initialGameState'
      let playerName = "player3"
      let expectedErr = Left $ InvalidMove playerName $ InvalidActionForStreet
      canShowOrMuckHand playerName preDealGame `shouldBe` expectedErr
    it "should return InvalidMoveErr if hand is not a singlePlayer showdown" $ do
      let showdownGame =
            (street .~ Showdown) .
            (pot .~ 1000) .
            (deck .~ initialDeck) .
            (winners .~ MultiPlayerShowdown [((Pair, []), "player4")]) .
            (players .~
             [ (((playerState .~ In) . (actedThisTurn .~ True)) player4)
             , (((playerState .~ In) . (actedThisTurn .~ True)) player5)
             ]) $
            initialGameState'
      let playerName = "player5"
      let expectedErr =
            Left $
            InvalidMove playerName $
            CannotShowHandOrMuckHand
              "Can only show or muck cards if winner of single player pot during showdown"
      canShowOrMuckHand playerName showdownGame `shouldBe` expectedErr
    it
      "should return InvalidMoveErr if action was not sent by winner of single player showdown" $ do
      let showdownGame =
            (street .~ Showdown) .
            (pot .~ 1000) .
            (deck .~ initialDeck) .
            (winners .~ SinglePlayerShowdown "player4") .
            (players .~
             [ (((playerState .~ In) . (actedThisTurn .~ True)) player4)
             , (((playerState .~ Folded) . (actedThisTurn .~ True)) player5)
             ]) $
            initialGameState'
      let playerName = "player5"
      let expectedErr =
            Left $
            InvalidMove playerName $
            CannotShowHandOrMuckHand "Not winner of hand"
      canShowOrMuckHand playerName showdownGame `shouldBe` expectedErr
    it
      "should return no InvalidMoveErr if action was sent by winner of single player showdown" $ do
      let showdownGame =
            (street .~ Showdown) .
            (pot .~ 1000) .
            (deck .~ initialDeck) .
            (winners .~ SinglePlayerShowdown "player4") .
            (players .~
             [ (((playerState .~ In) . (actedThisTurn .~ True)) player4)
             , (((playerState .~ Folded) . (actedThisTurn .~ True)) player5)
             ]) $
            initialGameState'
      let playerName = "player4"
      canShowOrMuckHand playerName showdownGame `shouldBe` Right ()
  describe "canTimeout" $ do
    it
      "should return InvalidActionForStreet InvalidMoveErr for Timeout if player is acting out of turn" $ do
      let preFlopGame =
            (street .~ PreFlop) . (players .~ playerFixtures2) $
            initialGameState'
      let playerName = "player3"
      let expectedErr =
            Left $
            InvalidMove "player3" $ OutOfTurn $ CurrentPlayerToActErr "player5"
      validateAction preFlopGame playerName Timeout `shouldBe` expectedErr
    it "should return no error for Timeout when acting in turn" $ do
      let preFlopGame =
            (street .~ PreFlop) . (players .~ playerFixtures2) $
            initialGameState'
      let playerName = "player5"
      validateAction preFlopGame playerName Timeout `shouldBe` Right ()
    it
      "should return InvalidActionForStreet InvalidMoveErr if Timeout action occurs during Showdown" $ do
      let showDownGame =
            (street .~ Showdown) . (players .~ playerFixtures2) $
            initialGameState'
      let playerName = "player3"
      let expectedErr = Left $ InvalidMove playerName InvalidActionForStreet
      validateAction showDownGame playerName Timeout `shouldBe` expectedErr
    it "should return err for LeaveSeat if game state is not PreDeal" $ do
      let preFlopGame =
            (street .~ PreFlop) . (players .~ playerFixtures2) $
            initialGameState'
      let playerName = "player3"
      let expectedErr = InvalidMove "player3" CannotLeaveSeatOutsidePreDeal
      validateAction preFlopGame playerName LeaveSeat' `shouldBe`
        Left expectedErr
    it "should return err for LeaveSeat if player is not sat at Table" $ do
      let preDealGame =
            (street .~ PreDeal) . (players .~ playerFixtures2) $
            initialGameState'
      let playerName = "playerX"
      let expectedErr = NotAtTable playerName
      validateAction preDealGame playerName LeaveSeat' `shouldBe`
        Left expectedErr
    it
      "should return no err for leave seat if player is sat at table during PreDeal" $ do
      let preDealGame =
            (street .~ PreDeal) . (players .~ playerFixtures2) $
            initialGameState'
      let playerName = "player3"
      validateAction preDealGame playerName LeaveSeat' `shouldBe` Right ()
  describe "canSitOut" $ do
    it
      "should allow player to sit out of the game during the PreDeal street if sat in" $ do
      let preDealGame =
            (street .~ PreDeal) .
            (players .~
             [ ((playerState .~ In) .
                (actedThisTurn .~ False) . (bet .~ 0) . (committed .~ 0))
                 player1
             , ((playerState .~ In) .
                (actedThisTurn .~ False) . (bet .~ 0) . (committed .~ 0))
                 player2
             ]) $
            initialGameState'
      let playerName = "player1"
      validateAction preDealGame playerName SitOut `shouldBe` Right ()
    it "should return error if player is not at table" $ do
      let preDealGame =
            (street .~ PreFlop) .
            (players .~
             [ ((playerState .~ In) .
                (actedThisTurn .~ True) . (bet .~ 50) . (committed .~ 50))
                 player1
             , ((playerState .~ In) .
                (actedThisTurn .~ False) . (bet .~ 50) . (committed .~ 50))
                 player2
             ]) $
            initialGameState'
      let playerName = "player3"
      let expectedErr = Left $ NotAtTable playerName
      validateAction preDealGame playerName SitOut `shouldBe` expectedErr
    it "should not allow player to sit out of the game if already sat out" $ do
      let preDealGame =
            (street .~ PreDeal) .
            (players .~
             [ ((playerState .~ None) .
                (actedThisTurn .~ False) . (bet .~ 0) . (committed .~ 0))
                 player1
             , ((playerState .~ In) .
                (actedThisTurn .~ False) . (bet .~ 0) . (committed .~ 0))
                 player2
             ]) $
            initialGameState'
      let playerName = "player1"
      let expectedErr = Left $ InvalidMove playerName AlreadySatOut
      validateAction preDealGame playerName SitOut `shouldBe` expectedErr
    it "should not allow player to sit out unless street is PreDeal" $ do
      let preDealGame =
            (street .~ PreFlop) .
            (players .~
             [ ((playerState .~ In) .
                (actedThisTurn .~ True) . (bet .~ 50) . (committed .~ 50))
                 player1
             , ((playerState .~ In) .
                (actedThisTurn .~ False) . (bet .~ 50) . (committed .~ 50))
                 player2
             ]) $
            initialGameState'
      let playerName = "player2"
      let expectedErr = Left $ InvalidMove playerName CannotSitOutOutsidePreDeal
      validateAction preDealGame playerName SitOut `shouldBe` expectedErr
