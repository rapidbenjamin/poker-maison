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
{-# LANGUAGE ScopedTypeVariables #-}

module GameSpec where

import Control.Lens
import Data.Either
import Data.List
import Data.Text (Text)
import Test.Hspec
import Test.QuickCheck hiding (Big, Small)
import Test.QuickCheck.Modifiers

import Poker
import Poker.ActionValidation
import Poker.Game
import Poker.Types

import Control.Lens
import Control.Monad
import Control.Monad.State hiding (state)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List.Lens
import Data.List.Split
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import GHC.Generics
import System.IO.Unsafe
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

instance Arbitrary HandRank where
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

player1 =
  Player
    { _pockets =
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

player5 =
  Player
    { _pockets =
        [Card {rank = King, suit = Diamonds}, Card {rank = Four, suit = Spades}]
    , _chips = 2000
    , _bet = 50
    , _playerState = In
    , _playerName = "player1"
    , _committed = 50
    , _actedThisTurn = True
    }

player6 =
  Player
    { _pockets = []
    , _chips = 2000
    , _bet = 0
    , _playerState = None
    , _playerName = "player6"
    , _committed = 0
    , _actedThisTurn = False
    }

initPlayers = [player1, player2, player3]

main :: IO ()
main =
  hspec $ describe "Poker.Game" $ do
    describe "dealToPlayers" $ do
      it "should deal correct number of cards" $ do
        let (_, newPlayers) = dealToPlayers initialDeck [player1, player3]
        (all
           (\Player {..} ->
              if _playerState == In
                then length _pockets == 2
                else null _pockets)
           newPlayers) `shouldBe`
          True
      it "should preserve ordering of players" $ property $ \(players) ->
        length players <= 21 ==> do
          let players' = players :: [Player]
          let (remainingDeck, players) = dealToPlayers initialDeck players'
          (_playerName <$> players) == (_playerName <$> players')
      it "the resulting set of cards should contain no duplicates" $ property $ \(players) -> do
        length players <= 21 ==> do
          let players' -- deal to players that have no pocket cards already
               = (players :: [Player]) & traverse . pockets .~ ([] :: [Card])
          let (remainingDeck, players) = dealToPlayers initialDeck players'
          let playerCards = concat $ _pockets <$> players
          null $ playerCards `intersect` remainingDeck
    describe "dealBoardCards" $ do
      it "should deal correct number of cards to board" $ property $ \(Positive n) -> do
        n < 52 ==> do
          let newGame = dealBoardCards n initialGameState
          length (newGame ^. board) `shouldBe` n
      it "should remove dealt cards from deck" $ property $ \(Positive n) -> do
        n < 52 ==> do
          let newGame = dealBoardCards n initialGameState
          length (newGame ^. deck) `shouldBe` (length initialDeck - n)
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
              initialGameState
        haveAllPlayersActed game `shouldBe` True
      it
        "should return False when not all players acted during PreDeal for Three Players" $ do
        let unfinishedBlindsGame =
              (street .~ PreDeal) . (players .~ [player1, player4, player6]) $
              initialGameState
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
              initialGameState
        haveAllPlayersActed game `shouldBe` True
      it
        "should return False when not all players acted during PreFlop for Two Players" $ do
        let unfinishedBlindsGame =
              (street .~ PreDeal) . (players .~ [player1, player4]) $
              initialGameState
        haveAllPlayersActed unfinishedBlindsGame `shouldBe` False
    describe "allButOneFolded" $ do
      it "should return True when all but one player " $ do
        let game =
              (street .~ PreFlop) .
              (players .~ [((playerState .~ (Folded)) player1), player2]) $
              initialGameState
        allButOneFolded game `shouldBe` True
      it "should return False when not all players acted" $ do
        let unfinishedBlindsGame =
              (street .~ PreFlop) . (players .~ [player1, player3]) $
              initialGameState
        allButOneFolded unfinishedBlindsGame `shouldBe` False
      it "should always return False for PreDeal (blinds) stage" $ do
        let unfinishedBlindsGame =
              (street .~ PreDeal) .
              (players .~ [((playerState .~ (Folded)) player1), player2]) $
              initialGameState
        allButOneFolded unfinishedBlindsGame `shouldBe` False
    describe "progressToPreFlop" $ do
      let preDealGame =
            (street .~ PreDeal) . (maxBet .~ 50) . (pot .~ 75) .
            (deck .~ initialDeck) .
            (players .~
             [ (((chips .~ 1000) . (committed .~ 25) . (bet .~ 25)) player5)
             , (((chips .~ 1000) . (committed .~ 50) . (bet .~ 50)) player2)
             ]) $
            initialGameState
      let preFlopGame = progressToPreFlop preDealGame
      traceShowM (_players preDealGame)
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
            initialGameState
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
            initialGameState
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
            initialGameState
      let riverGame = progressToRiver turnGame
      it "should update street to River" $ riverGame ^. street `shouldBe` River
      it "should reset maxBet" $ riverGame ^. maxBet `shouldBe` 0
      it "should reset all player bets" $ do
        let turnGame =
              (street .~ Turn) . (maxBet .~ 1000) . (pot .~ 1000) .
              (deck .~ initialDeck) .
              (players .~ [((chips .~ 1000) player5), ((chips .~ 1000) player2)]) $
              initialGameState
        let riverGame = progressToRiver turnGame
        let playerBets = (^. bet) <$> _players riverGame
        playerBets `shouldBe` [0, 0]
    describe "progressToShowdown" $ do
      let riverGame =
            (street .~ River) . (pot .~ 1000) . (deck .~ initialDeck) .
            (players .~ [((chips .~ 1000) player5), ((chips .~ 1000) player2)]) $
            initialGameState
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
              initialGameState
        let showdownGame = progressToShowdown riverGame
        let playerChipCounts =
              (\Player {..} -> _chips) <$> (_players showdownGame)
        playerChipCounts `shouldBe` [1500, 1500]
    describe "getNextHand" $ do
      let showdownGame =
            (street .~ Showdown) . (maxBet .~ 1000) . (pot .~ 1000) .
            (deck .~ initialDeck) .
            (players .~ [((chips .~ 1000) player5), ((chips .~ 1000) player2)]) $
            initialGameState
      let preDealGame = getNextHand showdownGame []
      it "should update street to PreDeal" $ preDealGame ^. street `shouldBe`
        PreDeal
      it "should set maxBet to bigBlind" $ preDealGame ^. maxBet `shouldBe`
        _bigBlind showdownGame
      it "should reset all player bets" $ do
        let playerBets = (\Player {..} -> _bet) <$> (_players preDealGame)
        playerBets `shouldBe` [0, 0]
      it "should increment dealer position" $ preDealGame ^. dealer `shouldBe`
        (showdownGame ^. dealer) +
        1
    describe "isEveryoneAllIn" $ do
      it "should return False for two player game if no one all in" $ do
        let preFlopGame' =
              (street .~ PreFlop) . (pot .~ 1000) . (deck .~ initialDeck) .
              (players .~
               [ (((playerState .~ In) . (actedThisTurn .~ False)) player1)
               , (((playerState .~ In) . (actedThisTurn .~ True)) player3)
               ]) $
              initialGameState
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
              initialGameState
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
              initialGameState
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
              initialGameState
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
              initialGameState
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
              initialGameState
        isEveryoneAllIn flopGame `shouldBe` False
