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
    , _playerState = Out AllIn
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
      it "should preserve ordering of players" $ do
        property $ \(players) -> do
          length players <= 21 ==> do
            let players' = players :: [Player]
            let (remainingDeck, players) = dealToPlayers initialDeck players'
            (_playerName <$> players) == (_playerName <$> players')
      it "the resulting set of cards should contain no duplicates" $ do
        property $ \(players) -> do
          length players <= 21 ==> do
            let players' -- deal to players that have no pocket cards already
                 = (players :: [Player]) & traverse . pockets .~ ([] :: [Card])
            let (remainingDeck, players) = dealToPlayers initialDeck players'
            let playerCards = concat $ _pockets <$> players
            null $ playerCards `intersect` remainingDeck
    describe "dealBoardCards" $ do
      it "should deal correct number of cards to board" $ do
        property $ \(Positive n) -> do
          n < 52 ==> do
            let newGame = dealBoardCards n initialGameState
            length (newGame ^. board) `shouldBe` n
      it "should remove dealt cards from deck" $ do
        property $ \(Positive n) -> do
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
              (players .~ [((playerState .~ (Out Folded)) player1), player2]) $
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
              (players .~ [((playerState .~ (Out Folded)) player1), player2]) $
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
      it "should update street to PreFlop" $ do
        preFlopGame ^. street `shouldBe` PreFlop
      it "should not reset all player bets!!!!!!" $ do
        let playerBets = (^. bet) <$> (_players preFlopGame)
        playerBets `shouldBe` [25, 50]
    describe "progressToFlop" $ do
      let preFlopGame =
            (street .~ Flop) . (maxBet .~ 1000) . (pot .~ 1000) .
            (deck .~ initialDeck) .
            (players .~ [((chips .~ 1000) player5), ((chips .~ 1000) player2)]) $
            initialGameState
      let flopGame = progressToFlop preFlopGame
      it "should update street to Turn" $ do flopGame ^. street `shouldBe` Flop
      it "should reset maxBet" $ do flopGame ^. maxBet `shouldBe` 0
      it "should reset all player bets" $ do
        let playerBets = (\Player {..} -> _bet) <$> (_players flopGame)
        playerBets `shouldBe` [0, 0]
    describe "progressToTurn" $ do
      let flopGame =
            (street .~ Flop) . (maxBet .~ 1000) . (pot .~ 1000) .
            (deck .~ initialDeck) .
            (players .~ [((chips .~ 1000) player5), ((chips .~ 1000) player2)]) $
            initialGameState
      let turnGame = progressToTurn flopGame
      it "should update street to Turn" $ do turnGame ^. street `shouldBe` Turn
      it "should reset maxBet" $ do turnGame ^. maxBet `shouldBe` 0
      it "should reset all player bets" $ do
        let playerBets = (\Player {..} -> _bet) <$> (_players turnGame)
        playerBets `shouldBe` [0, 0]
    describe "progressToRiver" $ do
      let turnGame =
            (street .~ Turn) . (maxBet .~ 1000) . (pot .~ 1000) .
            (deck .~ initialDeck) .
            (players .~ [((chips .~ 1000) player5), ((chips .~ 1000) player2)]) $
            initialGameState
      let riverGame = progressToRiver turnGame
      it "should update street to River" $ do
        riverGame ^. street `shouldBe` River
      it "should reset maxBet" $ do riverGame ^. maxBet `shouldBe` 0
      it "should reset all player bets" $ do
        let turnGame =
              (street .~ Turn) . (maxBet .~ 1000) . (pot .~ 1000) .
              (deck .~ initialDeck) .
              (players .~ [((chips .~ 1000) player5), ((chips .~ 1000) player2)]) $
              initialGameState
        let riverGame = progressToRiver turnGame
        let playerBets = (\Player {..} -> _bet) <$> (_players riverGame)
        playerBets `shouldBe` [0, 0]
    describe "progressToShowdown" $ do
      let riverGame =
            (street .~ River) . (pot .~ 1000) . (deck .~ initialDeck) .
            (players .~ [((chips .~ 1000) player5), ((chips .~ 1000) player2)]) $
            initialGameState
      let showdownGame = progressToShowdown riverGame
      it "should update street to Turn" $ do
        showdownGame ^. street `shouldBe` Showdown
      it "should award pot chips to winner of hand" $ do
        let playerChipCounts =
              (\Player {..} -> _chips) <$> (_players showdownGame)
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
      it "should update street to PreDeal" $ do
        preDealGame ^. street `shouldBe` PreDeal
      it "should set maxBet to bigBlind" $ do
        preDealGame ^. maxBet `shouldBe` _bigBlind showdownGame
      it "should reset all player bets" $ do
        let playerBets = (\Player {..} -> _bet) <$> (_players preDealGame)
        playerBets `shouldBe` [0, 0]
      it "should increment dealer position" $ do
        preDealGame ^. dealer `shouldBe` (showdownGame ^. dealer) + 1
    describe "hasBettingFinished" $ do
      it
        "should return True when all players are All In and all players have acted" $ do
        let flopGame =
              (street .~ Flop) . (pot .~ 1000) . (deck .~ initialDeck) .
              (players .~
               [ (((playerState .~ In) . (actedThisTurn .~ True)) player1)
               , (((playerState .~ Out AllIn) . (actedThisTurn .~ True)) player2)
               ]) $
              initialGameState
        hasBettingFinished flopGame `shouldBe` True
      it
        "should return False when all players are All In and not all players have acted" $ do
        let flopGame =
              (street .~ Flop) . (pot .~ 1000) . (deck .~ initialDeck) .
              (players .~
               [ (((playerState .~ Out AllIn) . (actedThisTurn .~ True)) player1)
               , (((playerState .~ In) . (actedThisTurn .~ False)) player2)
               ]) $
              initialGameState
        hasBettingFinished flopGame `shouldBe` False
      it "should return False when more than one player is not AllIn" $ do
        let flopGame =
              (street .~ Flop) . (pot .~ 1000) . (deck .~ initialDeck) .
              (players .~
               [ (((playerState .~ In) . (actedThisTurn .~ True)) player1)
               , (((playerState .~ Out AllIn) . (actedThisTurn .~ True)) player2)
               , (((playerState .~ In) . (actedThisTurn .~ True)) player3)
               ]) $
              initialGameState
        hasBettingFinished flopGame `shouldBe` False
      it "Should return False when a player is left to act" $ do
        let flopGame =
              eitherDecode
                "{\"_smallBlind\":25,\"_maxPlayers\":5,\"_waitlist\":[],\"_street\":\"Turn\",\"_deck\":[{\"suit\":\"Hearts\",\"rank\":\"Four\"},{\"suit\":\"Spades\",\"rank\":\"Four\"},{\"suit\":\"Clubs\",\"rank\":\"Five\"},{\"suit\":\"Diamonds\",\"rank\":\"Five\"},{\"suit\":\"Hearts\",\"rank\":\"Five\"},{\"suit\":\"Spades\",\"rank\":\"Five\"},{\"suit\":\"Clubs\",\"rank\":\"Six\"},{\"suit\":\"Diamonds\",\"rank\":\"Six\"},{\"suit\":\"Hearts\",\"rank\":\"Six\"},{\"suit\":\"Spades\",\"rank\":\"Six\"},{\"suit\":\"Clubs\",\"rank\":\"Seven\"},{\"suit\":\"Diamonds\",\"rank\":\"Seven\"},{\"suit\":\"Hearts\",\"rank\":\"Seven\"},{\"suit\":\"Spades\",\"rank\":\"Seven\"},{\"suit\":\"Clubs\",\"rank\":\"Eight\"},{\"suit\":\"Diamonds\",\"rank\":\"Eight\"},{\"suit\":\"Hearts\",\"rank\":\"Eight\"},{\"suit\":\"Spades\",\"rank\":\"Eight\"},{\"suit\":\"Clubs\",\"rank\":\"Nine\"},{\"suit\":\"Diamonds\",\"rank\":\"Nine\"},{\"suit\":\"Hearts\",\"rank\":\"Nine\"},{\"suit\":\"Spades\",\"rank\":\"Nine\"},{\"suit\":\"Clubs\",\"rank\":\"Ten\"},{\"suit\":\"Diamonds\",\"rank\":\"Ten\"},{\"suit\":\"Hearts\",\"rank\":\"Ten\"},{\"suit\":\"Spades\",\"rank\":\"Ten\"},{\"suit\":\"Clubs\",\"rank\":\"Jack\"},{\"suit\":\"Diamonds\",\"rank\":\"Jack\"},{\"suit\":\"Hearts\",\"rank\":\"Jack\"},{\"suit\":\"Spades\",\"rank\":\"Jack\"},{\"suit\":\"Clubs\",\"rank\":\"Queen\"},{\"suit\":\"Diamonds\",\"rank\":\"Queen\"},{\"suit\":\"Hearts\",\"rank\":\"Queen\"},{\"suit\":\"Spades\",\"rank\":\"Queen\"},{\"suit\":\"Clubs\",\"rank\":\"King\"},{\"suit\":\"Diamonds\",\"rank\":\"King\"},{\"suit\":\"Hearts\",\"rank\":\"King\"},{\"suit\":\"Spades\",\"rank\":\"King\"},{\"suit\":\"Clubs\",\"rank\":\"Ace\"},{\"suit\":\"Diamonds\",\"rank\":\"Ace\"},{\"suit\":\"Hearts\",\"rank\":\"Ace\"},{\"suit\":\"Spades\",\"rank\":\"Ace\"}],\"_dealer\":0,\"_pot\":75,\"_players\":[{\"_bet\":0,\"_playerState\":{\"tag\":\"In\"},\"_committed\":0,\"_pockets\":[{\"suit\":\"Clubs\",\"rank\":\"Two\"},{\"suit\":\"Diamonds\",\"rank\":\"Two\"}],\"_playerName\":\"1z\",\"_actedThisTurn\":true,\"_chips\":2000},{\"_bet\":0,\"_playerState\":{\"tag\":\"Out\",\"contents\":\"Folded\"},\"_committed\":25,\"_pockets\":[{\"suit\":\"Hearts\",\"rank\":\"Two\"},{\"suit\":\"Spades\",\"rank\":\"Two\"}],\"_playerName\":\"2z\",\"_actedThisTurn\":true,\"_chips\":1975},{\"_bet\":0,\"_playerState\":{\"tag\":\"In\"},\"_committed\":50,\"_pockets\":[{\"suit\":\"Clubs\",\"rank\":\"Three\"},{\"suit\":\"Diamonds\",\"rank\":\"Three\"}],\"_playerName\":\"3z\",\"_actedThisTurn\":false,\"_chips\":1950}],\"_currentPosToAct\":1,\"_board\":[{\"suit\":\"Hearts\",\"rank\":\"Three\"},{\"suit\":\"Spades\",\"rank\":\"Three\"},{\"suit\":\"Clubs\",\"rank\":\"Four\"},{\"suit\":\"Diamonds\",\"rank\":\"Four\"}],\"_winners\":{\"tag\":\"NoWinners\"},\"_maxBet\":0,\"_bigBlind\":50}"
        let turnGame = progressToTurn $ fromRight initialGameState flopGame
        hasBettingFinished turnGame `shouldBe` False
