{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Poker.Types where

import Data.Aeson
import Data.Aeson.Types
import Data.Text
import GHC.Generics

import Control.Monad.State hiding (state)
import Data.DeriveTH
import Data.Function
import Data.Text

------------------------------------------------------------------------------
data Rank
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Eq, Read, Ord, Bounded, Enum, Generic, ToJSON, FromJSON)

instance Show Rank where
  show x =
    case x of
      Two -> "2"
      Three -> "3"
      Four -> "4"
      Five -> "5"
      Six -> "6"
      Seven -> "7"
      Eight -> "8"
      Nine -> "9"
      Ten -> "T"
      Jack -> "J"
      Queen -> "Q"
      King -> "K"
      Ace -> "A"

data Suit
  = Clubs
  | Diamonds
  | Hearts
  | Spades
  deriving (Eq, Ord, Bounded, Enum, Read, Generic, ToJSON, FromJSON)

instance Show Suit where
  show x =
    case x of
      Clubs -> "♧ "
      Diamonds -> "♢ "
      Hearts -> "♡ "
      Spades -> "♤ "

data Card = Card
  { rank :: Rank
  , suit :: Suit
  } deriving (Eq, Read, Generic, ToJSON, FromJSON)

instance Ord Card where
  compare = compare `on` rank

instance Show Card where
  show (Card r s) = show r ++ show s

data HandRank
  = HighCard
  | Pair
  | TwoPair
  | Trips
  | Straight
  | Flush
  | FullHouse
  | Quads
  | StraightFlush
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

type Bet = Int

data Out
  = Folded
  | AllIn
  deriving (Eq, Show, Ord, Read, Generic, ToJSON, FromJSON)

data PlayerState
  = None
  | Out Out
  | In
  deriving (Eq, Show, Ord, Read, Generic, ToJSON, FromJSON)

data Street
  = PreDeal
  | PreFlop
  | Flop
  | Turn
  | River
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, ToJSON, FromJSON)

data Player = Player
  { pockets :: [Card]
  , chips :: Int
  , bet :: Bet
  , playerState :: PlayerState
  , playerName :: Text
  , committed :: Bet
  } deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)

data Game = Game
  { players :: [Player]
  , maxPlayers :: Int
  , community :: [Card]
  , deck :: [Card]
  , street :: Street
  , pot :: Int
  , maxBet :: Bet
  } deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)

type PlayerName = Text

data PlayerAction
  = TakeSeat PlayerName
  | LeaveSeat PlayerName
  deriving (Show, Eq, Read)

data PlayerMove
  = Fold PlayerName
  | Call PlayerName
  | Raise PlayerName
          Int
  | Check PlayerName
  | Bet PlayerName
        Int
  deriving (Show, Eq, Read)

data GameErr
  = NotEnoughChips
  | OutOfTurn
  deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)
