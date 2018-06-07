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
  = None -- none denotes a player that will not be dealt cards unless they send a postblinds action to the server
  | Out Out
  | In
  deriving (Eq, Show, Ord, Read, Generic, ToJSON, FromJSON)

data Street
  = PreDeal
  | PreFlop
  | Flop
  | Turn
  | River
  | Showdown
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
  , waitlist :: [Text] --playernames
  , deck :: [Card]
  , smallBlind :: Int
  , bigBlind :: Int
  , street :: Street
  , pot :: Int
  , maxBet :: Bet
  , dealer :: Int
  , currentPosToAct :: Int -- position here refes to the zero indexed set of active users
  } deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)

type PlayerName = Text

data Blind
  = Small
  | Big
  deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)

data PlayerAction
  = TakeSeat Player
  | LeaveSeat
  | PostBlind Blind
  | Fold
  | Call
  | Raise Int
  | Check
  | Bet Int
  deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)

data GameErr
  = NotEnoughChips PlayerName
  | PlayerNotAtTable PlayerName
  | NotAtTable PlayerName
  | OutOfTurn PlayerName
              CurrentPlayerToActErr
  | InvalidMove PlayerName
                InvalidMoveErr
  deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)

-- if player takes an invalid move we need to inform the client and include the reason why
data InvalidMoveErr
  = BlindNotRequired
  | BlindRequired Blind
  deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)

newtype CurrentPlayerToActErr =
  CurrentPlayerToActErr PlayerName
  deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)
