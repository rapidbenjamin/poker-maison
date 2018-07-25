{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Poker.Types where

import Control.Lens
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Types
import Data.Function
import Data.Monoid
import Data.Text
import GHC.Generics

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

data PlayerState
  = None -- none denotes a player that will not be dealt cards unless they send a postblinds action to the server
  | Folded
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
  { _pockets :: [Card]
  , _chips :: Int
  , _bet :: Bet
  , _playerState :: PlayerState
  , _playerName :: Text
  , _committed :: Bet
  , _actedThisTurn :: Bool
  } deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)

-- Folded To Signifies a a single player pot where everyone has
-- folded to them in this case the hand ranking is irrelevant 
-- and the winner takes all. Therefore the winner has the choice of showing 
-- or mucking (hiding) their cards as they are the only player in the pot.
--
-- Whereas in a MultiPlayer showdown all players must show their cards
-- as hand rankings are needed to ascertain the winner of the pot.
data Winners
  = MultiPlayerShowdown [((HandRank, [Card]), PlayerName)]
  | SinglePlayerShowdown PlayerName -- occurs when everyone folds to one player
  | NoWinners
  deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)

data Game = Game
  { _players :: [Player]
  , _maxPlayers :: Int
  , _board :: [Card]
  , _winners :: Winners
  , _waitlist :: [PlayerName]
  , _deck :: [Card]
  , _smallBlind :: Int
  , _bigBlind :: Int
  , _street :: Street
  , _pot :: Int
  , _maxBet :: Bet
  , _dealer :: Int
  , _currentPosToAct :: Int -- position here refes to the zero indexed set of active players that have a playerState not set to None
  } deriving (Eq, Read, Ord, Generic, ToJSON, FromJSON)

instance Show Game where
  show Game {..} =
    show _players <> show _board <> "\n dealer: " <> show _dealer <>
    "\n _currentPosToAct: " <>
    show _currentPosToAct <>
    "\n _street: " <>
    show _street <>
    "\n _winners: " <>
    show _winners <>
    "\n _board: " <>
    show _board

type PlayerName = Text

data Blind
  = Small
  | Big
  deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)

-- If you can check, that is you aren't facing an amount you have to call, 
-- then when you put in chips it is called a bet. If you have to put in
-- some amount of chips to continue with the hand, and you want to 
-- increase the pot, it's called a raise. If it is confusing, just remember 
-- this old poker adage: "You can't raise yourself."
--
-- Mucking hands refers to a player choosing not to
-- show his hands after everyone has folded to them. Essentially in
-- this scenario mucking or showing refers to the decision to
-- show ones hand or not to the table after everyone else has folded.
data PlayerAction
  = SitDown Player -- doesnt progress the game
  | LeaveSeat -- doesnt progress the game
  | PostBlind Blind
  | Fold
  | Call
  | Raise Int
  | Check
  | Bet Int
  | ShowHand
  | MuckHand
  | Timeout
  deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)

data GameErr
  = NotEnoughChips PlayerName
  | PlayerNotAtTable PlayerName
  | AlreadySatAtTable PlayerName
  | NotAtTable PlayerName
  | InvalidMove PlayerName
                InvalidMoveErr
  deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)

-- ToDO
-- errors for each player action should be defined as follows
-- cannotBet Text
-- cannotFold Text
-- so Text would be the error message giving information
-- if player takes an invalid move we need to inform the client and include the reason why
--
-- Also more generic actions such as InvalidActionForStreet just take a second Text argument
-- detailing which street would be valid for the action in question - also this error
-- doesn't tell us which action the error refers to
data InvalidMoveErr
  = BlindNotRequired
  | BlindRequired Blind
  | NoBlindRequired
  | BlindAlreadyPosted Blind
  | OutOfTurn CurrentPlayerToActErr
  | CannotPostBlindOutsidePreDeal
  | InvalidActionForStreet
  | BetLessThanBigBlind
  | NotEnoughChipsForAction
  | CannotBetShouldRaiseInstead
  | PlayerToActNotAtTable
  | CannotRaiseShouldBetInstead
  | RaiseAmountBelowMinRaise Int
  | CannotCheckShouldCallRaiseOrFold
  | CannotCallZeroAmountCheckOrBetInstead
  | CannotShowHandOrMuckHand Text
  deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)

newtype CurrentPlayerToActErr =
  CurrentPlayerToActErr PlayerName
  deriving (Show, Eq, Read, Ord, Generic, ToJSON, FromJSON)

makeLenses ''Player

makeLenses ''Game

makeLenses ''Winners
