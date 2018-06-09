{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Poker where

------------------------------------------------------------------------------
import Control.Monad.State.Lazy
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (Text)

import Poker.ActionValidation

------------------------------------------------------------------------------
import Poker.Betting
import Poker.Game
import Poker.Hands
import Poker.Types
import Poker.Utils

newGame :: Game -> State Game (Maybe GameErr)
newGame initState = state $ \_ -> (Nothing, initialGameState)

-- this is public api of the poker module 
-- the function takes a player action and returns either a new game for a valid 
-- player action or an err signifying an invalid player action with the reason why
-- if the current game stage is showdown then the next game state will have a newly shuffled
-- deck and pocket cards/ bets reset
progressGame :: PlayerName -> PlayerAction -> StateT Game IO (Maybe GameErr)
progressGame playerName action =
  StateT $ \currGame@Game {..} ->
    case handlePlayerAction currGame playerName action of
      Left err -> return (Just err, currGame)
      Right newGameState ->
        if _street == Showdown
          then do
            nextGameState <- liftIO $ getNextHand currGame
            return (Nothing, nextGameState)
          else return (Nothing, newGameState)

------------------------------------------------------------------------------
initialGameState :: Game
initialGameState =
  Game
    { _players = []
    , _waitlist = []
    , _maxPlayers = 5
    , _dealer = 0
    , _currentPosToAct = 0 -- position here refes to the zero indexed set of active users
    , _community = []
    , _deck = []
    , _smallBlind = 25
    , _bigBlind = 50
    , _pot = 0
    , _street = PreDeal
    , _maxBet = 0
    }

-- initially a players state is set to None to denote that they havent posted their blinds yet
getPlayer :: Text -> Int -> Player
getPlayer playerName chips =
  Player
    { _pockets = []
    , _bet = 0
    , _playerState = None
    , _playerName = playerName
    , _committed = 0
    , _chips = chips
    }

handlePlayerAction :: Game -> PlayerName -> PlayerAction -> Either GameErr Game
handlePlayerAction game _ action@(SitDown player) = seatPlayer game player
handlePlayerAction game playerName action@LeaveSeat {} = undefined
handlePlayerAction game@Game {..} playerName action@(PostBlind blind) =
  maybe
    (Right $ postBlind game playerName blind)
    Left
    (validateAction game playerName action)
handlePlayerAction game playerName action@Fold {} = undefined
handlePlayerAction game playerName action@Call {} = undefined
handlePlayerAction game playerName action@Raise {} = undefined
handlePlayerAction game playerName action@Check {} = undefined
handlePlayerAction game playerName action@Bet {} = undefined

-- TODO should be able to choose seat
seatPlayer :: Game -> Player -> Either GameErr Game
seatPlayer Game {..} player@Player {..}
  | _playerName `elem` getPlayerNames _players =
    Left $ AlreadySatAtTable _playerName
  | length _players < _maxPlayers =
    Right Game {_players = _players <> [player], ..}
  | otherwise = Right $ Game {_waitlist = _waitlist <> [_playerName], ..}

-- reset hand related state
-- TODO move players from waitlist to players list
-- TODO need to send msg to players on waitlist when a seat frees up to inform them 
-- to choose a seat and set limit for them t pick one
-- TODO - have newBlindNeeded field which new players will initially be put into in order to 
-- ensure they cant play without posting a blind before the blind position comes round to them
-- new players can of course post their blinds early. In the case of an early posting the initial
-- blind must be the big blind. After this 'early' blind or the posting of a normal blind in turn the 
-- new player will be removed from the newBlindNeeded field and can play normally.
getNextHand :: Game -> IO Game
getNextHand Game {..} = do
  shuffledDeck <- shuffle initialDeck
  return
    Game
      { _waitlist = newWaitlist
      , _maxBet = 0
      , _players = newPlayers
      , _community = []
      , _deck = shuffledDeck
      , _street = PreDeal
      , _dealer = newDealer
      , _currentPosToAct = nextPlayerToAct
      , ..
      }
  where
    newDealer = _dealer `modInc` length (getPlayersSatIn _players)
    freeSeatsNo = _maxPlayers - length _players
    newPlayers = resetPlayerCardsAndBets <$> _players
    newWaitlist = drop freeSeatsNo _waitlist
    nextPlayerToAct = _currentPosToAct `modInc` length newPlayers

resetPlayerCardsAndBets :: Player -> Player
resetPlayerCardsAndBets Player {..} =
  Player {_pockets = [], _bet = 0, _committed = 0, ..}
