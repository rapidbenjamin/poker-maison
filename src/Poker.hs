{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Poker where

import Control.Monad.State.Lazy
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (Text)

import Poker.ActionValidation
import Poker.Actions
import Poker.Blinds
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
runPlayerAction :: PlayerName -> PlayerAction -> StateT Game IO (Maybe GameErr)
runPlayerAction playerName action =
  StateT $ \currGame@Game {..} ->
    case handlePlayerAction currGame playerName action of
      Left err -> return (Just err, currGame)
      Right newGameState -> do
        liftIO $
          print $
          "\n haveAllPlayersActed: " <> show (haveAllPlayersActed newGameState)
        liftIO $
          print $
          "\n everyPlayerFolded: " <> show (everyPlayerFolded newGameState)
        case action of
          SitDown _ -> return (Nothing, newGameState)
          LeaveSeat -> return (Nothing, newGameState)
          _ -> do
            game' <- progressGame newGameState
            return (Nothing, game')

------------------------------------------------------------------------------
initialGameState :: Game
initialGameState =
  Game
    { _players = []
    , _waitlist = []
    , _maxPlayers = 5
    , _dealer = 0
    , _currentPosToAct = 1 -- position here refers to the zero indexed set of active users
    , _board = []
    , _deck = initialDeck
    , _smallBlind = 25
    , _bigBlind = 50
    , _pot = 0
    , _street = PreDeal
    , _maxBet = 0
    , _winners = []
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
    , _actedThisTurn = False
    , _chips = chips
    }

handlePlayerAction :: Game -> PlayerName -> PlayerAction -> Either GameErr Game
handlePlayerAction game _ action@(SitDown player) = seatPlayer game player
handlePlayerAction game playerName action@LeaveSeat = undefined
handlePlayerAction game@Game {..} playerName action@(PostBlind blind) =
  maybe
    (Right $ postBlind blind playerName game)
    Left
    (validateAction game playerName action)
handlePlayerAction game playerName action@Fold =
  maybe
    (Right $ foldCards playerName game)
    Left
    (validateAction game playerName action)
handlePlayerAction game playerName action@Call =
  maybe
    (Right $ call playerName game)
    Left
    (validateAction game playerName action)
handlePlayerAction game playerName action@(Raise amount) =
  maybe
    (Right $ makeBet amount playerName game)
    Left
    (validateAction game playerName action)
handlePlayerAction game playerName action@Check =
  maybe
    (Right $ check playerName game)
    Left
    (validateAction game playerName action)
handlePlayerAction game playerName action@(Bet amount) =
  maybe
    (Right $ makeBet amount playerName game)
    Left
    (validateAction game playerName action)

-- TODO should be able to choose seat
seatPlayer :: Game -> Player -> Either GameErr Game
seatPlayer Game {..} player@Player {..}
  | _playerName `elem` getPlayerNames _players =
    Left $ AlreadySatAtTable _playerName
  | length _players < _maxPlayers =
    Right Game {_players = _players <> [player], ..}
  | otherwise = Right $ Game {_waitlist = _waitlist <> [_playerName], ..}
