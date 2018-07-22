{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Poker where

import Control.Lens hiding (Fold)
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

import Text.Pretty.Simple (pPrint)

import Debug.Trace

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
        case action of
          SitDown _ -> return (Nothing, newGameState)
          LeaveSeat -> return (Nothing, newGameState)
          _ -> do
            liftIO $ print $ "action is " <> show action
            game' <- progressGame newGameState
            liftIO $ pPrint game'
            liftIO $
              print $
              "\n haveAllPlayersActed: " <> show (haveAllPlayersActed game')
            liftIO $
              print $ "\n allButOneFolded: " <> show (allButOneFolded game')
            liftIO $ print $ "isEveryoneAllin" ++ (show $ isEveryoneAllIn game')
            return (Nothing, game')

-- when no player action is possible we can can call this function to get the game 
-- to the next stage.
-- When the stage is showdown there are no possible player actions so this function is called
-- to progress the game to the next hand.
-- A similar situation occurs when no further player action is possible but  the game is not over
-- - in other words more than one players are active and all or all but one are all in
nextStage :: StateT Game IO (Maybe GameErr)
nextStage = do
  StateT $ \currGame@Game {..} -- should give error if not showdown as progress wont lead to new hand
   -> do
    game' <- progressGame currGame
    liftIO $ print game'
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
    , _maxBet = 50
    , _winners = NoWinners
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
handlePlayerAction game@Game {..} playerName =
  \case
    action@(PostBlind blind) ->
      maybe
        (Right $ postBlind blind playerName game)
        Left
        (validateAction game playerName action)
    action@Fold ->
      maybe
        (Right $ foldCards playerName game)
        Left
        (validateAction game playerName action)
    action@Call ->
      maybe
        (Right $ call playerName game)
        Left
        (validateAction game playerName action)
    action@(Raise amount) ->
      maybe
        (Right $ makeBet amount playerName game)
        Left
        (validateAction game playerName action)
    action@Check ->
      maybe
        (Right $ check playerName game)
        Left
        (validateAction game playerName action)
    action@(Bet amount) ->
      maybe
        (Right $ makeBet amount playerName game)
        Left
        (validateAction game playerName action)
    action@(Timeout) ->
      if isNothing $ canCheck playerName game
        then maybe
               (Right $ check playerName game)
               Left
               (validateAction game playerName action)
        else maybe
               (Right $ foldCards playerName game)
               Left
               (validateAction game playerName action)
    action@(SitDown player) -> seatPlayer game player
    action@LeaveSeat -> undefined

-- TODO should be able to choose seat
seatPlayer :: Game -> Player -> Either GameErr Game
seatPlayer Game {..} player@Player {..}
  | _playerName `elem` getPlayerNames _players =
    Left $ AlreadySatAtTable _playerName
  | length _players < _maxPlayers =
    Right Game {_players = _players <> [player], ..}
  | otherwise = Right $ Game {_waitlist = _waitlist <> [_playerName], ..}
