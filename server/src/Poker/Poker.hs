{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Poker.Poker
  ( initialGameState
  , initPlayer
  , progressGame
  , canProgressGame
  , runPlayerAction
  , handlePlayerTimeout
  ) where

import Control.Lens hiding (Fold)
import Control.Concurrent.STM.TVar

import Data.Either
import Data.Functor
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (Text)

import Poker.ActionValidation
import Poker.Game.Actions
import Poker.Game.Blinds
import Poker.Game.Game
import Poker.Game.Hands
import Poker.Game.Utils
import Poker.Types

-- this is public api of the poker module 
-- the function takes a player action and returns either a new game for a valid 
-- player action or an err signifying an invalid player action with the reason why
-- if the current game stage is showdown then the next game state will have a newly shuffled
-- deck and pocket cards/ bets reset
runPlayerAction ::
     Game -> PlayerName -> PlayerAction -> IO (Either GameErr Game)
runPlayerAction currGame@Game {..} playerName action =
    case handlePlayerAction currGame playerName action of
      Left err -> return $ Left err
      Right newGameState ->
        case action of
          SitDown _ -> return $ Right newGameState
          LeaveSeat' -> return $ Right newGameState
          _ -> do 
              nextStage <- progressGame newGameState
              return $ Right nextStage

  
canProgressGame :: Game -> Bool
canProgressGame game@Game{..}
    | _street == Showdown = True
    | _street == PreDeal = haveRequiredBlindsBeenPosted game
    | otherwise = haveAllPlayersActed game 

    --  ((length $ getActivePlayers _players) < 2 && haveAllPlayersActed game) ||
--  (haveAllPlayersActed game && (length $ getActivePlayers _players) >= 2))

-- when no player action is possible we can can call this function to get the game 
-- to the next stage.
-- When the stage is showdown there are no possible player actions so this function is called
-- to progress the game to the next hand.
-- A similar situation occurs when no further player action is possible but  the game is not over
-- - in other words more than one players are active and all or all but one are all in


-- | Just get the identity function if not all players acted otherwise we return 
-- the function necessary to progress the game to the next stage.
-- toDO - make function pure by taking stdGen as an arg
progressGame :: Game -> IO Game
progressGame game@Game {..}
  | _street == Showdown = getNextHand game <$> shuffledDeck
  | _street == PreDeal && haveAllPlayersActed game && numberPlayersSatIn < 2 =
    getNextHand game <$> shuffledDeck
  | haveAllPlayersActed game &&
      (not (allButOneFolded game) || (_street == PreDeal || _street == Showdown)) =
    case getNextStreet _street of
      PreFlop -> return $ progressToPreFlop game
      Flop -> return $ progressToFlop game
      Turn -> return $ progressToTurn game
      River -> return $ progressToRiver game
      Showdown -> return $ progressToShowdown game
      PreDeal -> getNextHand game <$> shuffledDeck
  | allButOneFolded game && _street /= Showdown =
    return $ progressToShowdown game
  | otherwise = return game
  where
    numberPlayersSatIn = length $ getActivePlayers _players

handlePlayerAction :: Game -> PlayerName -> PlayerAction -> Either GameErr Game
handlePlayerAction game@Game {..} playerName =
  \case
    action@(PostBlind blind) ->
      validateAction game playerName action $> postBlind blind playerName game
    action@Fold ->
      validateAction game playerName action $> foldCards playerName game
    action@Call -> validateAction game playerName action $> call playerName game
    action@(Raise amount) ->
      validateAction game playerName action $> makeBet amount playerName game
    action@Check ->
      validateAction game playerName action $> check playerName game
    action@(Bet amount) ->
      validateAction game playerName action $> makeBet amount playerName game
    action@Timeout -> handlePlayerTimeout playerName game
    action@(SitDown player) ->
      validateAction game playerName action $> seatPlayer player game
    action@SitIn ->
      validateAction game playerName action $> sitIn playerName game
    action@LeaveSeat' ->
      validateAction game playerName action $> leaveSeat playerName game

-- TODO - "Except" or ExceptT Identity has a more reliable Alternative instance.
-- Use except and remove the guards and just use <|> to combine all the 
-- eithers and return the first right. I.e try each action in turn and return the first
-- valid action. 
handlePlayerTimeout :: PlayerName -> Game -> Either GameErr Game
handlePlayerTimeout playerName game@Game {..}
  | playerCanCheck && handStarted =
    validateAction game playerName Check $> check playerName game
  | not playerCanCheck && handStarted =
    validateAction game playerName Timeout $> foldCards playerName game
  | not handStarted =
    validateAction game playerName SitOut $> sitOut playerName game
  where
    handStarted = _street /= PreDeal
    playerCanCheck = isRight $ canCheck playerName game


initialGameState :: Deck -> Game
initialGameState shuffledDeck =
  Game
    { _players = []
    , _waitlist = []
    , _minBuyInChips = 1500
    , _maxBuyInChips = 3000
    , _maxPlayers = 5
    , _dealer = 0
    , _currentPosToAct = 1 -- position here refers to the zero indexed set of active users
    , _board = []
    , _deck = shuffledDeck
    , _smallBlind = 25
    , _bigBlind = 50
    , _pot = 0
    , _street = PreDeal
    , _maxBet = 0
    , _winners = NoWinners
    }
