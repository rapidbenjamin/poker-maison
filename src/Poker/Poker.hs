{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Poker.Poker
  ( initialGameState
  , getPlayer
  , nextStage
  , runPlayerAction
  ) where

import Control.Lens hiding (Fold)
import Control.Monad.State.Lazy
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

newGame :: Game -> State Game (Maybe GameErr)
newGame initState = state $ const (Nothing, initialGameState)

-- this is public api of the poker module 
-- the function takes a player action and returns either a new game for a valid 
-- player action or an err signifying an invalid player action with the reason why
-- if the current game stage is showdown then the next game state will have a newly shuffled
-- deck and pocket cards/ bets reset
runPlayerAction ::
     PlayerName -> PlayerAction -> StateT Game IO (Either GameErr ())
runPlayerAction playerName action =
  StateT $ \currGame@Game {..} ->
    case handlePlayerAction currGame playerName action of
      Left err -> return (Left err, currGame)
      Right newGameState ->
        case action of
          SitDown _ -> return (Right (), newGameState)
          LeaveSeat -> return (Right (), newGameState)
          _ -> do
            game' <- progressGame newGameState
            return (Right (), game')

-- when no player action is possible we can can call this function to get the game 
-- to the next stage.
-- When the stage is showdown there are no possible player actions so this function is called
-- to progress the game to the next hand.
-- A similar situation occurs when no further player action is possible but  the game is not over
-- - in other words more than one players are active and all or all but one are all in
nextStage :: StateT Game IO (Either GameErr ())
nextStage =
  StateT $ \currGame@Game {..} -> do
    game' <- progressGame currGame
    return (Right (), game')

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
    action@Timeout ->
      if isRight $ canCheck playerName game
        then validateAction game playerName action $> check playerName game
        else validateAction game playerName action $> foldCards playerName game
    action@(SitDown player) ->
      validateAction game playerName action $> seatPlayer player game
    action@LeaveSeat ->
      validateAction game playerName action $> leaveSeat playerName game

-- | Just get the identity function if not all players acted otherwise we return 
-- the function necessary to progress the game to the next stage.
-- toDO - make function pure by taking stdGen as an arg
progressGame :: Game -> IO Game
progressGame game@Game {..}
  | haveAllPlayersActed game &&
      (not (allButOneFolded game) || (_street == PreDeal || _street == Showdown)) =
    case getNextStreet _street of
      PreFlop -> return $ progressToPreFlop game
      Flop -> return $ progressToFlop game
      Turn -> return $ progressToTurn game
      River -> return $ progressToRiver game
      Showdown -> return $ progressToShowdown game
      PreDeal -> getNextHand game <$> shuffle initialDeck
  | allButOneFolded game && (_street /= PreDeal || _street /= Showdown) =
    return $ progressToShowdown game
  | otherwise = return game
