{-
  Functions for excluding sensitive game data from game state.
-}
{-# LANGUAGE RecordWildCards #-}

module Poker.Game.Privacy where

import Data.Text (Text)

import Data.Functor
import Data.List
import Data.Maybe
import Data.Monoid

import Control.Lens

import Poker.Game.Game (isEveryoneAllIn)
import Poker.Types

-- For players that are sat in game
excludeOtherPlayerCards :: PlayerName -> Game -> Game
excludeOtherPlayerCards playerName = excludePrivateCards $ Just playerName

-- For spectators who aren't in game
excludeAllPlayerCards :: Game -> Game
excludeAllPlayerCards = excludePrivateCards Nothing

-- Exclude player cards and Deck so spectators can't see private cards.
--
-- Takes an optional playerName. If no playerName is given then all private
-- cards are excluded. However if playerName is given then their cards
-- will not be excluded.
--
-- So if a game update is going to be sent to a user then we pass in his playerName
-- so that information that is private to him is not excluded from the 
-- Game state (his pocket cards)
--
-- If everyone in the game is AllIn then their pocket cards should all be visible.
excludePrivateCards :: Maybe PlayerName -> Game -> Game
excludePrivateCards maybePlayerName game =
  game & (players %~ (<$>) pocketCardsPrivacyModifier) . (deck .~ Deck [])
  where
    everyoneAllIn = isEveryoneAllIn game
    pocketCardsPrivacyModifier =
      maybe
        (updatePocketCardsForSpectator everyoneAllIn)
        (updatePocketCardsForPlayer everyoneAllIn)
        maybePlayerName

-- Takes a boolean denoting if everyone is all in. If they are
-- then all the pocket cards helld by players whose _playerState 
-- is set to In (active and) are public and therefore not removed.
updatePocketCardsForSpectator :: Bool -> (Player -> Player)
updatePocketCardsForSpectator isEveryoneAllIn
  | isEveryoneAllIn =
    (\player@Player {..} ->
       if _playerState == In
         then player
         else Player {_pockets = PocketCards [], ..})
  | otherwise = (\Player {..} -> Player {_pockets = PocketCards [], ..})

updatePocketCardsForPlayer :: Bool -> PlayerName -> (Player -> Player)
updatePocketCardsForPlayer isEveryoneAllIn playerName
  | isEveryoneAllIn =
    (\player@Player {..} ->
       if _playerState == In
         then player
         else if _playerName == playerName
                then player
                else Player {_pockets = PocketCards [], ..})
  | otherwise =
    (\player@Player {..} ->
       if _playerName == playerName
         then player
         else Player {_pockets = PocketCards [], ..})
