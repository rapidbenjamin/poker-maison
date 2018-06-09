{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.Betting where

import Control.Lens

------------------------------------------------------------------------------
import Control.Monad.State hiding (state)
import Data.Char (toLower)
import Text.Read (readMaybe)

------------------------------------------------------------------------------
import Poker.Types
import Poker.Utils

postBlind :: Game -> PlayerName -> Blind -> Game
postBlind game@Game {..} pName blind =
  case getGamePlayer game pName of
    Nothing -> game
    Just Player {..} ->
      let betAmount =
            case blind of
              Small -> _smallBlind
              Big -> _bigBlind
          newPlayerState =
            if betAmount == _chips
              then Out AllIn
              else _playerState
          newPlayer =
            Player
              { _playerState = newPlayerState
              , _chips = _chips - betAmount
              , _bet = betAmount
              , _committed = betAmount
              , ..
              }
          newPlayers =
            (\p ->
               if p ^. playerName == _playerName
                 then newPlayer
                 else p) <$>
            _players
       in Game {_players = newPlayers, ..}
