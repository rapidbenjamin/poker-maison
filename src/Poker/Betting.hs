{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.Betting where

------------------------------------------------------------------------------
import Control.Monad.State hiding (state)
import Data.Char (toLower)
import Text.Read (readMaybe)

------------------------------------------------------------------------------
import Poker.Types
import Poker.Utils

postBlind g p b = undefined
{-
postBlind :: Game -> PlayerName -> Game -> Game
postBlind game@Game {..} playerName blind = undefined
 where betAmount = case blind of 
      Small -> _smallBlind game
      Big -> _bigBlind game 
    newPlayers = (\player -> if name == _playerName player then bet then <$>
-}
