{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Poker.Betting where

------------------------------------------------------------------------------
import Control.Monad.State hiding (state)
import Data.Char (toLower)
import Text.Read (readMaybe)

------------------------------------------------------------------------------
import Poker.Types
import Poker.Utils

postBlind game playerName blind = return $ game
