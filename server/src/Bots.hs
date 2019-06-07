{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE ScopedTypeVariables #-}

module Bots where

import Control.Applicative

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import Control.Exception

import Control.Lens hiding (Fold)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.STM
import Control.Monad.State.Lazy

import Data.Either
import Data.Foldable
import Data.Functor
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Postgresql (ConnectionString)
import qualified Network.WebSockets as WS

import System.Random

import Prelude

import Debug.Trace

import Text.Pretty.Simple (pPrint)

import Control.Concurrent.Async
import Poker.ActionValidation
import Poker.Game.Game
import Poker.Game.Utils
import Poker.Poker
import Poker.Types hiding (LeaveSeat)
import Schema
import Socket.Clients
import Socket.Lobby
import Socket.Subscriptions
import Socket.Types
import Socket.Utils

import Types

actions :: Int -> [PlayerAction]
actions chips = [PostBlind Big, PostBlind Small, Check, Call, Fold, Bet chips, Raise chips]


getValidAction :: Game -> PlayerName -> PlayerAction -> IO PlayerAction
getValidAction g@Game{..} name action =  do
    betAmount' <- randomRIO (lowerBetBound, _chips)
    let validActions = filter (isRight . validateAction g name) (actions betAmount')
    if null validActions then panic else return ()
    randIx <- randomRIO (0, length validActions - 1)
    return $ validActions !! randIx
  where 
      lowerBetBound = if (_bet > 0) then (2 * _bet) else 1
      Player{..} = fromJust $ getGamePlayer g name
      panic = do
        print $ "UHOH no valid actions for " <> show name
        print g
        error $ "UHOH no valid actions"

chooseAction = undefined
