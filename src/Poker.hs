{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Poker where

------------------------------------------------------------------------------
import Control.Monad.State
import Data.Text (Text)

------------------------------------------------------------------------------
import Poker.Betting
import Poker.Game
import Poker.Hands
import Poker.Types

------------------------------------------------------------------------------
initialGameState :: Game
initialGameState =
  Game
    { players = []
    , maxPlayers = 5
    , community = []
    , deck = initialDeck
    , pot = 0
    , street = PreDeal
    , maxBet = 0
    }

initialPlayer :: Text -> Int -> Player 
initialPlayer playerName chips = Player { pockets = [], bet = 0, playerState=None, playerName=playerName, committed=0}

newGame :: Game -> State Game (Maybe GameErr)
newGame initState = state $ \_ -> (Nothing, initialGameState)

progress :: PlayerMove -> State Game (Maybe GameErr)
progress move =
  state $ \currGameState -> do
    case makePlayerMove currGameState move of
      Left err -> (Just err, currGameState)
      Right newGameState -> (Nothing, newGameState)

getGameStage Game{..} = street

makePlayerMove :: Game -> PlayerMove -> Either GameErr Game
makePlayerMove game (Fold playerName) = undefined
makePlayerMove game (Call playerName) = undefined
makePlayerMove game (Raise playerName amount) = undefined
makePlayerMove game (Check playerName) = undefined
makePlayerMove game (Bet playerName amount) = undefined
