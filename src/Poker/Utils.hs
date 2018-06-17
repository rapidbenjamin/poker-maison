{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.Utils where

------------------------------------------------------------------------------
import Control.Monad.State hiding (state)
import Data.Text (Text)

import Data.Functor
import Data.List
import Data.Maybe

import Control.Lens

------------------------------------------------------------------------------
import Poker.Types
import Poker.Types

------------------------------------------------------------------------------
import Control.Monad
import Data.Array.IO
import System.Random

shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newArray n xs
  forM [1 .. n] $ \i -> do
    j <- randomRIO (i, n)
    vi <- readArray ar i
    vj <- readArray ar j
    writeArray ar j vi
    return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs = newListArray (1, n) xs

modInc :: Int -> Int -> Int
modInc num modulo
  | incNum > modulo = 0
  | otherwise = incNum
  where
    incNum = num + 1
    modInc = incNum `mod` modulo

-- return players which have the ability to make further moves i.e not all in or folded
-- the distinction between sat in and active is important
-- if a player is sat out then there has been no historical participation in this hand 
-- as there can be no future participation in this hand 
-- whereas sat in means that the player has at the very least had some historical participation
-- in the current hand
getActivePlayers :: [Player] -> [Player]
getActivePlayers = filter ((== In) . (^. playerState))

-- get all players who are not currently sat out
getPlayersSatIn :: [Player] -> [Player]
getPlayersSatIn = filter ((/= None) . (^. playerState))

-- player position is the order of a given player in the set of all players with a 
-- playerState of In or in other words the players that are both sat at the table and active 
-- return Nothing if the given playerName is not sat at table
getPlayerPosition :: [PlayerName] -> PlayerName -> Maybe Int
getPlayerPosition playersSatIn playerName = playerName `elemIndex` playersSatIn

getMaxBet plyrs = maximum $ flip (^.) bet <$> (getActivePlayers plyrs)

getGameStage :: Game -> Street
getGameStage game = game ^. street

getGamePlayers :: Game -> [Player]
getGamePlayers game = game ^. players

getGamePlayer :: Game -> PlayerName -> Maybe Player
getGamePlayer game playerName =
  find (\Player {..} -> _playerName == playerName) $ _players game

getGamePlayerNames :: Game -> [Text]
getGamePlayerNames game = _playerName <$> _players game

getPlayerNames :: [Player] -> [Text]
getPlayerNames players = (^. playerName) <$> players
