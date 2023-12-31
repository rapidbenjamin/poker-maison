{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Poker.Game.Utils where

import Control.Lens ((^.))
import Data.Bool (bool)
import Data.Foldable (find)
import Data.List (elemIndex, find)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Text (Text)
import Poker.Types
import System.Random (Random (randomR), RandomGen)

-- | A standard deck of cards.
initialDeck :: Deck
initialDeck = Deck $ Card <$> [minBound ..] <*> [minBound ..]

-- Get a shuffled deck of cards.
shuffledDeck :: RandomGen g => g -> Deck
shuffledDeck gen = Deck <$> fst $ shuffle gen (unDeck initialDeck)

fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) =
  ((M.insert j x . M.insert i (m M.! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

-- shuffle using the Fisher Yates algorithm
shuffle :: RandomGen g => g -> [a] -> ([a], g)
shuffle gen [] = ([], gen)
shuffle gen l =
  toElems $
    foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (M.elems x, y)
    numerate = zip [1 ..]
    initial x gen = (M.singleton 0 x, gen)

modInc :: Int -> Int -> Int -> Int
modInc incAmount num modulo
  | incNum > modulo = 0
  | otherwise = incNum
  where
    incNum = num + incAmount
    modInc = incNum `mod` modulo

modDec :: Int -> Int -> Int
modDec num modulo
  | decNum < modulo = 0
  | otherwise = decNum
  where
    decNum = num - 1
    modInc = decNum `mod` modulo

-- return players which have the ability to make further moves i.e not all in or folded
-- the distinction between sat in and active is important
-- if a player is sat out then there has been no historical participation in this hand
-- as there can be no future participation in this hand
-- whereas sat in means that the player has at the very least had some historical participation
-- in the current hand
getActivePlayers :: [Player] -> [Player]
getActivePlayers = filter ((==) PlayerCanAct . canAct . _playerStatus)

filterPlayersWithLtChips :: Int -> [Player] -> [Player]
filterPlayersWithLtChips count =
  filter
    ( \Player {..} ->
        unChips _chips >= count
    )

filterSatOutPlayers :: [Player] -> [Player]
filterSatOutPlayers = filter (\Player {..} -> _playerStatus /= SatOut)

countActive :: [Player] -> Int
countActive = length . getActivePlayers

canAct :: PlayerStatus -> CanPlayerAct
canAct (InHand (CanAct _)) = PlayerCanAct
canAct _ = PlayerCannotAct

canPlayersAct :: Functor f => f Player -> f CanPlayerAct
canPlayersAct ps = canAct . _playerStatus <$> ps

canAnyPlayerAct :: [Player] -> Bool
canAnyPlayerAct = elem PlayerCanAct . canPlayersAct

bettingActionStatus :: [Player] -> BettingAction
bettingActionStatus ps
  | allButOneFolded ps = EveryoneFolded
  | playersNotAllIn ps == 1 = EveryoneAllIn
  | canAnyPlayerAct ps = AwaitingPlayerAction
  | not (canAnyPlayerAct ps) = NotAwaitingPlayerAction
  | otherwise = error "undhandled guard"

allButOneAllIn :: [Player] -> Bool
allButOneAllIn = (== 1) . playersNotAllIn

playersNotAllIn :: [Player] -> Int
playersNotAllIn ps
  | numPlayersIn < 2 = 0
  | otherwise = numPlayersIn - numPlayersAllIn
  where
    numPlayersIn = length $ getActivePlayers ps
    numPlayersAllIn =
      length $ filter (\Player {..} -> _playerStatus == InHand AllIn) ps

-- The game should go straight to showdown if all but one players is In hand
allButOneFolded :: [Player] -> Bool
allButOneFolded ps = length playersInHand <= 1
  where
    playersInHand = filter ((== InHand Folded) . (^. playerStatus)) ps

-- get all players who are not currently sat out
getPlayersSatIn :: [Player] -> [Player]
getPlayersSatIn = filter ((/= SatOut) . (^. playerStatus))

-- player position is the order of a given player in the set of all players with a
-- playerStatus of In or in other words the players that are both sat at the table and active
-- return Nothing if the given playerName is not sat at table
getPlayerPosition :: [PlayerName] -> PlayerName -> Maybe Int
getPlayerPosition playersSatIn playerName = playerName `elemIndex` playersSatIn

getPlayerPosition' :: PlayerName -> [Player] -> Maybe Int
getPlayerPosition' playerName = flip getPlayerPosition playerName . getPlayerNames . getPlayersSatIn

getGameStage :: Game -> Street
getGameStage game = game ^. street

getGamePlayers :: Game -> [Player]
getGamePlayers game = game ^. players

getGamePlayer :: Game -> PlayerName -> Maybe Player
getGamePlayer game playerName =
  find (\Player {..} -> _playerName == playerName) $ _players game

getGamePlayerState :: Game -> PlayerName -> Maybe PlayerStatus
getGamePlayerState game playerName = do
  Player {..} <- getGamePlayer game playerName
  return _playerStatus

getGamePlayerNames :: Game -> [Text]
getGamePlayerNames game = _playerName <$> _players game

getPlayerChipCounts :: Game -> [(Text, Int)]
getPlayerChipCounts Game {..} =
  (\Player {..} -> (_playerName, unChips _chips)) <$> _players

getPlayerNames :: [Player] -> [Text]
getPlayerNames players = (^. playerName) <$> players

-- Nothing for currentPosToAct during Predeal means that the first blind
-- can be posted from any position as this is the first blind to get a new game started
-- on the otherhand a value of Just pos means that pos is the position that we require a blind to
-- be posted from next as a game is underway.
inPositionToAct :: PlayerName -> Game -> Bool
inPositionToAct playerName Game {..} =
  case playerPos of
    Nothing -> False
    Just pos -> case _currentPosToAct of
      Nothing -> _street == PreDeal -- Wheareas Nothing during Predeal means anyone can act
      -- Nothing in currentPostToAct field after predeal means no player can act.
      Just posToAct -> pos == posToAct
  where
    playerPos = getPlayerPosition' playerName _players

maximums :: Ord a => [(a, b)] -> [(a, b)]
maximums [] = []
maximums (x : xs) = foldl f [x] xs
  where
    f ys y = case fst (head ys) `compare` fst y of
      GT -> ys
      EQ -> y : ys
      LT -> [y]
