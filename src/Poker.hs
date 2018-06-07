{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Poker where

------------------------------------------------------------------------------
import Control.Monad.State.Lazy
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (Text)

------------------------------------------------------------------------------
import Poker.Betting
import Poker.Game
import Poker.Hands
import Poker.Types

newGame :: Game -> State Game (Maybe GameErr)
newGame initState = state $ \_ -> (Nothing, initialGameState)

-- this is public api of the poker module 
-- the function takes a player action and returns either a new game for a valid 
-- player action or an err signifying an invalid player action with the reason why
progressGame :: PlayerName -> PlayerAction -> StateT Game IO (Maybe GameErr)
progressGame playerName action =
  StateT $ \currGameState ->
    case isPlayerActingOutofTurn currGameState playerName of
      Just err -> return (Just $ OutOfTurn playerName err, currGameState)
      Nothing ->
        case handlePlayerAction currGameState playerName action of
          Left err -> return (Just err, currGameState)
          Right newGameState ->
            return (Nothing, nextHandIfShowdown newGameState)

------------------------------------------------------------------------------
initialGameState :: Game
initialGameState =
  Game
    { players = []
    , waitlist = []
    , maxPlayers = 5
    , dealer = 0
    , currentPosToAct = 0 -- position here refes to the zero indexed set of active users
    , community = []
    , deck = initialDeck
    , smallBlind = 25
    , bigBlind = 50
    , pot = 0
    , street = PreDeal
    , maxBet = 0
    }

-- initially a players state is set to None to denote that they havent posted their blinds yet
getPlayer :: Text -> Int -> Player
getPlayer playerName chips =
  Player
    { pockets = []
    , bet = 0
    , playerState = None
    , playerName = playerName
    , committed = 0
    , chips = chips
    }

nextHandIfShowdown :: Game -> Game
nextHandIfShowdown game@Game {..}
  | street == Showdown = getNextHand game
  | otherwise = game

getGameStage :: Game -> Street
getGameStage Game {..} = street

getGamePlayers :: Game -> [Player]
getGamePlayers Game {..} = players

getGamePlayer :: Game -> PlayerName -> Maybe Player
getGamePlayer Game {..} playerName =
  find (\Player {..} -> playerName == playerName) players

getGamePlayerNames :: Game -> [Text]
getGamePlayerNames Game {..} = (\Player {..} -> playerName) <$> players

getPlayerNames :: Functor f => f Player -> f Text
getPlayerNames players = (\Player {..} -> playerName) <$> players

handlePlayerAction :: Game -> PlayerName -> PlayerAction -> Either GameErr Game
handlePlayerAction game _ action@(TakeSeat player) =
  Right $ takeSeat game player
handlePlayerAction game playerName action@LeaveSeat {} = undefined
handlePlayerAction game playerName action@PostBlind {} = undefined
handlePlayerAction game playerName action@Fold {} = undefined
handlePlayerAction game playerName action@Call {} = undefined
handlePlayerAction game playerName action@Raise {} = undefined
handlePlayerAction game playerName action@Check {} = undefined
handlePlayerAction game playerName action@Bet {} = undefined

-- if player taking the action is not the current player to act then send back an err wrapped in Just
-- otherwise if the player is making an action in turn then return Nothing to signify the absence of error
isPlayerActingOutofTurn :: Game -> PlayerName -> Maybe CurrentPlayerToActErr
isPlayerActingOutofTurn Game {..} playerName =
  if currentPlayerToAct == playerName
    then Nothing
    else Just $ CurrentPlayerToActErr currentPlayerToAct
  where
    activePlayers = getPlayerNames $ getActivePlayers players
    currentPlayerToAct = activePlayers !! currentPosToAct

-- TODO should be able to choose seat
takeSeat :: Game -> Player -> Game
takeSeat Game {..} player@Player {..}
  | length players < maxPlayers = Game {players = players <> [player], ..}
  | otherwise = Game {waitlist = waitlist <> [playerName], ..}

-- if a player does not post their blind at the appropriate time then their state will be changed to 
--None signifying that they have a seat but are now sat out
-- blind is required either if player is sitting in bigBlind or smallBlind position relative to dealer
-- or if their current playerState is set to Out 
-- If no blind is required for the player to remain In for the next hand then we will return Nothing
blindRequiredByPlayer :: Game -> Text -> Maybe Blind
blindRequiredByPlayer game@Game {..} playerName = do
  Player {..} <- getGamePlayer game playerName
  case playerState of
    None -> Just Big
    _ -> do
      playerPosition <- getPlayerPosition (getPlayerNames players) playerName
      let playersSatIn = getPlayerNames $ getPlayersSatIn players
      let smallBlindPos = getSmallBlindPosition playersSatIn dealer
      let bigBlindPos = smallBlindPos `modInc` length playersSatIn
      if playerPosition == smallBlindPos
        then Just Small
        else if playerPosition == bigBlindPos
               then Just Big
               else Nothing

-- return players which have the ability to make further moves i.e not all in or folded
-- the distinction between sat in and active is important
-- if a player is sat out then there has been no historical participation in this hand 
-- as there can be no future participation in this hand 
-- whereas sat in means that the player has at the very least had some historical participation
-- in the current hand
getActivePlayers :: [Player] -> [Player]
getActivePlayers = filter (\Player {..} -> playerState == In)

-- get all players who are not currently sat out
getPlayersSatIn :: [Player] -> [Player]
getPlayersSatIn = filter (\Player {..} -> playerState /= None)

-- player position is the order of a given player in the set of all players with a 
-- playerState of In or in other words the players that are both sat at the table and active 
-- return Nothing if the given playerName is not sat at table
getPlayerPosition :: [PlayerName] -> PlayerName -> Maybe Int
getPlayerPosition playersSatIn playerName = playerName `elemIndex` playersSatIn

getSmallBlindPosition :: [Text] -> Int -> Int
getSmallBlindPosition playersSatIn dealerPos =
  modInc dealerPos (length playersSatIn)

-- the posting of a blind by a player 
-- will ensure that their player state is set to In and they will be dealt the next hand
handleBlindAction :: Game -> PlayerName -> Blind -> Either GameErr Game
handleBlindAction game@Game {..} playerName _
  | not atTable = Left $ NotAtTable playerName
  where
    playerNames = getGamePlayerNames game
    atTable = playerName `elem` playerNames
handleBlindAction game@Game {..} playerName _
  | isNothing $ blindRequiredByPlayer game playerName =
    Left $ InvalidMove playerName BlindNotRequired
handleBlindAction game@Game {..} playerName blind =
  case fromJust blindRequired of
    Small ->
      if blind == Small
        then postBlind game playerName blind
        else Left $ InvalidMove playerName $ BlindRequired Small
    Big ->
      if blind == Big
        then postBlind game playerName blind
        else Left $ InvalidMove playerName $ BlindRequired Big
  where
    blindRequired = blindRequiredByPlayer game playerName

-- reset hand related state
-- TODO move players from waitlist to players list
-- TODO need to send msg to players on waitlist when a seat frees up to inform them 
-- to choose a seat and set limit for them t pick one
-- TODO - have newBlindNeeded field which new players will initially be put into in order to 
-- ensure they cant play without posting a blind before the blind position comes round to them
-- new players can of course post their blinds early. In the case of an early posting the initial
-- blind must be the big blind. After this 'early' blind or the posting of a normal blind in turn the 
-- new player will be removed from the newBlindNeeded field and can play normally.
getNextHand :: Game -> Game
getNextHand Game {..} =
  Game
    { waitlist = newWaitlist
    , maxBet = 0
    , players = newPlayers
    , community = []
    , street = PreDeal
    , dealer = newDealer
    , currentPosToAct = nextPlayerToAct
    , ..
    }
  where
    newDealer = dealer `modInc` length (getPlayersSatIn players)
    freeSeatsNo = maxPlayers - length players
    newPlayers = resetPlayerCardsAndBets <$> players
    newWaitlist = drop freeSeatsNo waitlist
    nextPlayerToAct = currentPosToAct `modInc` length newPlayers

modInc :: Integral p => p -> p -> p
modInc num modulo
  | incNum == 0 = modInc
  | otherwise = incNum
  where
    incNum = num + 1
    modInc = incNum `mod` modulo

resetPlayerCardsAndBets :: Player -> Player
resetPlayerCardsAndBets Player {..} =
  Player {pockets = [], bet = 0, committed = 0, ..}
