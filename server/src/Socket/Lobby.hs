{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Socket.Lobby where

import           Control.Concurrent             ( MVar
                                                , modifyMVar
                                                , modifyMVar_
                                                , readMVar
                                                )
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad.STM

import Control.Lens
import Control.Lens.At

import           Control.Monad                  ( void )
import           Control.Monad.Except
import           Control.Monad.Logger           ( LoggingT
                                                , runStdoutLoggingT
                                                )
import           Control.Monad.Reader
import           Data.ByteString.Char8          ( pack
                                                , unpack
                                                )
import           Data.Int                       ( Int64 )
import           Data.List                      ( unfoldr )
import           Data.Map.Lazy                  ( Map )
import qualified Data.Map.Lazy                 as M
import           Data.Monoid
import           Data.Text                      ( Text )
import           Poker.Game.Utils
import           Poker.Poker
import           Poker.Types
import           Socket.Clients
import           Socket.Types
import           Socket.Utils
import           Types
import           Pipes.Concurrent
import           System.Random
import           Socket.Table

initialLobby :: IO Lobby
initialLobby = do
  chan <- atomically newBroadcastTChan
  randGen   <- getStdGen
  let shuffledDeck' = shuffledDeck randGen
  (output, input) <- spawn $ newest 1
  let tableName = "Black"
  let table' = Table { subscribers    = []
                     , gameInMailbox  = output
                     , gameOutMailbox = input
                     , waitlist       = []
                     , game           = initialGameState shuffledDeck'
                     , channel        = chan
                     }
  return $ Lobby $ M.fromList [("Black", table')]

joinGame :: Username -> Int -> Game -> Game
joinGame (Username username) chips Game {..} = 
    Game { _players = _players <> pure player , .. }
  where player = initPlayer username chips

joinTableWaitlist :: Username -> Table -> Table
joinTableWaitlist username Table {..} =
  Table { waitlist = waitlist <> [username], .. }

insertTable :: TableName -> Table -> Lobby -> Lobby
insertTable tableName newTable = Lobby . (at tableName .~ Just newTable) . unLobby

-- to do - return an either as there are multiple errs for why plyr cant join game ie no chips
canJoinGame :: Game -> Bool
canJoinGame Game {..} = length _players < _maxPlayers

summariseGame :: TableName -> Table -> TableSummary
summariseGame tableName Table { game = Game {..}, ..} = TableSummary
  { _tableName     = tableName
  , _playerCount   = length _players
  , _waitlistCount = length _waitlist
  , ..
  }

summariseTables :: Lobby -> [TableSummary]
summariseTables (Lobby lobby) = uncurry summariseGame <$> M.toList lobby


