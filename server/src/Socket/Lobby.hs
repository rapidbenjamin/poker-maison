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
  g    <- getStdGen
  let shuffledDeck' = shuffledDeck g
  (output, input) <- spawn unbounded
  let tableName = "Black"
  let table' = Table { subscribers    = []
                     , gameInMailbox  = output
                     , gameOutMailbox = input
                     , waitlist       = []
                     , game           = initialGameState shuffledDeck'
                     , channel        = chan
                     }
  setUpTablePipes tableName table'
  return $ Lobby $ M.fromList [("Black", table')]
  where maxChanLength = 10000

joinGame :: Username -> Int -> Game -> Game
joinGame (Username username) chips Game {..} = Game
  { _players = _players <> [player]
  , ..
  }
  where player = initPlayer username chips

joinTableWaitlist :: Username -> Table -> Table
joinTableWaitlist username Table {..} =
  Table { waitlist = waitlist <> [username], .. }

updateTable :: TableName -> Table -> Lobby -> Lobby
updateTable tableName newTable (Lobby lobby) =
  Lobby $ M.insert tableName newTable lobby

-- to do - return an either as there are multiple errs for why plyr cant join game ie no chips
canJoinGame :: Game -> Bool
canJoinGame Game {..} = length _players < _maxPlayers

updateTableGame :: TableName -> Game -> Lobby -> Lobby
updateTableGame tableName newGame (Lobby lobby) = Lobby
  $ M.adjust updateTable tableName lobby
  where updateTable Table {..} = Table { game = newGame, .. }

summariseGame :: TableName -> Table -> TableSummary
summariseGame tableName Table { game = Game {..}, ..} = TableSummary
  { _tableName     = tableName
  , _playerCount   = length _players
  , _waitlistCount = length _waitlist
  , ..
  }

summariseTables :: Lobby -> [TableSummary]
summariseTables (Lobby lobby) = uncurry summariseGame <$> M.toList lobby


