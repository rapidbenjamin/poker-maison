{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Socket.TimeAction where
import           Control.Concurrent      hiding ( yield )
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Data.Map.Lazy                 as M
import           Database.Persist.Postgresql
import qualified Network.WebSockets            as WS
import           Prelude

import           Database.Persist
import           Database.Persist.Postgresql    ( ConnectionString
                                                , SqlPersistT
                                                , runMigration
                                                , withPostgresqlConn
                                                )
import           Types

import           Control.Lens            hiding ( Fold )
import           Poker.Types             hiding ( LeaveSeat )


import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy.Char8    as C
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as X
import qualified Data.Text.Lazy.Encoding       as D
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.STM
import qualified Data.ByteString.Lazy          as BL

import           Socket.Types
import qualified Data.List                     as L

import           System.Random
import           Data.Map.Lazy                  ( Map )
import qualified Data.Map.Lazy                 as M

import           Poker.ActionValidation
import           Poker.Game.Blinds
import           Data.Either
import           System.Timeout
import           Poker.Game.Game
import           Poker.Types                    ( Player )
import           Data.Maybe
import           Poker.Game.Utils
import           Socket.Types
import           Socket.Utils
import           System.Random

import           Data.ByteString.UTF8           ( fromString )

import           Database
import           Schema

import qualified Data.Aeson                    as A

import           Data.ByteString.Lazy           ( fromStrict
                                                , toStrict
                                                )

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Hashable

import           Control.Monad
import           Control.Exception
import qualified GHC.IO.Exception              as G

import qualified Network.WebSockets            as WS

import           Pipes.Aeson
import           Pipes
import           Pipes.Core                     ( push )
import           Pipes.Concurrent
import           Pipes.Parse             hiding ( decode
                                                , encode
                                                )
import qualified Pipes.Prelude                 as P

import           Poker.Game.Privacy
import           Socket.Clients
import           Poker.Types
import           Poker.Poker

                
                
--  when playerHasToAct $ do


-----------------------------------------
-- VERSION 1 OF TIMEOUTS BELOW ---------

-- | Forks a thread which timeouts the player when they dont act in sufficient time
-- We fork this thread using async once a player sits down at a table
--
-- Duplicates the channel which reads table updates and starts a timeout if it is the players turn to act
-- If no valid game action is received from the socket msg read thread then we update the game with a special
-- timeout action which will usually check or fold the player.
--
-- Note that timeouts are only enforced when the expediency of the player's action is required
-- to avoid halting the progress of the game.
playerTimeoutLoop :: TableName -> TChan MsgOut -> MsgHandlerConfig -> IO ()
playerTimeoutLoop tableName channel msgHandlerConfig@MsgHandlerConfig {..} = do
  msgReaderDup <- atomically $ dupTChan socketReadChan
  dupTableChan <- atomically $ dupTChan channel
  forever $ do
    dupTableChanMsg <- atomically $ readTChan dupTableChan
    case dupTableChanMsg of
      (NewGameState tableName newGame) ->
        let playerHasToAct = doesPlayerHaveToAct (unUsername username) newGame
        in  when playerHasToAct $ awaitTimedPlayerAction socketReadChan
                                                         newGame
                                                         tableName
                                                         username
      _ -> return ()


-- We read from a duplicate of the channel which reads msgs from the client socket.
-- We use a duplicate to listen to the client so as to not consume messages intended for other tables
awaitValidPlayerAction
  :: TableName -> Game -> PlayerName -> TChan MsgIn -> STM MsgIn
awaitValidPlayerAction tableName game name dupChan =
  readTChan dupChan >>= \msg -> if isValidAction game name msg
    then return msg
    else awaitValidPlayerAction tableName game name dupChan
 where
  isValidAction game name = \case
    GameMsgIn (GameMove tableName' action) ->
      (isRight $ validateAction game name action) && tableName == tableName'
    _ -> False

-- Race the timer and the blocking read call which awaits a valid player msg.
-- The race results in an Either where the Left signals a timeout and a 
-- Right denotes a valid game action was received from the client in sufficient
-- time.
awaitTimedPlayerAction :: TChan MsgIn -> Game -> TableName -> Username -> IO ()
awaitTimedPlayerAction socketReadChan game tableName (Username playerName) = do
  delayTVar         <- registerDelay timeoutDuration
  dupChan           <- atomically $ dupTChan socketReadChan
  validPlayerAction <- async $ atomically $ awaitValidPlayerAction tableName
                                                                   game
                                                                   playerName
                                                                   dupChan
  timer           <- async $ atomically $ readTVar delayTVar >>= check
  msgInOrTimedOut <- waitEitherCancel timer validPlayerAction
  when (isLeft msgInOrTimedOut) $ atomically $ writeTChan
    socketReadChan
    (GameMsgIn $ GameMove tableName Timeout)
  where timeoutDuration = 6500000
