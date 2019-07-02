{-# LANGUAGE RecordWildCards #-}

module Socket.Table where
import           Control.Concurrent      hiding ( yield )
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Data.Map.Lazy                 as M
import           Database.Persist.Postgresql    ( ConnectionString )
import qualified Network.WebSockets            as WS
import           Prelude

import           Socket.Clients
import           Socket.Lobby
import           Socket.Msg
import           Socket.Setup
import           Socket.Subscriptions
import           Socket.Types
import           Socket.Workers
import           Types

import           Control.Lens            hiding ( Fold )
import           Poker.Types             hiding ( LeaveSeat )
--import           Data.Traversable


import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy.Char8    as C
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as X
import qualified Data.Text.Lazy.Encoding       as D
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.STM
--import           Control.Monad.State.Lazy hiding (evalStateT)
import qualified Data.ByteString.Lazy          as BL

import           Socket.Types
import qualified Data.List                     as L
import qualified Data.Text.Lazy                as X

import qualified Data.Text.Lazy.Encoding       as D
import           System.Random

import           Poker.ActionValidation
import           Poker.Game.Blinds
import           Data.Either
import           System.Timeout
import           Poker.Game.Game
import           Poker.Types                    ( Player )
import           Data.Maybe
import           Poker.Game.Utils
import           Socket
import           Socket.Types
import           Socket.Msg
import           Socket.Utils
import           Data.ByteString.UTF8           ( fromString )

import           Poker.Poker
import           Crypto.JWT
import qualified Data.Aeson                    as A

import           Data.ByteString.Lazy           ( fromStrict
                                                , toStrict
                                                )
--import           Data.Conduit                   ( Conduit
--                                                , runConduitRes
--                                                , yieldM
--                                                , (.|)
--                                                )
--
--import qualified Data.Conduit.List             as CL
-- import           Conduit
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Poker.Types

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



