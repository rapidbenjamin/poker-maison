{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Socket.Msg
  ( msgHandler
  ) where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, readMVar)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.List
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import qualified Network.WebSockets as WS
import Prelude
import Socket.Types
import Socket.Utils
import Text.Pretty.Simple (pPrint)
import Types

msgHandler :: Msg -> ReaderT MsgHandlerConfig IO ()
msgHandler msg = undefined
