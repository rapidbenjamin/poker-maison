{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Socket.Msg
  ( msgHandler
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import Prelude
import Text.Pretty.Simple (pPrint)

import Socket.Types
import Types

msgHandler :: Msg -> ReaderT MsgHandlerConfig IO ()
msgHandler msg = undefined
