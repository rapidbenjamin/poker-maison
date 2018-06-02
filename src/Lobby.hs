{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lobby where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)
import Data.Int (Int64)
import Data.Proxy (Proxy(..))
import Data.Typeable

import Database.Persist
import Database.Persist.Postgresql
import Network.Wai hiding (run)

import Auth (authHandler)
import Servant.API
import Servant.Client
import qualified Servant.Docs as Docs
import Servant.JS
import Servant.Server
import Servant.Server.Experimental.Auth
import Types
import Users

import Database
import Schema

type LobbyAPI = "lobby" :> AuthProtect "JWT" :> Get '[ JSON] UserProfile

lobbyAPI :: Proxy LobbyAPI
lobbyAPI = Proxy :: Proxy LobbyAPI

--lobbyAPIDocs = Docs.markdown $ Docs.docs lobbyAPI
lobbyServer :: ConnectionString -> Server LobbyAPI
lobbyServer connString = (h connString)

h :: ConnectionString -> User -> Handler UserProfile
h connString User {..} =
  return
    UserProfile
      { proEmail = userEmail
      , proChips = userChips
      , proUsername = Username userUsername
      }
