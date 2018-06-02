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

module API where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)
import Data.Int (Int64)
import Data.Proxy (Proxy(..))
import Data.Typeable

import Database.Persist
import Database.Persist.Postgresql
import Network.Wai hiding (run)

import Auth (authHandler)
import Lobby
import Servant.API
import Servant.Client
import Servant.Server
import Servant.Server.Experimental.Auth
import Users

import Database
import Schema

type API = UsersAPI :<|> LoginRequiredAPI

type LoginRequiredAPI = LobbyAPI

loginRequiredAPI :: Proxy LoginRequiredAPI
loginRequiredAPI = Proxy :: Proxy LoginRequiredAPI

api :: Proxy API
api = Proxy :: Proxy API

server :: ConnectionString -> Server API
server connString =
  (usersServer connString) :<|> (loginRequiredServer connString)

loginRequiredServer :: ConnectionString -> Server LoginRequiredAPI
loginRequiredServer connString = lobbyServer connString

app :: ConnectionString -> Application
app connString = serveWithContext api serverAuthContext (server connString)
  where
    serverAuthContext :: Context (AuthHandler Request User ': '[])
    serverAuthContext = (authHandler connString) :. EmptyContext
