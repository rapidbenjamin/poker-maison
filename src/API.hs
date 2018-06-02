{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module API where

import Data.Proxy (Proxy(..))
import Database.Persist.Postgresql
import Network.Wai
import Servant.Server
import Servant.Server.Experimental.Auth

import Auth (authHandler)
import Schema
import Users

type API = UsersAPI

api :: Proxy API
api = Proxy :: Proxy API

server :: ConnectionString -> Server API
server = usersServer

app :: ConnectionString -> Application
app connString = serveWithContext api serverAuthContext (server connString)
  where
    serverAuthContext :: Context (AuthHandler Request User ': '[])
    serverAuthContext = authHandler connString :. EmptyContext
