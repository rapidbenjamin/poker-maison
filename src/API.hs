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
import Web.JWT (Secret)

import Auth (authHandler)
import Schema
import Types (RedisConfig)
import Users

type API = UsersAPI

api :: Proxy API
api = Proxy :: Proxy API

server :: Secret -> ConnectionString -> RedisConfig -> Server API
server = usersServer

app :: Secret -> ConnectionString -> RedisConfig -> Application
app secretKey connString redisConfig =
  serveWithContext
    api
    serverAuthContext
    (server secretKey connString redisConfig)
  where
    serverAuthContext :: Context (AuthHandler Request User ': '[])
    serverAuthContext =
      authHandler secretKey connString redisConfig :. EmptyContext
