{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module API where

import Data.Proxy (Proxy(..))
import Database.Persist.Postgresql
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Servant.Server
import Servant.Server.Experimental.Auth
import Web.JWT (Secret)

import Network.Wai.Middleware.Servant.Options

import Auth (authHandler)
import Schema
import Types (RedisConfig)
import Users

import Network.Wai.Middleware.Cors


type Middleware = Application -> Application

addMiddleware :: Application -> Application
addMiddleware = logStdoutDev . cors (const $ Just policy) . (provideOptions api)
  where
    corsReqHeaders = ["content-type", "Access-Control-Allow-Origin", "POST", "GET", "*"]
    policy = simpleCorsResourcePolicy {corsRequestHeaders = corsReqHeaders}



app :: Secret -> ConnectionString -> RedisConfig -> Application
app secretKey connString redisConfig =  addMiddleware $ app' secretKey connString redisConfig
 
type API = UsersAPI

api :: Proxy API
api = Proxy :: Proxy API

server :: Secret -> ConnectionString -> RedisConfig -> Server API
server = usersServer

app' :: Secret -> ConnectionString -> RedisConfig -> Application
app' secretKey connString redisConfig = 
  serveWithContext
    api
    serverAuthContext
    (server secretKey connString redisConfig)
  where
    serverAuthContext :: Context (AuthHandler Request UserEntity ': '[])
    serverAuthContext =
      authHandler secretKey connString redisConfig :. EmptyContext

       
