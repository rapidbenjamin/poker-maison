{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module API where

import Data.Proxy (Proxy(..))
import Database.Persist.Postgresql
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Servant.Server
import Servant.Auth.Server
import Crypto.JOSE.JWK (JWK)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)
import Servant
import Servant.Auth.Server
import GHC.TypeLits
import Servant.Foreign
import qualified Data.ByteString.Lazy as BSL
import Network.Wai.Middleware.Servant.Options
import Data.Proxy
import Schema
import Users
import Control.Lens ((&), (<>~))
import Debug.Trace
import Types
import Network.Wai.Middleware.Cors

import GHC.TypeLits

type API auths =
         (Servant.Auth.Server.Auth auths Username :> ProtectedUsersAPI)
    :<|> UnprotectedUsersAPI

type UnprotectedUsersAPI = 
       "login" :> ReqBody '[ JSON] Login :> Post '[ JSON] ReturnToken      
  :<|> "register" :> ReqBody '[ JSON] Register :> Post '[ JSON] ReturnToken
  :<|> "lobby" :> Get '[ JSON] NoContent
    
type ProtectedUsersAPI =
  "profile" :> Get '[ JSON] UserProfile   

api :: Proxy (API '[JWT])
api = Proxy :: Proxy (API '[JWT])

protectedUsersApi :: Proxy ProtectedUsersAPI 
protectedUsersApi = Proxy :: Proxy ProtectedUsersAPI

unprotectedUsersApi :: Proxy UnprotectedUsersAPI
unprotectedUsersApi = Proxy :: Proxy UnprotectedUsersAPI

app :: BS.ByteString -> ConnectionString -> RedisConfig -> Application
app secretKey connString redisConfig = addMiddleware $ serveWithAuth secretKey connString redisConfig

type Token = String

type family TokenHeaderName xs :: Symbol where
  TokenHeaderName (Cookie ': xs) = "X-XSRF-TOKEN"
  TokenHeaderName (JWT ': xs) = "Authorization"
  TokenHeaderName (x ': xs) = TokenHeaderName xs
  TokenHeaderName '[] = TypeError (Text "Neither JWT nor cookie auth enabled")
instance
    ( TokenHeaderName auths ~ header
    , KnownSymbol header
    , HasForeignType lang ftype Token
    , HasForeign lang ftype sub
    )
    => HasForeign lang ftype (Auth auths a :> sub) where
  type Foreign ftype (Auth auths a :> sub) = Foreign ftype sub

  foreignFor lang Proxy Proxy req =
    foreignFor lang Proxy subP $ req & reqHeaders <>~ [HeaderArg arg]
    where
      arg   = Arg
        { _argName = PathSegment . T.pack $ symbolVal @header Proxy
        , _argType = token
        }
      token = typeFor lang (Proxy @ftype) (Proxy @Token)
      subP  = Proxy @sub
      

-- Adds JWT Authentication to our server
serveWithAuth :: BS.ByteString -> ConnectionString -> RedisConfig -> Application
serveWithAuth secretKey c r = 
  serveWithContext api cfg (server jwtCfg c r)
  where
    jwk = fromSecret secretKey 
    jwtCfg = defaultJWTSettings jwk
    cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
    api = Proxy :: Proxy (API '[JWT]) -- API is a type synonym for our api - type is now concrete

server :: JWTSettings -> ConnectionString -> RedisConfig -> Server (API '[JWT])
server j c r = protectedUsersServer j c r :<|> unprotectedUsersServer j c r

unprotectedUsersServer :: JWTSettings -> ConnectionString -> RedisConfig -> Server UnprotectedUsersAPI
unprotectedUsersServer jwtSettings connString redisConfig =
    loginHandler jwtSettings connString :<|>
    registerUserHandler jwtSettings connString redisConfig :<|>
    getLobbyHandler jwtSettings connString redisConfig

protectedUsersServer :: JWTSettings -> ConnectionString -> RedisConfig -> AuthResult Username -> Server ProtectedUsersAPI
protectedUsersServer j c r (Authenticated username') = fetchUserProfileHandler c username'
protectedUsersServer _ _ _ er = traceShow  er (throwAll err401)

type Middleware = Application -> Application

addMiddleware :: Application -> Application
addMiddleware = logStdoutDev . cors (const $ Just policy) . (provideOptions api)
  where
    corsReqHeaders = ["content-type", "Access-Control-Allow-Origin", "POST", "GET", "*"]
    policy = simpleCorsResourcePolicy {corsRequestHeaders = corsReqHeaders}
