{- 
  Unfortunately jwtSettingsToJwtValidationSettings is directly 
  the servant-auth library internals the JWT is signed by the User API
  when a user registers or logs in so essentially this file has
  to currently duplicate the token verification logic that the servant-auth
  library hides internally so we can suthenticate socket connections
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Socket.Auth where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Either

import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Database.Persist.Postgresql    ( ConnectionString
                                                , entityVal
                                                )
import qualified Network.WebSockets            as WS
import           Prelude
import           Text.Pretty.Simple             ( pPrint )
import qualified Data.ByteString.Lazy          as BL

import qualified Data.Set                      as Set
import           Database
import           Schema
import           Socket.Utils
import           Types
import           Servant.Auth.Server
import qualified Data.ByteString               as BS

import           Crypto.JOSE                   as Jose
import           Crypto.JWT                    as Jose

import           Data.Maybe
import           System.Timeout
import           Crypto.JWT


verifyJWT :: BS.ByteString -> BL.ByteString -> IO (Either JWTError ClaimsSet)
verifyJWT key jwt = runExceptT $ do
  jwt' <- decodeCompact jwt
  -- decode JWT
  verifyClaims jwtCfg jwk jwt'
 where
  jwk    = fromSecret key
  jwtCfg = jwtSettingsToJwtValidationSettings $ defaultJWTSettings jwk

jwtSettingsToJwtValidationSettings :: JWTSettings -> Jose.JWTValidationSettings
jwtSettingsToJwtValidationSettings s = defaultJWTValidationSettings
  (toBool <$> audienceMatches s)
 where
  toBool Matches      = True
  toBool DoesNotMatch = False
