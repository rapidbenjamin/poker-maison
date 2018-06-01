{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}

module Auth
  ( authHandler
  , signToken
  ) where

import Control.Monad.Except (liftIO)
import qualified Crypto.Hash.SHA256 as H
import Data.Aeson (Result(..), fromJSON, toJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Char (isAlphaNum)
import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Database
import Database.Persist
import Database.Persist.Postgresql
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Prelude
import Schema
import Servant
import Servant.API
import Servant.Docs
import Servant.Server.Experimental.Auth
import System.Random
import Types

import qualified Web.JWT as J

authHandler :: ConnectionString -> AuthHandler Request User
  -- FIXME Too nested.
authHandler connString =
  let handler req =
        case lookup "Authorization" (requestHeaders req) of
          Nothing ->
            throwError
              (err401 {errBody = "Missing Token in 'Authorization' header"})
          Just token -> do
            email <- checkToken (Token (decodeUtf8 token))
            maybeUser <- liftIO $ dbgGetUserByEmail connString (email)
            case maybeUser of
              Nothing ->
                throwError
                  (err401 {errBody = "No User with Given Email Exists in DB"})
              Just userEntity -> return $ entityVal userEntity
   in mkAuthHandler handler

getSecret :: J.Secret
getSecret = J.secret "wwaaifidsa9109f0dasfda-=2-13"

getAlgorithm :: J.Algorithm
getAlgorithm = J.HS256

getNewToken :: User -> Password -> Handler ReturnToken
getNewToken User {..} password
  | password /= decodeUtf8 hashedPassword =
    throwError (err401 {errBody = "Password Invalid"})
  | otherwise = signToken userEmail
  where
    hashedPassword = H.hash $ encodeUtf8 password

randomText :: IO T.Text
randomText = do
  gen <- newStdGen
  let s = T.pack . take 128 $ filter isAlphaNum $ randomRs ('A', 'z') gen
  return s

signToken :: Text -> Handler ReturnToken
signToken userId = do
  expTime <- liftIO $ createExpTime 60 -- expire at 1 hour
  let jwtClaimsSet =
        J.def {J.iss = J.stringOrURI userId, J.exp = J.intDate expTime} -- iss is the issuer (username)
      s = getSecret
      alg = getAlgorithm
      token = J.encodeSigned alg s jwtClaimsSet
  randomText <- liftIO $ randomText
  return $
    ReturnToken
      {access_token = token, refresh_token = randomText, expiration = (60 * 60)}

createExpTime :: Int -> IO NominalDiffTime
createExpTime min = do
  cur <- getPOSIXTime
  return $ cur + (fromIntegral min + 5) * 60 -- add 5 more minutes

checkExpValid = checkExpValid' . J.exp

checkExpValid' :: Maybe J.IntDate -> IO Bool
checkExpValid' Nothing = return False
checkExpValid' (Just d) = do
  cur <- getPOSIXTime
  return (J.secondsSinceEpoch d > cur)

checkToken :: Token -> Handler Text
checkToken (Token t) = do
  liftIO $ print t
  case J.decodeAndVerifySignature getSecret t of
    Nothing ->
      throwError (err401 {errBody = "Could Not Verify Token Signature"})
    (Just verifiedToken) -> do
      isValid <- liftIO $ checkExpValid tokenClaims
      if isValid
        then case J.iss tokenClaims of
               Nothing ->
                 throwError (err401 {errBody = "No issuer in token claims"})
               (Just issuer) -> return $ J.stringOrURIToText issuer
        else throwError (err401 {errBody = "Token Expired"})
      where tokenClaims = J.claims verifiedToken
