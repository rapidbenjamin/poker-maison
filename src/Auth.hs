{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Auth
  ( authHandler
  , signToken
  , checkToken'
  ) where

import Control.Monad.Except (liftIO)
import qualified Crypto.Hash.SHA256 as H
import Data.Char (isAlphaNum)
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Database.Persist
import Database.Persist.Postgresql
import Network.Wai
import Prelude
import Servant
import Servant.Server.Experimental.Auth
import System.Random
import qualified Web.JWT as J

import Database
import Schema
import Types

authHandler :: ConnectionString -> AuthHandler Request User
authHandler connString =
  let handler req =
        case lookup "Authorization" (requestHeaders req) of
          Nothing ->
            throwError
              (err401 {errBody = "Missing Token in 'Authorization' header"})
          Just token -> do
            email <- checkToken (Token (decodeUtf8 token))
            maybeUser <- liftIO $ dbGetUserByEmail connString email
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
        J.def {J.iss = J.stringOrURI userId, J.exp = J.numericDate expTime} -- iss is the issuer (username)
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
  return $ cur + (fromIntegral min + 5) * 600 -- add 5 more minutes

checkExpValid = checkExpValid' . J.exp

checkExpValid' :: Maybe J.NumericDate -> IO Bool
checkExpValid' Nothing = return False
checkExpValid' (Just d) = do
  cur <- getPOSIXTime
  return (J.secondsSinceEpoch d > cur)

checkToken :: Token -> Handler Text
checkToken (Token t) =
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

checkToken' :: Token -> IO (Either Text Text)
checkToken' (Token t) =
  case J.decodeAndVerifySignature getSecret t of
    Nothing -> return $ Left "Could Not Verify Token Signature"
    (Just verifiedToken) -> do
      isValid <- checkExpValid tokenClaims
      if isValid
        then case J.iss tokenClaims of
               Nothing -> return $ Left "No issuer in token claims"
               (Just issuer) -> return $ Right $ J.stringOrURIToText issuer
        else return $ Left "Token Expired"
      where tokenClaims = J.claims verifiedToken
