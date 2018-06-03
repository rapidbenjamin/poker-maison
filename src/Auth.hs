{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Auth
  ( authHandler
  , signToken
  , verifyToken
  , hashPassword
  ) where

import Control.Monad.Except
import qualified Crypto.Hash.SHA256 as H
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
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

authHandler ::
     J.Secret -> ConnectionString -> RedisConfig -> AuthHandler Request User
authHandler secretKey connString redisConfig =
  let handler req =
        case lookup "Authorization" (requestHeaders req) of
          Nothing ->
            throwError
              (err401 {errBody = "Missing Token in 'Authorization' header"})
          Just token -> do
            authResult <-
              liftIO $
              runExceptT $
              verifyToken secretKey connString redisConfig $
              Token $ decodeUtf8 token
            case authResult of
              (Left err) ->
                throwError $ err401 {errBody = CL.pack $ T.unpack err}
              (Right user) -> return user
   in mkAuthHandler handler

hashPassword :: Text -> Text
hashPassword password = T.pack $ C.unpack $ H.hash $ encodeUtf8 password

verifyToken ::
     J.Secret
  -> ConnectionString
  -> RedisConfig
  -> Token
  -> ExceptT Text IO User
verifyToken secretKey connString redisConfig token = do
  (Username email) <- verifyJWTToken secretKey token
  maybeUser <- liftIO $ fetchUserByEmail connString redisConfig email
  case maybeUser of
    Nothing -> throwError "No User with Given Email Exists in DB"
    Just user -> return user

getAlgorithm :: J.Algorithm
getAlgorithm = J.HS256

getNewToken :: J.Secret -> User -> Password -> Handler ReturnToken
getNewToken secretKey User {..} password
  | password /= decodeUtf8 hashedPassword =
    throwError (err401 {errBody = "Password Invalid"})
  | otherwise = signToken secretKey userEmail
  where
    hashedPassword = H.hash $ encodeUtf8 password

randomText :: IO T.Text
randomText = do
  gen <- newStdGen
  let s = T.pack . take 128 $ filter isAlphaNum $ randomRs ('A', 'z') gen
  return s

signToken :: J.Secret -> Text -> Handler ReturnToken
signToken secretKey userId = do
  expTime <- liftIO $ createExpTime 60 -- expire at 1 hour
  let jwtClaimsSet =
        J.def {J.iss = J.stringOrURI userId, J.exp = J.numericDate expTime} -- iss is the issuer (username)
      alg = getAlgorithm
      token = J.encodeSigned alg secretKey jwtClaimsSet
  randomText <- liftIO randomText
  return $
    ReturnToken
      {access_token = token, refresh_token = randomText, expiration = 60 * 60}

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

verifyJWTToken :: J.Secret -> Token -> ExceptT Text IO Username
verifyJWTToken secretKey (Token token) =
  case J.decodeAndVerifySignature secretKey token of
    Nothing -> throwError "Could Not Verify Token Signature"
    (Just verifiedToken) -> do
      isValid <- liftIO $ checkExpValid tokenClaims
      if isValid
        then case J.iss tokenClaims of
               Nothing -> throwError "No issuer in token claims"
               (Just issuer) -> return $ Username $ J.stringOrURIToText issuer
        else throwError "Token Expired"
      where tokenClaims = J.claims verifiedToken
