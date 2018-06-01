{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}

module Users where

import Auth (signToken)
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
import Schema
import Servant
import Servant.API
import Servant.Docs
import Servant.Server.Experimental.Auth
import System.Random
import UserTypes

import qualified Web.JWT as J

type UsersAPI
   = "profile" :> AuthProtect "JWT" :> Get '[ JSON] UserProfile :<|> "login" :> ReqBody '[ JSON] Login :> Post '[ JSON] ReturnToken :<|> "register" :> ReqBody '[ JSON] Register :> Post '[ JSON] ReturnToken

-- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "JWT") = User

instance ToSample Int64 where
  toSamples _ = [("Sample User", 1)]

usersAPI :: Proxy UsersAPI
usersAPI = Proxy :: Proxy UsersAPI

usersServer :: ConnectionString -> Server UsersAPI
usersServer connString =
  (fetchUserProfileHandler connString) :<|> (loginHandler connString) :<|>
  (registerUserHandler connString)

fetchUserProfileHandler :: ConnectionString -> User -> Handler UserProfile
fetchUserProfileHandler connString User {..} =
  return
    UserProfile
      {proEmail = userEmail, proChips = userChips, proUsername = userUsername}

------------------------------------------------------------------------
-- | Handlers
loginHandler :: ConnectionString -> Login -> Handler ReturnToken
loginHandler conn Login {..} = do
  maybeUser <- liftIO $ dbGetUserByLogin conn loginWithHashedPswd
  maybe (throwError unAuthErr) createToken maybeUser
  where
    unAuthErr = (err401 {errBody = "Incorrect email or password"})
    createToken (Entity _ User {..}) = signToken userEmail
    loginWithHashedPswd = Login {loginPassword = hashPassword loginPassword, ..}

hashPassword password = T.pack $ C.unpack $ H.hash $ encodeUtf8 password

registerUserHandler :: ConnectionString -> Register -> Handler ReturnToken
registerUserHandler connString Register {..} = do
  liftIO $ print "register handler"
  let hashedPassword = hashPassword newUserPassword
  let newUser =
        User
          { userUsername = newUsername
          , userEmail = newUserEmail
          , userPassword = hashedPassword
          , userChips = 3000
          }
  dbResult <- liftIO $ runAction connString $ insertBy newUser
  case dbResult -- when unique constraints conflict on entities then throw  duplicate error
        of
    Left _ -> throwError (err401 {errBody = "Email Already Taken"})
    _ -> signToken newUserEmail
