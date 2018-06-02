{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Users where

import Control.Monad.Except (liftIO)
import qualified Crypto.Hash.SHA256 as H
import qualified Data.ByteString.Char8 as C
import Data.Proxy
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Database
import Database.Persist
import Database.Persist.Postgresql
import Servant
import Servant.Server.Experimental.Auth

import Auth (signToken)
import Schema
import Types

type UsersAPI
   = "profile" :> AuthProtect "JWT" :> Get '[ JSON] UserProfile :<|> "login" :> ReqBody '[ JSON] Login :> Post '[ JSON] ReturnToken :<|> "register" :> ReqBody '[ JSON] Register :> Post '[ JSON] ReturnToken

-- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "JWT") = User

usersAPI :: Proxy UsersAPI
usersAPI = Proxy :: Proxy UsersAPI

usersServer :: ConnectionString -> Server UsersAPI
usersServer connString =
  fetchUserProfileHandler :<|> loginHandler connString :<|>
  registerUserHandler connString

fetchUserProfileHandler :: User -> Handler UserProfile
fetchUserProfileHandler User {..} =
  return
    UserProfile
      { proEmail = userEmail
      , proChips = userChips
      , proUsername = Username userUsername
      }

------------------------------------------------------------------------
-- | Handlers
loginHandler :: ConnectionString -> Login -> Handler ReturnToken
loginHandler conn Login {..} = do
  maybeUser <- liftIO $ dbGetUserByLogin conn loginWithHashedPswd
  maybe (throwError unAuthErr) createToken maybeUser
  where
    unAuthErr = err401 {errBody = "Incorrect email or password"}
    createToken (Entity _ User {..}) = signToken userEmail
    loginWithHashedPswd = Login {loginPassword = hashPassword loginPassword, ..}

hashPassword :: Text -> Text
hashPassword password = T.pack $ C.unpack $ H.hash $ encodeUtf8 password

registerUserHandler :: ConnectionString -> Register -> Handler ReturnToken
registerUserHandler connString Register {..} = do
  let hashedPassword = hashPassword newUserPassword
  let (Username username) = newUsername
  let newUser =
        User
          { userUsername = username
          , userEmail = newUserEmail
          , userPassword = hashedPassword
          , userChips = 3000
          }
  dbResult <- liftIO $ runAction connString $ insertBy newUser
  case dbResult -- when unique constraints conflict on entities then throw  duplicate error
        of
    Left _ -> throwError (err401 {errBody = "Email Already Taken"})
    _ -> signToken newUserEmail
