{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database where

import Control.Lens
import Control.Monad (void)
import Control.Monad.Except
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Control.Monad.Reader (runReaderT)
import Data.ByteString.Char8 (pack, unpack)
import Data.Int (Int64)
import Data.Maybe
import Data.Text (Text)
import Database.Persist
import Database.Persist.Postgresql
  ( ConnectionString
  , SqlPersistT
  , runMigration
  , withPostgresqlConn
  )
import Database.Persist.Sql
import Database.Redis (Redis, connect, runRedis, setex)
import qualified Database.Redis as Redis

import Schema
import Types

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action =
  runStdoutLoggingT $
  withPostgresqlConn connectionString $ \backend -> runReaderT action backend

migrateDB :: ConnectionString -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)

deleteUserPG :: ConnectionString -> Int64 -> IO ()
deleteUserPG connString uid = runAction connString (delete userKey)
  where
    userKey :: Key User
    userKey = toSqlKey uid

dbGetUserByEmail :: ConnectionString -> Text -> IO (Maybe User)
dbGetUserByEmail connString email = do
  maybeUser <- runAction connString (selectFirst [UserEmail ==. email] [])
  return $
    case maybeUser of
      Just (Entity _ user) -> Just user
      Nothing -> Nothing

dbGetUserByUsername :: ConnectionString -> Username -> IO (Maybe User)
dbGetUserByUsername connString (Username username) = do
  maybeUser <- runAction connString (selectFirst [UserUsername ==. username] [])
  return $
    case maybeUser of
      Just (Entity _ user) -> Just user
      Nothing -> Nothing

dbRegisterUser ::
     ConnectionString -> RedisConfig -> User -> ExceptT Text IO Int64
dbRegisterUser connString redisConfig user@User {..} = do
  emailAvailable <- liftIO $ fetchUserByEmail connString redisConfig userEmail
  if isJust emailAvailable
    then throwError "Email is Already Taken"
    else do
      usernameAvailable <-
        liftIO $ dbGetUserByUsername connString (Username userUsername)
      if isJust usernameAvailable
        then throwError "Username is Already Taken"
        else liftIO $ fromSqlKey <$> runAction connString (insert user)

dbGetUserByLogin :: ConnectionString -> Login -> IO (Maybe User)
dbGetUserByLogin connString Login {..} = do
  maybeUser <-
    runAction
      connString
      (selectFirst [UserEmail ==. loginEmail, UserPassword ==. loginPassword] [])
  return $
    case maybeUser of
      Just (Entity _ user) -> Just user
      Nothing -> Nothing

fetchUserByEmail :: ConnectionString -> RedisConfig -> Text -> IO (Maybe User)
fetchUserByEmail connString redisConfig email = do
  maybeCachedUser <- liftIO $ redisFetchUserByEmail redisConfig email
  case maybeCachedUser of
    Just user -> return $ Just user
    Nothing -> dbGetUserByEmail connString email

-------  Redis  --------
runRedisAction :: RedisConfig -> Redis a -> IO a
runRedisAction redisConfig action = do
  connection <- connect redisConfig
  runRedis connection action

-- we use emails instead of usernames for keys as users can change their usernames 
cacheUser :: RedisConfig -> Text -> User -> IO ()
cacheUser redisConfig email user =
  runRedisAction redisConfig $
  void $ setex (pack . show $ email) 3600 (pack . show $ user)

redisFetchUserByEmail :: RedisConfig -> Text -> IO (Maybe User)
redisFetchUserByEmail redisConfig email =
  runRedisAction redisConfig $ do
    result <- Redis.get (pack . show $ email)
    case result of
      Right (Just userString) -> return $ Just (read . unpack $ userString)
      _ -> return Nothing

-------  Redis  --------
-- Query is called at the end of every hand to update player balances
dbUpdateUsersChips :: ConnectionString -> [(Text, Int)] -> IO ()
dbUpdateUsersChips connString userChipCounts =
  runAction
    connString
    (updateWhere
       ((UserUsername ==.) . fst <$> userChipCounts)
       ((UserAvailableChips =.) . snd <$> userChipCounts))

-- Query runs when player takes or leaves a seat at a game
dbDepositChipsIntoPlay :: ConnectionString -> Text -> Int -> IO ()
dbDepositChipsIntoPlay connString username chipsToAdd =
  runAction
    connString
    (updateWhere
       [UserUsername ==. username]
       [UserAvailableChips -=. chipsToAdd, UserChipsInPlay +=. chipsToAdd])

dbWithdrawChipsFromPlay :: ConnectionString -> Text -> Int -> IO ()
dbWithdrawChipsFromPlay connString username chipsToAdd =
  runAction
    connString
    (updateWhere
       [UserUsername ==. username]
       [UserAvailableChips +=. chipsToAdd, UserChipsInPlay -=. chipsToAdd])
