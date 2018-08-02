{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database where

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

dbGetUserByEmail :: ConnectionString -> Text -> IO (Maybe (Entity User))
dbGetUserByEmail connString email =
  runAction connString (selectFirst [UserEmail ==. email] [])

dbGetUserByUsername :: ConnectionString -> Username -> IO (Maybe (Entity User))
dbGetUserByUsername connString (Username username) =
  runAction connString (selectFirst [UserUsername ==. username] [])

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

dbGetUserByLogin :: ConnectionString -> Login -> IO (Maybe (Entity User))
dbGetUserByLogin connString Login {..} =
  runAction
    connString
    (selectFirst [UserEmail ==. loginEmail, UserPassword ==. loginPassword] [])

fetchUserByEmail :: ConnectionString -> RedisConfig -> Text -> IO (Maybe User)
fetchUserByEmail connString redisConfig email = do
  maybeCachedUser <- liftIO $ redisFetchUserByEmail redisConfig email
  case maybeCachedUser of
    Just user -> return $ Just user
    Nothing -> do
      maybeUser <- liftIO $ dbGetUserByEmail connString email
      case maybeUser of
        Just (Entity _ user@User {..}) -> do
          cacheUser redisConfig email user
          return $ Just user
        Nothing -> return Nothing

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
dbUpdateUserChips :: ConnectionString -> [(Text, Int)] -> IO ()
dbUpdateUserChips connString userChipCounts =
  runAction
    connString
    (updateWhere
       ((UserUsername ==.) . fst <$> userChipCounts)
       ((UserChips =.) . snd <$> userChipCounts))
