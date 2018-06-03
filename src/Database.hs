{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database where

import Control.Monad.Except
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Control.Monad.Reader (runReaderT)
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

dbGetUserByUsername :: ConnectionString -> Text -> IO (Maybe (Entity User))
dbGetUserByUsername connString username =
  runAction connString (selectFirst [UserUsername ==. username] [])

dbRegisterUser :: ConnectionString -> User -> ExceptT Text IO Int64
dbRegisterUser connString user@User {..} = do
  emailAvailable <- liftIO $ dbGetUserByEmail connString userEmail
  case isJust emailAvailable of
    True -> throwError "Email is Already Taken"
    False -> do
      usernameAvailable <- liftIO $ dbGetUserByUsername connString userUsername
      case isJust usernameAvailable of
        True -> throwError "Username is Already Taken"
        False -> do
          newUserSQLKey <-
            liftIO $ fromSqlKey <$> runAction connString (insert user)
          return $ newUserSQLKey

dbGetUserByLogin :: ConnectionString -> Login -> IO (Maybe (Entity User))
dbGetUserByLogin connString Login {..} =
  runAction
    connString
    (selectFirst [UserEmail ==. loginEmail, UserPassword ==. loginPassword] [])
