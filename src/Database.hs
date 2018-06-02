{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (LoggingT, MonadLogger, runStdoutLoggingT)
import Control.Monad.Reader (runReaderT)
import Data.Either
import Data.Int (Int64)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Class
import Database.Persist.Postgresql
  ( ConnectionString
  , SqlPersistT
  , runMigration
  , withPostgresqlConn
  )

import Types

import Database.Persist.Sql

import Schema

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action =
  runStdoutLoggingT $
  withPostgresqlConn connectionString $ \backend -> runReaderT action backend

migrateDB :: ConnectionString -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)

--getFilteredUsers connString age = runAction connString (selectYoungUsers age)
deleteUserPG :: ConnectionString -> Int64 -> IO ()
deleteUserPG connString uid = runAction connString (delete userKey)
  where
    userKey :: Key User
    userKey = toSqlKey uid

dbGetUserByEmail :: ConnectionString -> Text -> IO (Maybe (Entity User))
dbGetUserByEmail connString email =
  runAction connString (selectFirst [UserEmail ==. email] [])

dbAddUser :: ConnectionString -> User -> IO Int64
dbAddUser conn user = fromSqlKey <$> runAction conn (insert user)

dbGetUserByLogin :: ConnectionString -> Login -> IO (Maybe (Entity User))
dbGetUserByLogin connString Login {..} =
  runAction
    connString
    (selectFirst [UserEmail ==. loginEmail, UserPassword ==. loginPassword] [])
