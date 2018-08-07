module Concurrency where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.STM

import Data.Text (Text)
import Database.Persist
import Database.Persist.Postgresql
  ( ConnectionString
  , SqlPersistT
  , runMigration
  , withPostgresqlConn
  )

import Database
import Schema
import Socket.Types

-- Looks up the tableName in the DB to get the key and if no corresponsing  table is found in the db then
-- we insert a new table to the db. This step is necessary as we use the TableID as a foreign key in the
-- For Game Entities in the DB. 
-- After we have the TableID we fork a new process which listens to the channel which emits new game states
-- for a given table. For each new game state msg received we write the new game state into the DB.
forkGameDBWriter ::
     ConnectionString -> TChan MsgOut -> TableName -> IO (Async ())
forkGameDBWriter connString chan tableName = do
  maybeTableEntity <- dbGetTableEntity connString tableName
  case maybeTableEntity of
    Nothing -> do
      tableKey <- dbInsertTableEntity connString tableName
      forkGameWriter tableKey
    Just (Entity tableKey _) -> forkGameWriter tableKey
  where
    forkGameWriter tableKey = async (writeNewGameToDB connString chan tableKey)

writeNewGameToDB :: ConnectionString -> TChan MsgOut -> Key TableEntity -> IO ()
writeNewGameToDB connString chan tableKey = do
  dupChan <- atomically $ dupTChan chan
  forever $ do
    chanMsg <- atomically $ readTChan dupChan
    case chanMsg of
      (NewGameState tableName game) ->
        void (dbInsertGame connString game tableKey)
      _ -> return ()
