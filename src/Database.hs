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
import Data.Time.Clock
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
import Poker.Types

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
    userKey :: Key UserEntity
    userKey = toSqlKey uid

dbGetUserByEmail :: ConnectionString -> Text -> IO (Maybe UserEntity)
dbGetUserByEmail connString email = do
  maybeUserEntity <-
    runAction connString (selectFirst [UserEntityEmail ==. email] [])
  return $
    case maybeUserEntity of
      Just (Entity _ user) -> Just user
      Nothing -> Nothing

dbGetUserByUsername :: ConnectionString -> Username -> IO (Maybe UserEntity)
dbGetUserByUsername connString (Username username) = do
  maybeUserEntity <-
    runAction connString (selectFirst [UserEntityUsername ==. username] [])
  return $
    case maybeUserEntity of
      Just (Entity _ user) -> Just user
      Nothing -> Nothing

dbRegisterUser ::
     ConnectionString -> RedisConfig -> UserEntity -> ExceptT Text IO Int64
dbRegisterUser connString redisConfig userE@UserEntity {..} = do
  emailAvailable <-
    liftIO $ fetchUserByEmail connString redisConfig userEntityEmail
  if isJust emailAvailable
    then throwError "Email is Already Taken"
    else do
      usernameAvailable <-
        liftIO $ dbGetUserByUsername connString (Username userEntityUsername)
      if isJust usernameAvailable
        then throwError "Username is Already Taken"
        else liftIO $ fromSqlKey <$> runAction connString (insert userE)

dbGetUserByLogin :: ConnectionString -> Login -> IO (Maybe UserEntity)
dbGetUserByLogin connString Login {..} = do
  maybeUser <-
    runAction
      connString
      (selectFirst
         [UserEntityEmail ==. loginEmail, UserEntityPassword ==. loginPassword]
         [])
  return $
    case maybeUser of
      Just (Entity _ userE) -> Just userE
      Nothing -> Nothing

fetchUserByEmail ::
     ConnectionString -> RedisConfig -> Text -> IO (Maybe UserEntity)
fetchUserByEmail connString redisConfig email = do
  maybeCachedUser <- liftIO $ redisFetchUserByEmail redisConfig email
  case maybeCachedUser of
    Just userE -> return $ Just userE
    Nothing -> dbGetUserByEmail connString email

-------  Redis  --------
runRedisAction :: RedisConfig -> Redis a -> IO a
runRedisAction redisConfig action = do
  connection <- connect redisConfig
  runRedis connection action

-- we use emails instead of usernames for keys as users can change their usernames 
cacheUser :: RedisConfig -> Text -> UserEntity -> IO ()
cacheUser redisConfig email userE =
  runRedisAction redisConfig $
  void $ setex (pack . show $ email) 3600 (pack . show $ userE)

redisFetchUserByEmail :: RedisConfig -> Text -> IO (Maybe UserEntity)
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
       ((UserEntityUsername ==.) . fst <$> userChipCounts)
       ((UserEntityAvailableChips =.) . snd <$> userChipCounts))

-- Query runs when player takes or leaves a seat at a game
dbDepositChipsIntoPlay :: ConnectionString -> Text -> Int -> IO ()
dbDepositChipsIntoPlay connString username chipsToAdd =
  runAction
    connString
    (updateWhere
       [UserEntityUsername ==. username]
       [ UserEntityAvailableChips -=. chipsToAdd
       , UserEntityChipsInPlay +=. chipsToAdd
       ])

dbWithdrawChipsFromPlay :: ConnectionString -> Text -> Int -> IO ()
dbWithdrawChipsFromPlay connString username chipsToAdd =
  runAction
    connString
    (updateWhere
       [UserEntityUsername ==. username]
       [ UserEntityAvailableChips +=. chipsToAdd
       , UserEntityChipsInPlay -=. chipsToAdd
       ])

dbGetTableEntity :: ConnectionString -> Text -> IO (Maybe (Entity TableEntity))
dbGetTableEntity connString tableName =
  runAction connString (selectFirst [TableEntityName ==. tableName] [])

dbInsertGame ::
     ConnectionString -> Game -> Key TableEntity -> IO (Key GameEntity)
dbInsertGame connString Game {..} tableId = do
  timestamp <- getCurrentTime
  runAction connString (insert (gameEntity timestamp))
  where
    gameEntity timestamp =
      GameEntity
        { gameEntityTableID = tableId
        , gameEntityCreatedAt = timestamp
        , gameEntityPlayers = _players
        , gameEntityMinBuyInChips = _minBuyInChips
        , gameEntityMaxBuyInChips = _maxBuyInChips
        , gameEntityMaxPlayers = _maxPlayers
        , gameEntityBoard = _board
        , gameEntityWinners = _winners
        , gameEntityWaitlist = _waitlist
        , gameEntityDeck = _deck
        , gameEntitySmallBlind = _smallBlind
        , gameEntityBigBlind = _bigBlind
        , gameEntityStreet = _street
        , gameEntityPot = _pot
        , gameEntityMaxBet = _maxBet
        , gameEntityDealer = _dealer
        , gameEntityCurrentPosToAct = _currentPosToAct
        }
