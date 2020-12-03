-- |MODULE that inits and creates a db for our Haskell data, converts it toSql and enters values into the db

{-# LANGUAGE BlockArguments #-}

module Database 
    (initialiseDB,
    currencyToSqlValues,
    timeToSqlValues,
    prepareInsertTimeStmt,
    saveTimeRecords,
    prepareInsertGbpStmt,
    saveGbpRecords,
    prepareInsertUsdStmt,
    saveUsdRecords,
    prepareInsertEurStmt,
    saveEurRecords,
    prepareInsertFKStmt,
    saveFKRecords,
    queryItemByCode,
    getCurrencyId,
    insertIntoLinkingTable,
    queryTime
    ) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Parse
    ( Currency(code, symbol, rate, description, rate_float),
      Time(updated, updatedISO, updateduk) )
import Data.Char

-- || INITIALIZE OUR DB AND CREATE TABLES AND KEYS

initialiseDB :: IO Connection
initialiseDB =
 do
    conn <- connectSqlite3 "bitcoin-test2.sqlite"
    -- runRaw conn "COMMIT; PRAGMA foreign_keys = ON; BEGIN TRANSACTION" 
    run conn "CREATE TABLE IF NOT EXISTS linkingTable (\
          \usd_id INTEGER NOT NULL, \
          \gbp_id INTEGER NOT NULL, \
          \eur_id INTEGER NOT NULL, \
          \updated VARCHAR(40) NOT NULL, \        
          \FOREIGN KEY (usd_id) REFERENCES usd(usd_id), \
          \FOREIGN KEY (gbp_id) REFERENCES usd(gbp_id), \
          \FOREIGN KEY (eur_id) REFERENCES usd(eur_id) \
          \FOREIGN KEY (updated) REFERENCES time(updated) \
          \) " []  
    commit conn
    run conn "CREATE TABLE IF NOT EXISTS usd (\
          \code VARCHAR(40) NOT NULL, \
          \symbol VARCHAR(40) NOT NULL, \
          \rate VARCHAR(40) NOT NULL,  \
          \description VARCHAR(40) NOT NULL, \
          \rate_float DOUBLE,  \
          \usd_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT \
          \) " []       
        --   \usd_id INTEGER PRIMARY KEY, \
    commit conn
    run conn "CREATE TABLE IF NOT EXISTS gbp (\
          \code VARCHAR(40) NOT NULL, \
          \symbol VARCHAR(40) NOT NULL, \
          \rate VARCHAR(40) NOT NULL,  \
          \description VARCHAR(40) NOT NULL, \
          \rate_float DOUBLE, \
          \gbp_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT \
          \) " []
        --   \gbp_id INTEGER PRIMARY KEY, \
    commit conn
    run conn "CREATE TABLE IF NOT EXISTS eur (\
          \code VARCHAR(40) NOT NULL, \
          \symbol VARCHAR(40) NOT NULL, \
          \rate VARCHAR(40) NOT NULL,  \
          \description VARCHAR(40) NOT NULL, \
          \rate_float DOUBLE, \
          \eur_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT \
          \) " []
        --   \eur_id INTEGER PRIMARY KEY, \
    commit conn
    run conn "CREATE TABLE IF NOT EXISTS time (\
          \updated VARCHAR(40) NOT NULL, \
          \updated_ISO VARCHAR(40) NOT NULL, \
          \updateduk VARCHAR(40) NOT NULL, \
          \id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT \
          \) " []                            
    commit conn
    return conn   
{-\FOREIGN KEY (updated) REFERENCES linkingTable(updated) \-}

-- CONVERT OUR HASKELL DATATYPES TOSQL

-- || TO SQL
-- |TIME TO SQL: This converts all our time values toSql
timeToSqlValues :: Time -> [SqlValue] 
timeToSqlValues time = [
       toSql $ updated time,
       toSql $ updatedISO time,
       toSql $ updateduk time
    ]

-- |CURRENCY TO SQL: This converts all our currency values toSql
currencyToSqlValues :: Currency -> [SqlValue] 
currencyToSqlValues currency = [
       toSql $ code currency,
       toSql $ symbol currency,
       toSql $ rate currency,
       toSql $ description currency,
       toSql $ rate_float currency
    ]   

-- || TIME
-- |PREPARE TIME : This prepares our db connection and takes our SQL statement
prepareInsertTimeStmt :: Connection -> IO Statement
prepareInsertTimeStmt conn = prepare conn "INSERT INTO time (updated, updated_ISO, updateduk) VALUES (?,?,?)"

-- |SAVE TIME: This saves our time records to the db
saveTimeRecords :: Time -> Connection -> IO ()
saveTimeRecords time conn = do
     stmt <- prepareInsertTimeStmt conn 
     execute stmt (timeToSqlValues time) 
     commit conn    

-- || CURRENCYS
-- |PREPARE GBP : This prepares our db connection and takes our SQL statement
prepareInsertGbpStmt :: Connection -> IO Statement
prepareInsertGbpStmt conn = prepare conn "INSERT INTO gbp (code, symbol, rate, description, rate_float) VALUES (?,?,?,?,?)"

-- |SAVE GBP: This saves our gbp records to the db
saveGbpRecords :: Currency -> Connection -> IO ()
saveGbpRecords currency conn = do
     stmt <- prepareInsertGbpStmt conn 
     execute stmt (currencyToSqlValues currency) 
     commit conn

-- |PREPARE USD : This prepares our db connection and takes our SQL statement
prepareInsertUsdStmt :: Connection -> IO Statement
prepareInsertUsdStmt conn = prepare conn "INSERT INTO usd (code, symbol, rate, description, rate_float) VALUES (?,?,?,?,?)"

-- |SAVE USD: This saves our gbp records to the db
saveUsdRecords :: Currency -> Connection -> IO ()
saveUsdRecords currency conn = do
     stmt <- prepareInsertUsdStmt conn 
     execute stmt (currencyToSqlValues currency) 
     commit conn

-- |PREPARE EUR : This prepares our db connection and takes our SQL statement
prepareInsertEurStmt :: Connection -> IO Statement
prepareInsertEurStmt conn = prepare conn "INSERT INTO eur (code, symbol, rate, description, rate_float) VALUES (?,?,?,?,?)"

-- |SAVE EUR: This saves our gbp records to the db
saveEurRecords :: Currency -> Connection -> IO ()
saveEurRecords currency conn = do
     stmt <- prepareInsertEurStmt conn 
     execute stmt (currencyToSqlValues currency) 
     commit conn

-- || QUERIES
-- | BY CODE: This queries items by currency code
queryItemByCode ::  IConnection conn => String -> conn -> IO [String]
queryItemByCode itemCode conn = do
  stmt <- prepare conn query
  execute stmt [toSql itemCode]
  rows <- fetchAllRows stmt 
  return $ map fromSql $ head rows
  where
    query = unlines $ ["SELECT description, rate, rate_float FROM "++ map toLower itemCode ++" WHERE code = ?"]

-- | BY ID: This selects currency id
getCurrencyId ::  IConnection conn => String -> conn -> IO String
getCurrencyId currency conn = do
  stmt <- prepare conn query
  execute stmt []
  rows <- fetchAllRows stmt 
  return $ head $ map fromSql $ head rows
  where
    query = unlines $ ["SELECT " ++ currency ++ "_id FROM "++ currency]

-- | TIME : This gets value updated from time table 
queryTime ::  IConnection conn => conn -> IO String
queryTime conn = do
  stmt <- prepare conn query
  execute stmt []
  rows <- fetchAllRows stmt 
  return $ head $ map fromSql $ head rows
  where
    query = unlines $ ["SELECT updated FROM time"]

-- || SAVE RETRIEVED VALUES TO THE linkingTable TABLE
insertIntoLinkingTable :: IConnection conn => String -> String -> String -> String -> conn -> IO ()
insertIntoLinkingTable usd_id gbp_id eur_id time conn = do
  stmt <- prepare conn query
  execute stmt []
  commit conn
  where 
    query = unlines $ ["INSERT INTO linkingTable (usd_id, gbp_id, eur_id, updated) VALUES ("++ usd_id ++","++ gbp_id ++","++ eur_id ++ ",'"++ time ++ "')"]


-------------------WIP--------------------
--STILL TO DO
-- insert data to currencys_updated keys     


--FOREIGN KEYS
-- Next create a function to prepare Currency 
prepareInsertFKStmt :: Connection -> IO Statement
prepareInsertFKStmt conn = prepare conn "INSERT INTO currencys_last_updated ( updated ) VALUES (?) SELECT updated FROM time"

-- Saves currency records to db 
saveFKRecords :: Time -> Connection -> IO ()
saveFKRecords time conn = do
     stmt <- prepareInsertFKStmt conn 
     execute stmt (timeToSqlValues time) 
     commit conn