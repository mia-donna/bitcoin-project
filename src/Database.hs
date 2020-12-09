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
    queryItemByCode,
    getCurrencyId,
    insertIntoLinkingTable,
    queryTime,
    queryAll
    ) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Parse
    ( Currency(code, symbol, rate, description, rate_float),
      Time(updated, updatedISO, updateduk) )
import Data.Char

-- | INITIALIZE OUR DB AND CREATE TABLES AND KEYS

initialiseDB :: IO Connection
initialiseDB =
 do
    conn <- connectSqlite3 "bitcoin.sqlite"
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
    commit conn
    run conn "CREATE TABLE IF NOT EXISTS gbp (\
          \code VARCHAR(40) NOT NULL, \
          \symbol VARCHAR(40) NOT NULL, \
          \rate VARCHAR(40) NOT NULL,  \
          \description VARCHAR(40) NOT NULL, \
          \rate_float DOUBLE, \
          \gbp_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT \
          \) " []
    commit conn
    run conn "CREATE TABLE IF NOT EXISTS eur (\
          \code VARCHAR(40) NOT NULL, \
          \symbol VARCHAR(40) NOT NULL, \
          \rate VARCHAR(40) NOT NULL,  \
          \description VARCHAR(40) NOT NULL, \
          \rate_float DOUBLE, \
          \eur_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT \
          \) " []
    commit conn
    run conn "CREATE TABLE IF NOT EXISTS time (\
          \updated VARCHAR(40) NOT NULL, \
          \updated_ISO VARCHAR(40) NOT NULL, \
          \updateduk VARCHAR(40) NOT NULL, \
          \id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT \
          \) " []                            
    commit conn
    return conn   

-- |TIME TO SQL: "timeToSqlValues" function converts all our time values toSql
timeToSqlValues :: Time -> [SqlValue] 
timeToSqlValues time = [
       toSql $ updated time,
       toSql $ updatedISO time,
       toSql $ updateduk time
    ]

-- |CURRENCY TO SQL: "currencyToSqlValues" function converts all our currency values toSql
currencyToSqlValues :: Currency -> [SqlValue] 
currencyToSqlValues currency = [
       toSql $ code currency,
       toSql $ symbol currency,
       toSql $ rate currency,
       toSql $ description currency,
       toSql $ rate_float currency
    ]   

-- |PREPARE TIME: "prepareInsertTimeStmt" function prepares our db connection and takes our SQL statement
prepareInsertTimeStmt :: Connection -> IO Statement
prepareInsertTimeStmt conn = prepare conn "INSERT INTO time (updated, updated_ISO, updateduk) VALUES (?,?,?)"

-- |SAVE TIME: "saveTimeRecords" function saves our time records to the db
saveTimeRecords :: Time -> Connection -> IO ()
saveTimeRecords time conn = do
     stmt <- prepareInsertTimeStmt conn 
     execute stmt (timeToSqlValues time) 
     commit conn    

-- |PREPARE GBP : "prepareInsertGbpStmt" function prepares our db connection and takes our SQL statement
prepareInsertGbpStmt :: Connection -> IO Statement
prepareInsertGbpStmt conn = prepare conn "INSERT INTO gbp (code, symbol, rate, description, rate_float) VALUES (?,?,?,?,?)"

-- |SAVE GBP: "saveGbpRecords" function saves our gbp records to the db
saveGbpRecords :: Currency -> Connection -> IO ()
saveGbpRecords currency conn = do
     stmt <- prepareInsertGbpStmt conn 
     execute stmt (currencyToSqlValues currency) 
     commit conn

-- |PREPARE USD : "prepareInsertUsdStmt" function prepares our db connection and takes our SQL statement
prepareInsertUsdStmt :: Connection -> IO Statement
prepareInsertUsdStmt conn = prepare conn "INSERT INTO usd (code, symbol, rate, description, rate_float) VALUES (?,?,?,?,?)"

-- |SAVE USD: "saveUsdRecords" function saves our gbp records to the db
saveUsdRecords :: Currency -> Connection -> IO ()
saveUsdRecords currency conn = do
     stmt <- prepareInsertUsdStmt conn 
     execute stmt (currencyToSqlValues currency) 
     commit conn

-- |PREPARE EUR : "prepareInsertEurStmt" function prepares our db connection and takes our SQL statement
prepareInsertEurStmt :: Connection -> IO Statement
prepareInsertEurStmt conn = prepare conn "INSERT INTO eur (code, symbol, rate, description, rate_float) VALUES (?,?,?,?,?)"

-- |SAVE EUR: "saveEurRecords" function saves our gbp records to the db
saveEurRecords :: Currency -> Connection -> IO ()
saveEurRecords currency conn = do
     stmt <- prepareInsertEurStmt conn 
     execute stmt (currencyToSqlValues currency) 
     commit conn

-- |SAVE RETRIEVED: "insertIntoLinkingTable" function inserts primary key values to the linkingTable TABLE to create relations
insertIntoLinkingTable :: IConnection conn => String -> String -> String -> String -> conn -> IO ()
insertIntoLinkingTable usd_id gbp_id eur_id time conn = do
  stmt <- prepare conn query
  execute stmt []
  commit conn
  where 
    query = unlines $ ["INSERT INTO linkingTable (usd_id, gbp_id, eur_id, updated) VALUES ("++ usd_id ++","++ gbp_id ++","++ eur_id ++ ",'"++ time ++ "')"]



-- |Now that the data is inserted here are functions to query tables
-- |BY CODE: "queryItemByCode" function queries items by currency code
queryItemByCode ::  IConnection conn => String -> conn -> IO [String]
queryItemByCode itemCode conn = do
  stmt <- prepare conn query
  execute stmt [toSql itemCode]
  rows <- fetchAllRows stmt 
  return $ map fromSql $ last rows -- Makes sure you return only last (most recent) row - this is to provide LIVE data
  where
    query = unlines $ ["SELECT rate FROM "++ map toLower itemCode ++" WHERE code = ?"]

-- |BY ID: "getCurrencyId" function selects currency id
getCurrencyId ::  IConnection conn => String -> conn -> IO String
getCurrencyId currency conn = do
  stmt <- prepare conn query
  execute stmt []
  rows <- fetchAllRows stmt 
  return $ head $ map fromSql $ last rows -- Makes sure you return only last (most recent) row - to retrieve most recent ids
  where
    query = unlines $ ["SELECT " ++ currency ++ "_id FROM "++ currency]

-- |TIME : "queryTime" function gets value updated from time table 
queryTime ::  IConnection conn => conn -> IO String
queryTime conn = do
  stmt <- prepare conn query
  execute stmt []
  rows <- fetchAllRows stmt 
  return $ head $ map fromSql $ last rows -- Makes sure you return only last (most recent) row - this is to provide LIVE data
  where
    query = unlines $ ["SELECT updated FROM time"]

-- |ALL CURRENCY: "queryAll" function returns results from join query of all currency tables
queryAll ::  IConnection conn => conn -> IO [String]
queryAll conn = do
  stmt <- prepare conn query
  execute stmt []
  rows <- fetchAllRows stmt 
  return $ map fromSql $ last rows -- fetch most recent currency rates
  where
    query = unlines $ ["SELECT usd.code, usd.rate, gbp.code, gbp.rate, eur.code, eur.rate FROM usd INNER JOIN linkingTable ON usd.usd_id = linkingTable.usd_id INNER JOIN eur ON eur.eur_id = linkingTable.eur_id INNER JOIN gbp ON gbp.gbp_id = linkingTable.gbp_id;"]
