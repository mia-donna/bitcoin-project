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
    ) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Parse
    ( Currency(code, symbol, rate, description, rate_float),
      Time(updated, updatedISO, updateduk) )

-- Creates multiple tables with our db connection handler conn
initialiseDB :: IO Connection
initialiseDB =
 do
    conn <- connectSqlite3 "bitcoin-test8.sqlite" 
    run conn "CREATE TABLE IF NOT EXISTS usd (\
          \id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, \
          \code VARCHAR(40) NOT NULL, \
          \symbol VARCHAR(40) NOT NULL, \
          \rate VARCHAR(40) NOT NULL,  \
          \description VARCHAR(40) NOT NULL, \
          \rate_float DOUBLE  \
          \) " []       
        --   \usd_id INTEGER PRIMARY KEY, \
    commit conn
    run conn "CREATE TABLE IF NOT EXISTS gbp (\
          \code VARCHAR(40) NOT NULL, \
          \symbol VARCHAR(40) NOT NULL, \
          \rate VARCHAR(40) NOT NULL,  \
          \description VARCHAR(40) NOT NULL, \
          \rate_float DOUBLE \
          \) " [] 
        --   \gbp_id INTEGER PRIMARY KEY, \
    commit conn
    run conn "CREATE TABLE IF NOT EXISTS eur (\
          \code VARCHAR(40) NOT NULL, \
          \symbol VARCHAR(40) NOT NULL, \
          \rate VARCHAR(40) NOT NULL,  \
          \description VARCHAR(40) NOT NULL, \
          \rate_float DOUBLE \
          \) " [] 
        --   \eur_id INTEGER PRIMARY KEY, \
    commit conn
    run conn "CREATE TABLE IF NOT EXISTS time (\
          \updated VARCHAR(40), \
          \updated_ISO VARCHAR(40) NOT NULL, \
          \updateduk VARCHAR(40) NOT NULL \
          \) " []                            
    commit conn
 {-   run conn "CREATE TABLE IF NOT EXISTS currencys_updated (\
          \ID INTEGER PRIMARY KEY AUTOINCREMENT, \
          \usd_code VARCHAR(40) NOT NULL, \
          \gbp_code VARCHAR(40) NOT NULL, \
          \eur_code VARCHAR(40) NOT NULL, \
          \FOREIGN KEY (usd_code) REFERENCES usd(code), \
          \FOREIGN KEY (gbp_code) REFERENCES gbp(code), \
          \FOREIGN KEY (eur_code) REFERENCES eur(code)) " []        -}                  
    commit conn
    return conn   
 
-- CONVERT OUR HASKELL DATATYPES TOSQL

-- TIME: This will work because all values are Strings
timeToSqlValues :: Time -> [SqlValue] 
timeToSqlValues time = [
       toSql $ updated time,
       toSql $ updatedISO time,
       toSql $ updateduk time
    ]

-- CURRENCY: This will work as all values are Strings and Double
currencyToSqlValues :: Currency -> [SqlValue] 
currencyToSqlValues currency = [
       toSql $ code currency,
       toSql $ symbol currency,
       toSql $ rate currency,
       toSql $ description currency,
       toSql $ rate_float currency
    ]   

-- Prepare to insert 3 records into time table -- still need to add PK id and autoincrement records into this field
prepareInsertTimeStmt :: Connection -> IO Statement
prepareInsertTimeStmt conn = prepare conn "INSERT INTO time VALUES (?,?,?)"

-- Saves time records to db 
saveTimeRecords :: Time -> Connection -> IO ()
saveTimeRecords time conn = do
     stmt <- prepareInsertTimeStmt conn 
     execute stmt (timeToSqlValues time) 
     commit conn    

-- Next create a functions to prepare currencies 
-- GBP
prepareInsertGbpStmt :: Connection -> IO Statement
prepareInsertGbpStmt conn = prepare conn "INSERT INTO gbp VALUES (?,?,?,?,?)"

-- Saves currency records to db 
saveGbpRecords :: Currency -> Connection -> IO ()
saveGbpRecords currency conn = do
     stmt <- prepareInsertGbpStmt conn 
     execute stmt (currencyToSqlValues currency) 
     commit conn

-- USD
-- Next create a function to prepare Currency 
prepareInsertUsdStmt :: Connection -> IO Statement
prepareInsertUsdStmt conn = prepare conn "INSERT INTO usd (code, symbol, rate, description, rate_float) VALUES (?,?,?,?,?)"

-- Saves currency records to db 
saveUsdRecords :: Currency -> Connection -> IO ()
saveUsdRecords currency conn = do
     stmt <- prepareInsertUsdStmt conn 
     execute stmt (currencyToSqlValues currency) 
     commit conn

-- EUR
-- Next create a function to prepare Currency 
prepareInsertEurStmt :: Connection -> IO Statement
prepareInsertEurStmt conn = prepare conn "INSERT INTO eur VALUES (?,?,?,?,?)"

-- Saves currency records to db 
saveEurRecords :: Currency -> Connection -> IO ()
saveEurRecords currency conn = do
     stmt <- prepareInsertEurStmt conn 
     execute stmt (currencyToSqlValues currency) 
     commit conn



     -- Q1 how to get updated into the other tables as a PK
     -- Q2 How to get Autoincrement to work with JSON data (works on table we creare ourselves)