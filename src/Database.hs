{-# LANGUAGE BlockArguments #-}

module Database 
    (initialiseDB,
    saveTimeRecords
    ) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Parse

-- Creates multiple tables with our db connection handler conn
initialiseDB :: IO Connection
initialiseDB =
 do
    conn <- connectSqlite3 "bitcoin4.sqlite"
    run conn "CREATE TABLE IF NOT EXISTS time (\
          \updated VARCHAR(40) NOT NULL PRIMARY KEY, \
          \updated_ISO VARCHAR(40) NOT NULL, \
          \updateduk VARCHAR(40) NOT NULL \ 
          \) " []                           
    commit conn
    run conn "CREATE TABLE IF NOT EXISTS currency (\
          \code VARCHAR(40) NOT NULL PRIMARY KEY, \
          \symbol VARCHAR(40) NOT NULL, \
          \rate VARCHAR(40) NOT NULL,  \
          \description VARCHAR(40) NOT NULL \ 
          \) " []      
    commit conn
    run conn "CREATE TABLE IF NOT EXISTS country (\
          \id INTEGER PRIMARY KEY AUTOINCREMENT, \
          \code VARCHAR(40) NOT NULL, \
          \FOREIGN KEY (code) REFERENCES currency (code)) " []  
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
       toSql $ description currency
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

-- Next create a function to prepare Currency 

-- Then create a function to map Currency