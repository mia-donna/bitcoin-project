{-# LANGUAGE BlockArguments #-}

module Database 
    (initialiseDB,
    saveRecords
    ) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Parse 

initialiseDB :: IO Connection
initialiseDB db = do 
        conn <- connectSqlite3 "bitcoin.sqlite" 
        run conn "CREATE TABLE IF NOT EXISTS bpi (\
            \ID INTEGER PRIMARY KEY AUTOINCREMENT, \
             \usd_id INTEGER NOT NULL, \
             \gbp_id INTEGER NOT NULL, \
             \eur_id INTEGER NOT NULL )" 


bpiToSqlValues :: Bpi -> [SqlValue] 
bpiToSqlValues bpi = [
       toSql $ usd bpi,
       toSql $ gbp bpi,
       toSql $ eur bpi
    ]  

currencyToSqlValues :: Currency -> [SqlValue] 
currencyToSqlValues currency = [
       toSql $ usd_id currency,
       toSql $ gbp_id currency,
       toSql $ eur_id currency
    ]

prepareInsertRecordStmt :: Connection -> IO Statement
prepareInsertRecordStmt conn = prepare conn "INSERT INTO records VALUES (?,?,?,?,?,?,?,?)"

saveRecords :: [Record] -> Connection -> IO ()
saveRecords records conn = do
     stmt <- prepareInsertRecordStmt conn 
     executeMany stmt (map recordToSqlValues records) 
     commit conn

-- CREATE USD TABLE
{-        run conn "CREATE TABLE IF NOT EXISTS usd (\
             \code VARCHAR(40) NOT NULL, \
             \symbol VARCHAR(40) NOT NULL, \
             \rate VARCHAR(40) NOT NULL,  \
             \description VARCHAR(40) NOT NULL )" -}   

-- CREATE GBP TABLE
{-        run conn "CREATE TABLE IF NOT EXISTS gbp (\
             \code VARCHAR(40) NOT NULL, \
             \symbol VARCHAR(40) NOT NULL, \
             \rate VARCHAR(40) NOT NULL,  \
             \description VARCHAR(40) NOT NULL )" -}      

 -- CREATE EUR TABLE
{-        run conn "CREATE TABLE IF NOT EXISTS eur (\
             \code VARCHAR(40) NOT NULL, \
             \symbol VARCHAR(40) NOT NULL, \
             \rate VARCHAR(40) NOT NULL,  \
             \description VARCHAR(40) NOT NULL )" -} 

-- CREATE TIME TABLE
{-        run conn "CREATE TABLE IF NOT EXISTS time (\
             \updated VARCHAR(40) NOT NULL, \
             \updated_ISO VARCHAR(40) NOT NULL, \
             \updateduk VARCHAR(40) NOT NULL )" -}

-- insert cabal deps from finance
