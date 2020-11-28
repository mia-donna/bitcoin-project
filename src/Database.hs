{-# LANGUAGE BlockArguments #-}

module Database 
    (initialiseDB,
    saveRecords
    ) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Parse


-- add HDBC dependencies to stack.yaml + package.yaml
-- creates access to a db while takes filename as a parameter

initialiseDB :: IO Connection
initialiseDB =
 do
    conn <- connectSqlite3 "bitcoin.sqlite"
    run conn "CREATE TABLE IF NOT EXISTS bpi (\
          \id INTEGER PRIMARY KEY AUTOINCREMENT, \
          \usd_id INTEGER NOT NULL, \
          \gbp_id INTEGER NOT NULL, \
          \eur_id INTEGER NOT NULL )"
    run conn "CREATE TABLE IF NOT EXISTS usd (\
          \usd_id INTEGER PRIMARY KEY, \
          \code VARCHAR(40) NOT NULL, \
          \symbol VARCHAR(40) NOT NULL, \
          \rate VARCHAR(40) NOT NULL,  \
          \description VARCHAR(40) NOT NULL
          \FOREIGN KEY (usd_id) REFERENCES bpi (usd_id))"
    run conn "CREATE TABLE IF NOT EXISTS gbp (\
          \gbp_id INTEGER PRIMARY KEY, \
          \code VARCHAR(40) NOT NULL, \
          \symbol VARCHAR(40) NOT NULL, \
          \rate VARCHAR(40) NOT NULL,  \
          \description VARCHAR(40) NOT NULL \
          \FOREIGN KEY (gbp_id) REFERENCES bpi (gbp_id))"
    run conn "CREATE TABLE IF NOT EXISTS eur (\
          \eur_id INTEGER PRIMARY KEY, \
          \code VARCHAR(40) NOT NULL, \
          \symbol VARCHAR(40) NOT NULL, \
          \rate VARCHAR(40) NOT NULL,  \
          \description VARCHAR(40) NOT NULL \
          \FOREIGN KEY (eur_id) REFERENCES bpi (eur_id))"
    run conn "CREATE TABLE IF NOT EXISTS time (\
          \id INTEGER PRIMARY KEY AUTOINCREMENT, \
          \updated VARCHAR(40) NOT NULL, \
          \updated_ISO VARCHAR(40) NOT NULL, \
          \updateduk VARCHAR(40) NOT NULL \ 
          \) " []                           
    commit conn
    return conn


-- to sql values

bpiToSqlValues :: Bpi -> [SqlValue] 
bpiToSqlValues bpi = [
       toSql $ usd bpi,
       toSql $ gbp bpi,
       toSql $ eur bpi
    ]  

currencyToSqlValues :: Currency -> [SqlValue] 
currencyToSqlValues currency = [
       toSql $ code currency,
       toSql $ symbol currency,
       toSql $ rate currency,
       toSql $ description currency,
       toSql $ rate_float currency
    ]

currencyToSqlValues :: Time -> [SqlValue] 
timeToSqlValues time = [
       toSql $ updated time,
       toSql $ updated_ISO time,
       toSql $ updateduk time
    ]

bitcoinToSqlValues :: Bitcoin -> [SqlValue] 
timeToSqlValues time = [
       toSql $ time bitcoin,
       toSql $ disclaimer bitcoin,
       toSql $ chartName bitcoin,
       toSql $ bpi bitcoin
    ]
