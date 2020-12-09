-- |Module that performs and has access points for our functions
{-# LANGUAGE BlockArguments #-}

module Main where

import HTTP
import Parse
import Database
import Data.Aeson ( encodeFile, encode )
import qualified Data.ByteString.Lazy.Char8 as L8

-- |MAIN FUNCTION : Access points for parsing live data, saving data to the db, creating json files and asking questions
main :: IO ()
main = do
    let url = "https://api.coindesk.com/v1/bpi/currentprice.json"
    json <- download url
    print "Parsing live bitcoin data from COINDESK... "
    case (parse json) of
        Left err -> print err
        Right bits -> do
           -- give names to values extracted from the parsed json
            let bpiData = bpi bits
            let usdCurrency = usd bpiData
            let gbpCurrency = gbp bpiData
            let eurCurrency = eur bpiData
            -- initialize the database connection
            conn <- initialiseDB
            print"***  Database Initialized  ***"
            -- save data into time table
            saveTimeRecords (time bits) conn
            print "LIVE TIME bitcoin data has been saved ..."
            -- save data into and USD currency table
            saveUsdRecords (usdCurrency) conn
            print "LIVE USD bitcoin data has been saved ..."
            -- save data into and GBP currency table
            saveGbpRecords (gbpCurrency) conn
            print "LIVE GBP bitcoin data has been saved ..."
            -- save data into and EUR currency table
            saveEurRecords (eurCurrency) conn
            print "LIVE EUR bitcoin data has been saved ..."
            -- create a linkingTable which fill store key to key relations
            linkTables conn
            -- create JSON files
            createJsonFiles
            -- get input from the user and return data from queries
            askQuestions conn
            -- ask user if they want to print time
            askTime conn

    putStrLn "\n\nAll done. Thank you for using the Bitcoin app. Disconnecting"        


-- |SAVE FOREIGN KEY DATA: This function queries primary keys from tables already created and 
-- uses them to form a linking table with foreign keys creating relationships between tables
linkTables conn = do

   usdId <- getCurrencyId "usd" conn
   gbpId <- getCurrencyId "gbp" conn
   eurId <- getCurrencyId "eur" conn
   time_updated <- queryTime conn

   -- pass the key data retrieved into function that will insert them into foreign key table
   insertIntoLinkingTable usdId gbpId eurId time_updated conn

   -- inform the user on successful saving of the data
   putStrLn $ "All LIVE data now successfully saved"

-- |Styles the output of the output of queryAll function, which returns data for all currencies
-- IMPORTANT ! We assume that this function always takes an argument of the same structure ["currency code", "rate", "currency code", "rate"...] in following order: USD, GBP, EUR
printAllCurrencies conn = do
   allCurrencies <- queryAll conn -- queryAll returns results from join query of all currency tables
   let styleCurrencies (x:xs) = if x == "USD" then do -- find the first instance of USD and return following value then continue with the rest of the list
                                 print("Latest USD rate: "++ head xs)
                                 styleCurrencies xs
                              else if x == "GBP" then do -- find the first instance of USD and return following value then continue with the rest of the list
                                 print("Latest GBP rate: "++ head xs)
                                 styleCurrencies xs
                              else if x == "EUR" then do -- find the first instance of USD and return following value then continue with the rest of the list
                                 print("Latest EUR rate: "++ head xs)
                                 styleCurrencies xs
                              else do -- if the list doest't have at lest 2 values then finish reading the list
                                 if length xs < 2 then
                                    print("That's all data for today")
                                 else
                                    styleCurrencies xs
   styleCurrencies allCurrencies


-- |JSON FILE: This generates JSON representations from our parsed haskell data and dumps it to files
createJsonFiles = do
    putStrLn $ "\n\nFirst it's time to create two json files from our parsed data and database."
    let url = "https://api.coindesk.com/v1/bpi/currentprice.json"
    json <- download url
    case (parse json) of
        Left err -> print err
        Right bits -> do
            let bpiData = bpi bits
            let writeDB = encode $ bpiData
            putStrLn $ "Do you want to generate a json representation of our haskell data? Enter 'yes' if yes"
            putStrLn $ "(or type anything else to continue)"
            x <- getLine
            if elem x ["yes", "YES", "y", "Y"] then
                do    
                    putStrLn $ "Awesome! First we're parsing and writing live bitcoin data to new file 'bitcoin.json'....." 
                    let url = "https://api.coindesk.com/v1/bpi/currentprice.json"
                    json <- download url
                    let jsonString = (parse json) 
                    encodeFile "bitcoin.json" jsonString
                    putStrLn $ "Done! Now we're generating another file directly from the database on bpi data..."
                    L8.writeFile "DB-BPI.json" writeDB
                    putStrLn $ "Great! Now you've got a second file DB-BPI.json from our database with just the latest BPI data."
            else 
                putStrLn "Alright, no file output created this time." 

-- |ASK QUERIES 1: We ask our user questions and pull data from our db to answer them
askQuestions conn = do
   putStrLn $ "\n\nNow for queries. Which Bitcoin currency rate you would like to query? Enter USD, GBP, EUR or ALL"
   putStrLn $ "(type anything else to continue)"

   currencyAnswer <- getLine
   if elem currencyAnswer ["EUR", "eur"] then
      do
         resultEUR <- queryItemByCode "EUR" conn
         putStrLn $ "\n\nHere's the latest EURO rate: " ++ show(resultEUR)

   else if elem currencyAnswer ["GBP", "gbp"] then
      do
         resultGBP <- queryItemByCode "GBP" conn
         putStrLn $ "\n\nHere's the latest GBP rate: " ++ show(resultGBP) 

   else if elem currencyAnswer ["USD", "usd"] then
      do
         resultUSD <- queryItemByCode "USD" conn
         putStrLn $ "\n\nHere's the latest USD rate: " ++ show(resultUSD)

   else if elem currencyAnswer ["ALL", "all"] then
      do
         printAllCurrencies conn
   else
      putStrLn $ "\n\n"


-- |ASK QUERIES 2: We ask our user if they want to get time data 
   -- disclaimer: there are two ways we use to get time: one is to query the database or to get it directly from json file
askTime conn = do 
   putStrLn $ "\n\nWould you like to know when the bitcoin rate was last updated? Type 'yes' to proceed"
   putStrLn $ "(type anything else to quit)"
   timeAnswer <- getLine
   
   if elem timeAnswer ["yes", "YES", "y", "Y"] then do
      putStrLn $ "\n\nYou can either query the database or return JSON data? Type 'DB' or 'JSON' to choose"
      putStrLn $ "(type anything else to quit)"
      dbJsonAnswer <- getLine

      if elem dbJsonAnswer ["json", "JSON"] then do
         putStrLn $ "Retrieving latest time Bitcoin data ....."
         let url = "https://api.coindesk.com/v1/bpi/currentprice.json"
         json <- download url
         case (parse json) of
            Left err -> print err
            Right bits -> do
               case getTime json of 
                  Nothing -> putStrLn $ "Could not find the Bitcoin time :("
                  Just time -> putStrLn $ "\n\nThe bitcoin currencies were last updated was at: " ++ show(time)
      else if elem dbJsonAnswer ["db", "DB", "database", "DATABASE"] then do
         putStrLn $ "Retrieving latest time Bitcoin data ....."
         time <- queryTime conn
         putStrLn $ "\n\nThe bitcoin currencies were last updated was at: " ++ show(time)
      else putStrLn $ "\n\nThank you for using the Bitcoin app"
   else putStrLn $ "\n\nThank you for using the Bitcoin app"          