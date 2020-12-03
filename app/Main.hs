-- |Module that performs and has access points for our functions
{-# LANGUAGE BlockArguments #-}

module Main where

import HTTP
import Parse
import Database
import Data.Aeson ( encodeFile, encode )
import qualified Data.ByteString.Lazy.Char8 as L8


-- || MAIN FUNCTION : Access points for parsing live data, saving data to the db, creating a json file and asking questions
main :: IO ()
main = do
    let url = "https://api.coindesk.com/v1/bpi/currentprice.json"
    json <- download url
    print "Parsing live bitcoin data from COINDESK... "
    case (parse json) of
        Left err -> print err
        Right bits -> do
            let bpiData = bpi bits
            let usdCurrency = usd bpiData
            let gbpCurrency = gbp bpiData
            let eurCurrency = eur bpiData
            --let writeDB = encode $ bpiData 
            --L8.writeFile "DB.json" writeDB
            conn <- initialiseDB
            print"***  Database Initialized  ***"
            saveTimeRecords (time bits) conn
            print "LIVE TIME bitcoin data has been saved ..."
            saveUsdRecords (usdCurrency) conn
            print "LIVE USD bitcoin data has been saved ..."
            saveGbpRecords (gbpCurrency) conn
            print "LIVE GBP bitcoin data has been saved ..."
            saveEurRecords (eurCurrency) conn
            print "LIVE EUR bitcoin data has been saved ..."
            linkTables conn
            createJsonFiles
            askQuestions
            askTime
    putStrLn "All done. Disconnecting"        


-- || SAVE FOREIGN KEY DATA: This creates a linking table in the database that introduces relationships between tables
linkTables conn = do
   usdId <- getCurrencyId "usd" conn
   gbpId <- getCurrencyId "gbp" conn
   eurId <- getCurrencyId "eur" conn
   time <- queryTime conn
   insertIntoLinkingTable usdId gbpId eurId time conn
   putStrLn $ "All LIVE data now successfully saved, data last updated at: " ++ show(time)

-- || JSON FILE: This generates JSON representation from our parsed haskell data and dumps it to a file
createJsonFiles = do
    putStrLn $ "First it's time to create two json files from our parsed data and database."
    let url = "https://api.coindesk.com/v1/bpi/currentprice.json"
    json <- download url
    case (parse json) of
        Left err -> print err
        Right bits -> do
            let bpiData = bpi bits
            let writeDB = encode $ bpiData
            print "Want to generate a json representation of our haskell data? Enter 'yes' if yes, or type anything else to move to queries."
            x <- getLine
            if x == "yes" then
                do    
                    print "Awesome! First we're parsing and writing live bitcoin data to new file 'bitcoin.json'....." 
                    let url = "https://api.coindesk.com/v1/bpi/currentprice.json"
                    json <- download url
                    let jsonString = (parse json) 
                    encodeFile "bitcoin.json" jsonString
                    print "Done! Now we're generating another file directly from the database on bpi data..."
                    L8.writeFile "DB-BPI.json" writeDB
                    print "Great! Now you've got a second file DB-BPI.json from our database with just the latest BPI data."
            else 
                putStrLn "Alright, no file output created this time." 

-- || ASK QUERIES 1: We ask our user questions and pull data from our db to answer them
askQuestions = do
   putStrLn $ "Now for queries. Which Bitcoin currency rate you would like to query? Enter USD, GBP or EUR"
   putStrLn $ "(type anything else to quit)"
   currencyAnswer <- getLine
   if currencyAnswer == "EUR" then
      do
         conn <- initialiseDB
         resultEUR <- queryItemByCode "EUR" conn
         putStrLn $ "Here's the latest EURO rate data: " ++ show(resultEUR)
   else if currencyAnswer == "GBP" then
      do
         conn <- initialiseDB
         resultGBP <- queryItemByCode "GBP" conn
         putStrLn $ "Here's the latest GBP rate data: " ++ show(resultGBP) 
   else if currencyAnswer == "USD" then
      do
         conn <- initialiseDB
         resultUSD <- queryItemByCode "USD" conn
         putStrLn $ "Here's the latest USD rate data: " ++ show(resultUSD)
   else
      putStrLn $ "Thank you for using the Bitcoin app"


-- || ASK QUERIES 2: We ask our user if they want time data and we use previews and keys to parse and grab from url 
askTime = do 
   putStrLn $ "Would you like to know when the bitcoin rate was last updated? Type 'yes' to proceed"
   putStrLn $ "(type anything else to quit)"
   timeAnswer <- getLine
   if timeAnswer == "yes" then
      do
         print "Retrieving latest TIME Bitcoin data ....."
         let url = "https://api.coindesk.com/v1/bpi/currentprice.json"
         json <- download url
         case (parse json) of
            Left err -> print err
            Right bits -> do
               case getTime json of 
                    Nothing -> putStrLn $ "Could not find the Bitcoin time :("
                    Just time -> putStrLn $ "The Time the bitcoin currencies were last updated was: " ++ show(getTime json)
   else
      putStrLn $ "Thank you for using the Bitcoin app"              