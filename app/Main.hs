-- |Module that performs and has access points for our functions
{-# LANGUAGE BlockArguments #-}

module Main where

import HTTP
import Parse
import Database
import Data.Aeson ( encodeFile )
import qualified Data.Text.IO as TIO

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
            conn <- initialiseDB
            print"***  Database Initialized  ***"
            saveTimeRecords (time bits) conn
            print "Live TIME bitcoin data has been saved ..."
            saveUsdRecords (usdCurrency) conn
            print "Live USD bitcoin data has been saved ..."
            saveGbpRecords (gbpCurrency) conn
            print "LIVE GBP bitcoin data has been saved ..."
            saveEurRecords (eurCurrency) conn
            print "LIVE EUR bitcoin data has been saved ..."
            print "All LIVE data now successfully saved to the Database."

           -- This can later go inside the askQuestions function
            resultEUR <- queryItemByCode "EUR" conn
            putStrLn $ "Latest EURO data: " ++ show(resultEUR)

            resultUSD <- queryItemByCode "USD" conn
            putStrLn $ "Latest USD data: " ++ show(resultUSD)

            resultGBP <- queryItemByCode "GBP" conn
            putStrLn $ "Latest GBP data: " ++ show(resultGBP)

            -- write https://api.coindesk.com/v1/bpi/currentprice.json to a file
            createJsonFile
            askQuestions
            askTime
    putStrLn "All done. Disconnecting"        


-- Generates JSON data file
createJsonFile = do
    putStrLn $ "First it's time to create a json file from our parsed data "
    let url = "https://api.coindesk.com/v1/bpi/currentprice.json"
    json <- download url
    case (parse json) of
        Left err -> print err
        Right bits -> do
            print "Want to generate a json representation of our haskell data? Enter 'yes' if yes, or type anything else to move to queries."
            x <- getLine
            if x == "yes" then
                do    
                    print "Writing bitcoin data to file....." 
                    let url = "https://api.coindesk.com/v1/bpi/currentprice.json"
                    json <- download url
                    let jsonString = (parse json) 
                    encodeFile "bitcoin.json" jsonString
            else 
                putStrLn "Alright, no file output created this time." 

askQuestions = do
   putStrLn $ "Now for queries. Which Bitcoin currency you would like to query? Enter USD, GBP or EUR"
   putStrLn $ "(type anything else to quit)"
   currencyAnswer <- getLine
   if currencyAnswer == "EUR" then
      do
         print "Retrieving latest EURO Bitcoin data ....."
         let url = "https://api.coindesk.com/v1/bpi/currentprice.json"
         json <- download url
         case (parse json) of
            Left err -> print err
            Right bits -> do
               let bpiData = bpi bits
               let eurCurrency = eur bpiData
               putStrLn $ "Latest EURO data: " ++ show(eurCurrency)
    else
      putStrLn $ "Thank you for using the Bitcoin app"
   if currencyAnswer == "GBP" then
            do
         print "Retrieving latest GBP STERLING Bitcoin data ....."
         let url = "https://api.coindesk.com/v1/bpi/currentprice.json"
         json <- download url
         case (parse json) of
            Left err -> print err
            Right bits -> do
               let bpiData = bpi bits
               let gbpCurrency = gbp bpiData
               putStrLn $ "Latest GBP data: " ++ show(gbpCurrency)
    else
      putStrLn $ "Thank you for using the Bitcoin app"
   if currencyAnswer == "USD" then
            do
         print "Retrieving latest USD Bitcoin data ....."
         let url = "https://api.coindesk.com/v1/bpi/currentprice.json"
         json <- download url
         case (parse json) of
            Left err -> print err
            Right bits -> do
               let bpiData = bpi bits
               let usdCurrency = usd bpiData
               putStrLn $ "Latest USD data: " ++ show(usdCurrency)
    else
      putStrLn $ "Thank you for using the Bitcoin app"


-- STILL TO DO:
-- Keys in the DB not working
-- Clean up data types in main functions - separate into functions and get rid of repeated data
-- Clean up data types in db functions    
-- Work out how to print just one data point from time e.g. "updated" 
-- IF TIME:
-- Work out how to grab the specific keys for currency e.g. "rate"

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
                    Just time -> putStrLn $ "Time last updated was: " ++ show(getTime json)
    else
      putStrLn $ "Thank you for using the Bitcoin app"


---- NOTES
-- print ("bpi: ")
            -- print (bpi bits)
            -- print(usd(bpi bits))
            -- print "The Bitcoin rate was last updated at: "
            -- print (time bits)



{-main :: IO ()
main = do
    let url = "https://api.coindesk.com/v1/bpi/currentprice.json"
    json <- download url
    print "Parsing... "
    case (parse json) of
        Left err -> print err
        Right bits -> do
            let bpiData = bpi bits
            let usdCurrency = usd bpiData
            let gbpCurrency = gbp bpiData
            let eurCurrency = eur bpiData
            print "USD Bitcoin Rate Info: "
            print(usdCurrency)
            print "GBP Bitcoin Rate Info: "
            print(gbpCurrency)
            print "EUR Bitcoin Rate Info: "
            print(eurCurrency)
            conn <- initialiseDB
            print"Initialized"
            -- Saves time records to db
            saveTimeRecords (time bits) conn
            -- print "Time records saved to db!"
            -- Saves currency records to db
            saveUsdRecords (usdCurrency) conn
            print "USD records saved to db!"
            saveGbpRecords (gbpCurrency) conn
            print "GBP records saved to db!"
            saveEurRecords (eurCurrency) conn
            print "EUR records saved to db!"
            print "All Data successfully saved to the Database."

           -- This can later go inside the askQuestions function
            resultEUR <- queryItemByCode "EUR" conn
            putStrLn $ "Latest EURO data: " ++ show(resultEUR)

            resultUSD <- queryItemByCode "USD" conn
            putStrLn $ "Latest USD data: " ++ show(resultUSD)

            resultGBP <- queryItemByCode "GBP" conn
            putStrLn $ "Latest GBP data: " ++ show(resultGBP)

            -- write https://api.coindesk.com/v1/bpi/currentprice.json to a file
            createJsonFile
            askQuestions
            askTime
    putStrLn "All done. Disconnecting"-}            