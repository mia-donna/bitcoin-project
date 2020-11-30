{-# LANGUAGE BlockArguments #-}

module Main where

import HTTP
import Parse
import Database
import Data.Aeson ( encodeFile )

main :: IO ()
main = do
    let url = "https://api.coindesk.com/v1/bpi/currentprice.json"
    json <- download url
    print "Parsing... "
    case (parse json) of
        Left err -> print err
        Right bits -> do
            print "The Bitcoin rate was last updated at: "
            print (time bits)
            -- print ("bpi: ")
            -- print (bpi bits)
            -- print(usd(bpi bits))
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
            -- write https://api.coindesk.com/v1/bpi/currentprice.json to a file
            createJs
    print "Done!"        

-- save bpi bits to a filename
 {-}   createFile :: IO ()
    createFile jsonfile = do
        putStrLn "Let's create a json file"
        print (bpi bits)
        let jsonfile = putStrLn $ bpiData 
load =
   do
   lst <- writeFile "bitcoin.txt"
   let new = encode lst
   case new of
      Nothing -> 
         error "Incorrect file format"
      Just n ->
         start n
-}
createJs = do
    putStrLn $ "Time to create a json file!! "
    let url = "https://api.coindesk.com/v1/bpi/currentprice.json"
    json <- download url
    print "Parsing json again... "
    case (parse json) of
        Left err -> print err
        Right bits -> do
            print "Writing BITCOIN DATA: https://api.coindesk.com/v1/bpi/currentprice.json to file" 
            let url = "https://api.coindesk.com/v1/bpi/currentprice.json"
            json <- download url
            let jsonString = (parse json) 
            encodeFile "bitcoin.json" jsonString