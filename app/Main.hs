module Main where

import HTTP
import Parse
import Database

main :: IO ()
main = do
    let url = "https://api.coindesk.com/v1/bpi/currentprice.json"
    json <- download url
    case (parse json) of
        Left err -> print err
        Right bits -> do
            print ("time: ")
            print (bpi bits)
            print "Saving on DB.."
            conn <- initialiseDB
            print("Initialized")
            -- Saves time records to db
            saveTimeRecords (time bits) conn
            print "Time records saved to db!"
    case (parseCurrencys json) of
        Left err -> print err
        Right curr -> do
            print "Saving on DB.."
            conn <- initialiseDB
            print("Initialized")
            -- Saves currency records to db
            saveCurrencyRecords (currencys curr) conn
            print "Currency records saved to db!"
    print "Finished"        

-- stuck here : "Error in $: parsing Parse.Currencys(Currencys) failed, key \"currencys\" not found"
-- But we created the data type for currencys so we shouldn't be looking for this key
