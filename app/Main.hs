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
            print (time bits)
            print "Saving on DB.."
            conn <- initialiseDB
            print("Initialized")
            -- Saves time records to db
            saveTimeRecords (time bits) conn
    print "Time records saved to db!"
            

