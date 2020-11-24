module Main where

import HTTP
import Parse

main :: IO ()
main = do
    let url = "https://api.coindesk.com/v1/bpi/currentprice.json"
    json <- download url
    case (parse json) of
        Left err -> print err
        Right bits -> print (bpi bits)

