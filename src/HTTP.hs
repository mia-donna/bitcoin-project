-- |Module that performs http requests
module HTTP
    ( download
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple

type URL = String 

-- |"download" function PERFORMS DOWNLOAD of our bitcoin url
download :: String -> IO L8.ByteString
download url = do
    request <- parseRequest url 
    response <- httpLBS request
    return $ getResponseBody response