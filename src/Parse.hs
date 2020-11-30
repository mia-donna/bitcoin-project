{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module Parse where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Generics ( Generic )
import Data.Aeson.Types    
import qualified Data.Char
import Data.Text ( Text )
import Data.Aeson.TH(deriveJSON, defaultOptions, Options(fieldLabelModifier))

-- CURRENCY

data Currency = Currency {
     code :: String,
     symbol :: String,
     rate :: String,
     description :: String,
     rate_float :: Double
 } deriving (Show, Generic)

 -- BPI

data Bpi = Bpi {
   usd :: Currency,
   gbp :: Currency,
   eur :: Currency
} deriving (Show, Generic)

-- This converts currency from lower to uppercase (eg. usd to USD)
$(deriveJSON defaultOptions {
    fieldLabelModifier = \x -> 
        if x == "usd" 
            then "USD" 
        else if x == "gbp" 
            then "GBP" 
        else if x == "eur" 
            then "EUR" 
        else x} ''Bpi)

-- TIME
data Time = Time {
    updated :: String,
    updatedISO :: String, 
    updateduk :: String
}  deriving (Show, Generic)

-- BITCOIN
data Bitcoin = Bitcoin {
   time :: Time,
   disclaimer :: String,
   chartName :: String,
   bpi :: Bpi
} deriving (Show, Generic)

-- Instances 
instance FromJSON Bitcoin
instance ToJSON Bitcoin

instance FromJSON Time
instance ToJSON Time

instance FromJSON Currency
instance ToJSON Currency

-- Datatype for the list of Currencys (like list of Records) -- as I couldn't get currencys to map to db, maybe we need to create a new datatype for currency
--data Currencys = Currencys {
--            currencys :: [Currency]
--      }  deriving (Show, Generic)

--instance FromJSON Currencys
--instance ToJSON Currencys 

parse :: L8.ByteString -> Either String Bitcoin
parse json = eitherDecode json :: Either String Bitcoin

--parseCurrencys :: L8.ByteString -> Either String Currencys
--parseCurrencys json = eitherDecode json :: Either String Currencys
  
--encodeFile :: ToJSON a => FilePath -> a -> IO ()
--encodeFile = encodeFile json :: 