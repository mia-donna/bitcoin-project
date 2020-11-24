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

-- Lower level currencys 

data Currency = Currency {
     code :: String,
     symbol :: String,
     rate :: String,
     description :: String,
     rate_float :: Float
 } deriving (Show, Generic)

 -- The Currencys data structure

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

-- The first data structure with parents
data Time = Time {
    updated :: String,
    updatedISO :: String, 
    updateduk :: String
}  deriving (Show, Generic)

-- The high-level data structure
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

parse :: L8.ByteString -> Either String Bitcoin
parse json = eitherDecode json :: Either String Bitcoin
