-- |MODULE that performs parsing of our html url and also creates Haskell data types
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
import Control.Lens ( preview )
import Data.Aeson.Lens ( key, _String )
import qualified Data.HashMap.Strict as HM 


-- |CREATING A HASKELL DATA TYPE FOR CURRENCY

data Currency = Currency {
     code :: String,
     symbol :: String,
     rate :: String,
     description :: String,
     rate_float :: Double
 } deriving (Show, Generic)

-- |CREATING A HASKELL DATA TYPE FOR BPI

data Bpi = Bpi {
   usd :: Currency,
   gbp :: Currency,
   eur :: Currency
} deriving (Show, Generic)

-- |This converts currency from lower to uppercase (eg. usd to USD)
$(deriveJSON defaultOptions {
    fieldLabelModifier = \x -> 
        if x == "usd" 
            then "USD" 
        else if x == "gbp" 
            then "GBP" 
        else if x == "eur" 
            then "EUR" 
        else x} ''Bpi)

-- |CREATING A HASKELL DATA TYPE FOR TIME
data Time = Time {
    updated :: String,
    updatedISO :: String, 
    updateduk :: String
}  deriving (Show, Generic)

-- |CREATING A HASKELL DATA TYPE FOR BITCOIN
data Bitcoin = Bitcoin {
   time :: Time,
   disclaimer :: String,
   chartName :: String,
   bpi :: Bpi
} deriving (Show, Generic)

-- |OUR ToJSON and FromJSON instances
instance FromJSON Bitcoin
instance ToJSON Bitcoin

instance FromJSON Time
instance ToJSON Time

instance FromJSON Currency
instance ToJSON Currency

-- |PERFORMS PARSING of all our BITCOIN DATA 
parse :: L8.ByteString -> Either String Bitcoin
parse json = eitherDecode json :: Either String Bitcoin

-- |PERFORMS PARSING of our TIME data using LENS

getTime :: L8.ByteString -> Maybe Text
getTime = preview (key "time" . key "updated" . _String)

-- |PERFORMS DB DUMP conversion of Haskell -> JSON for json file
bpiEncode :: [Bpi] -> Object
bpiEncode = HM.unions . map (\(Object bpi) -> bpi) . map toJSON