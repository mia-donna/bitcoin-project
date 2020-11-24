{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module Parse where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Generics ( Generic )
import Data.Aeson.Types    
import qualified Data.Char
import RenameUtils ( gbpCurrencyFieldRename, usdCurrencyFieldRename, eurCurrencyFieldRename )
import Data.Text ( Text )
import Data.Aeson.TH(deriveJSON, defaultOptions, Options(fieldLabelModifier))


-- The high-level data structure
data Bitcoin = Bitcoin {
   time :: Time,
   disclaimer :: String,
   chartName :: String,
   bpi :: Bpi
} deriving (Show, Generic)


-- The first data structure 
data Time = Time {
    updated :: String,
    updatedISO :: String, 
    updateduk :: String
}  deriving (Show, Generic)

-- Lower level currencys 

data CurrencyValue = CurrencyValue {
     code :: String,
     symbol :: String,
     rate :: String,
     description :: String,
     rate_float :: Float
 } deriving (Show, Generic)


-- The Currencys data structure

data Bpi = Bpi {
   usd :: CurrencyValue
} deriving (Show, Generic)

$(deriveJSON defaultOptions {fieldLabelModifier = usdCurrencyFieldRename } ''Bpi)

instance FromJSON Bitcoin
instance ToJSON Bitcoin

instance FromJSON Time
instance ToJSON Time

instance FromJSON CurrencyValue
instance ToJSON CurrencyValue

parse :: L8.ByteString -> Either String Bitcoin
parse json = eitherDecode json :: Either String Bitcoin

