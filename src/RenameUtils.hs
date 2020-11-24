module RenameUtils where

gbpCurrencyFieldRename :: String -> String
gbpCurrencyFieldRename "gbp" = "GBP"
gbpCurrencyFieldRename name = name

usdCurrencyFieldRename :: String -> String
usdCurrencyFieldRename "usd" = "USD"
usdCurrencyFieldRename name = name

eurCurrencyFieldRename :: String -> String
eurCurrencyFieldRename "eur" = "EUR"
eurCurrencyFieldRename name = name