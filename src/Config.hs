{-# LANGUAGE DeriveGeneric #-}
module Config (getConfig, hospitals, joint, Config, cheapestFree) where
import Data.Aeson ( decode, FromJSON, ToJSON )
import GHC.Generics ( Generic )
import Data.ByteString.Lazy.UTF8 (fromString)

-- |Config represents the "static" variables
data Config = Config {joint :: Double, hospitals :: [Double]} deriving (Generic, Show, Eq)

instance FromJSON Config
instance ToJSON Config

-- |getConfig returns a config from the json file given
getConfig :: String -> IO Config
getConfig path = do
    f <- readFile path
    let conf = decode (fromString f) :: Maybe Config
    case conf of
        Just c -> return c
        Nothing -> return defaultConfig

defaultConfig :: Config
defaultConfig = Config 0 [0]

-- |cheapestFree returns two doubles, the cheapest when joint Striking, and the cheapest otherwise
cheapestFree :: Config -> (Double, Double)
cheapestFree conf = (calc conf True, calc conf False) where
    calc c b  
        | b = sum (hospitals c) / (1 - joint c)
        | otherwise = sum (hospitals c)