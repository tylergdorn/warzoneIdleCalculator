{-# LANGUAGE DeriveGeneric #-}
module Config (getConfig, hospitals, joint, Config) where
import Data.Aeson ( decode, FromJSON, ToJSON )
import GHC.Generics
import Data.ByteString.Lazy.UTF8 (fromString)

-- type Config = (Double, [Double])
data Config = Config {joint :: Double, hospitals :: [Double]} deriving (Generic, Show)

instance FromJSON Config
instance ToJSON Config

getConfig :: String -> IO Config
getConfig path = do
    f <- readFile path
    return (getConf f)


getConf :: String -> Config
getConf s = safeConfig (decode (fromString s) :: Maybe Config)

safeConfig :: Maybe Config -> Config
safeConfig (Just c) = c
safeConfig Nothing = defaultConfig

defaultConfig = Config 0 [0]
