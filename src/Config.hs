{-# LANGUAGE DeriveGeneric #-}
module Config (getConfig, hospitals, joint, Config) where
import Data.Aeson ( decode, FromJSON, ToJSON )
import GHC.Generics ( Generic )
import Data.ByteString.Lazy.UTF8 (fromString)

-- type Config = (Double, [Double])
data Config = Config {joint :: Double, hospitals :: [Double]} deriving (Generic, Show)

instance FromJSON Config
instance ToJSON Config

getConfig :: String -> IO Config
getConfig path = do
    f <- readFile path
    let conf = decode (fromString f) :: Maybe Config
    case conf of
        Just c -> return c
        Nothing -> return defaultConfig

defaultConfig = Config 0 [0]
