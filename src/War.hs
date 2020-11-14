module War (handler) where

import System.Environment (getArgs)
import Data.Sequence (update, fromList)
import Data.Foldable (toList)
import Config ( getConfig, hospitals, joint, Config )


-- hospitals :: [Double]
-- hospitals = [0.068, 1.25, 8.83, 17.0]

-- joint :: Double
-- joint = 0.15

calcNode :: Config -> Node -> Double
calcNode conf (node, False) = max 0 $ node - sum (hospitals conf)
calcNode conf (node, True) = max 0 $ node - (sum (hospitals conf) + (node * joint conf))

calc :: Config -> Path -> Cost
calc conf path = sum $ map (calcNode conf) path

strToDouble :: [String] -> Path
strToDouble = map parsePath

type Paths = [Path]

type Path = [Node]

type Node = (Double, Bool)

type Costs = [Cost]
type Cost = Double

data Calculated = Calculated Paths Costs deriving (Show, Eq)

prettyPrintCalc :: Calculated -> String
prettyPrintCalc calc = undefined

parseArgs :: [String] -> Paths
parseArgs args = map strToDouble $ separateList args

parsePath :: String -> Node
parsePath path 
    | last path == 'j' = (read (init path) :: Double, True)
    | otherwise = (read path :: Double, False)


separateList :: [String] -> [[String]]
separateList args = recursiveFunc args [[]] 0 0

separator :: String
separator = ","

recursiveFunc :: [String] -> [[String]] -> Int -> Int -> [[String]]
recursiveFunc args build i count 
    | i == length args = build
    | args !! i == separator = recursiveFunc args (build++[[]]) (i + 1) (count + 1)
    | otherwise = recursiveFunc args newlist (i + 1) count where
        newlist = toList $ update count ((build!!count) ++ [args !! i]) (fromList build)


handler :: IO ()
handler = do 
    config <- getConfig "./config.json"
    arg <- getArgs
    print (parseArgs arg)
    print $ map (calc config) (parseArgs arg)


