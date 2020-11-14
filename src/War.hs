module War (handler) where

import System.Environment (getArgs)
import Data.Sequence (update, fromList)
import Data.Foldable (toList)
import Config ( getConfig, hospitals, joint, Config )
import Text.Printf

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

data Calculated = Calculated Paths Costs deriving (Eq)

instance Show Calculated where
    show (Calculated p c) = printf "%s\n%s\n" (show p) (show c)

parseAndCalculate :: Config -> [String] -> Calculated
parseAndCalculate conf args = Calculated parsed (map (calc conf) parsed)
    where parsed = parseArgs args

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
    let calc = parseAndCalculate config arg
    putStr $ show calc
    -- let parsed = parseArgs arg
    -- print parsed
    -- print $ map (calc config) parsed


