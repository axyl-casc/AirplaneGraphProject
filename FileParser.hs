module FileParser
(
    parseInputFiles,
    verifyDistanceMatrix,
    verifyConstraints,
    verifyPackages
) where

import System.Environment (getArgs)
import System.Exit (die)
import qualified Data.Map as Map
import JSONParser (parseSimpleJSON)  -- your custom JSON parser

-- Parse input JSON files specified via command-line arguments
parseInputFiles :: IO ([(String, String)], [(String, String)], [(String, String)])
parseInputFiles = do
    args <- getArgs
    if length args /= 3
        then die "Incorrect usage, expected: ./program distance_matrix.json package_data.json constraints.json"
        else do
            [distFile, pkgFile, constrFile] <- return args
            distData <- parseSimpleJSON distFile
            pkgData <- parseSimpleJSON pkgFile
            constrData <- parseSimpleJSON constrFile
            return (distData, pkgData, constrData)

-- Validate distance matrix structure
verifyDistanceMatrix :: [[Int]] -> Either String ()
verifyDistanceMatrix matrix
    | null matrix = Left "Distance matrix cannot be empty"
    | any (/= length matrix) (map length matrix) = Left "Matrix must be square"
    | any (any (< (-1))) matrix = Left "Matrix elements must be >= -1"
    | otherwise = Right ()

-- Validate constraints structure
verifyConstraints :: Map.Map String Int -> Either String ()
verifyConstraints constr
    | Map.size constr /= 3 = Left "Constraints must contain exactly three keys"
    | not (all (`Map.member` constr) ["numOfPlanes", "weightCapacity", "speed"]) = Left "Missing required constraint keys"
    | any (< 1) (Map.elems constr) = Left "Constraint values must be >= 1"
    | otherwise = Right ()

-- Validate packages structure
verifyPackages :: [Map.Map String String] -> Either String ()
verifyPackages packages
    | null packages = Left "Packages data cannot be empty"
    | otherwise = mapM_ verifyPackage packages
  where
    verifyPackage pkg = do
        let keys = ["id", "weight", "arrivalTime", "destination"]
        if length pkg /= length keys
            then Left "Incorrect number of keys in package"
            else if not (all (`Map.member` pkg) keys)
                then Left "Missing required package keys"
                else Right ()
