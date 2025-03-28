{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, OverloadedStrings #-}

-- | Module for parsing JSON input files related to package delivery system.
-- This module provides functions to parse constraints, distance matrices, and package data.
module JsonParser
(
  parseConstraints,
  parseInputFiles,
) where
  
import PackageType(createANewPackage, PackageData)
import AirplaneType
import AirportGraph
import Data.Aeson
import GHC.Generics
import System.Exit (die)
import Control.Exception (catch, SomeException)
import qualified Data.ByteString.Lazy as BL
import Control.Monad (unless)
import Data.Time
import Text.Read
import Data.Aeson.Types
import Data.List.Split
import System.Environment
import Text.Regex.TDFA

-- | Constraints for the package delivery system.
-- Contains specifications for planes, weight capacity, and speed.
data Constraints = Constraints
  { numOfPlanes :: Int     -- ^ Number of delivery planes available
  , weightCapacity :: Int  -- ^ Maximum weight capacity per plane
  , speed :: Int           -- ^ Speed of the planes
  } deriving (Show, Generic)

instance FromJSON Constraints
instance ToJSON Constraints

-- | JSON parser instance for 'PackageData'.
-- Parses package information including ID, weight, arrival time, deadline, and destination.
instance FromJSON PackageData where
  parseJSON = withObject "Failed to Parse Package Data" $ \v -> do
    pkgId              <- v .: "id"
    weight          <- v .: "weight"
    arrivalTime     <- v .: "arrivalTime" >>= parseTimeField 
    deadlineTime    <- v .: "deadlineTime" >>= parseTimeField 
    destination     <- v .: "destination"
    return $ createANewPackage pkgId weight arrivalTime deadlineTime destination

-- | Parse a time string in format "HH:MM" into minutes past midnight.
-- Validates time format and returns the result as minutes in a Parser context.
--
-- >>> parseTimeField "14:30"
-- Just 870
parseTimeField :: String -> Parser Int
parseTimeField timeStr = do
  unless (isTimeFormat timeStr) $ fail "Invalid time format for either arrival or deadlineTime in package data"
  case convertTimeOFDayToMinutes timeStr of
    Just minutes -> return minutes
    Nothing -> fail $ "Could not convert " ++ timeStr ++ " to minutes"

-- | Convert a time string in format "HH:MM" to minutes past midnight.
-- Hours can be greater than 24 (e.g., "36:30").
-- Returns Nothing if the format is invalid or parsing fails.
--
-- >>> convertTimeOFDayToMinutes "14:30"
-- Just 870
--
-- >>> convertTimeOFDayToMinutes "36:45"
-- Just 2205
convertTimeOFDayToMinutes :: String -> Maybe Int
convertTimeOFDayToMinutes timeStr =
  case splitByDelim ":" timeStr of
    [hourStr, minuteStr] -> do
      hour <- readMaybe hourStr
      minute <- readMaybe minuteStr
      return (hour * 60 + minute)
    _ -> Nothing  

-- | Splits a string based on a delimiter.
-- Returns a list of substrings.
--
-- >>> splitByDelim ":" "14:30"
-- ["14","30"]
splitByDelim :: String -> String -> [String]
splitByDelim delimiter str  = splitOn delimiter str

-- | Parse package data from a JSON file.
-- Returns a list of PackageData objects or terminates with an error.
--
-- @filePath@ Path to the JSON file containing package data
parsePackageData :: String -> IO [PackageData]
parsePackageData filePath = do
  fileContent <- getFileContent filePath
  case eitherDecode fileContent :: Either String [PackageData] of
    Left errMsg -> die $ "Error parsing JSON from package date file\n File Path \n " ++ filePath ++ ": " ++ errMsg
    Right packagesData -> return packagesData

-- | Parse constraints from a JSON file.
-- Returns a Constraints object or terminates with an error.
--
-- @filePath@ Path to the JSON file containing constraints
parseConstraints :: String -> IO Constraints
parseConstraints filePath = do
  fileContent <- getFileContent filePath
  case decode fileContent of
    Nothing -> die $ "Error parsing JSON From file Path" ++ filePath
    Just constraints -> do
                         unless (verifyConstraints constraints) $ die "Constraint values must be greater than 0"
                         return constraints

-- | Verify that all constraint values are valid (greater than 0).
-- Returns True if all constraints are valid, False otherwise.
verifyConstraints :: Constraints -> Bool
verifyConstraints constraints = all (> 0) [numOfPlanes constraints, weightCapacity constraints, speed constraints]

-- | Parse distance matrix from a JSON file.
-- Returns a matrix of distances or terminates with an error.
--
-- @filePath@ Path to the JSON file containing the distance matrix
parseDistanceMatrix :: String -> IO [[Int]]
parseDistanceMatrix filePath = do
  fileContent <- getFileContent filePath
  let distanceMatrix = decode fileContent :: Maybe [[Int]]
  case distanceMatrix of
    Nothing -> die $ "Error parsing JSON From file Path" ++ filePath
    Just distanceMatrix -> do
                           let resultOfVerification = verifyDistanceMatrix distanceMatrix
                           unless (fst resultOfVerification) $ die $ snd resultOfVerification
                           return distanceMatrix

-- | Read file contents safely, with error handling.
-- Returns the file content as a ByteString or terminates with an error message.
--
-- @filePath@ Path to the file to read
getFileContent :: String -> IO BL.ByteString
getFileContent filePath = BL.readFile filePath `catch` \ (e :: SomeException) ->
    die $ "Error reading file: " ++ filePath ++ "\nError: " ++ show e

-- | Verify that a distance matrix is valid.
-- Checks if the matrix is:
-- * non-empty
-- * square
-- * has valid distances (>= -1)
-- * represents an undirected graph (symmetric)
--
-- Returns a tuple with a boolean result and an error message if applicable.
verifyDistanceMatrix :: [[Int]] -> (Bool, String)
verifyDistanceMatrix matrix
    | null matrix = (False, "Distance matrix cannot be empty")
    | any (/= length matrix) (map length matrix) = (False, "Matrix must be square")
    | any (any (< (-1))) matrix = (False, "Matrix elements must be >= -1")
    | isNotUndirected matrix = (False, "Graph is not undirected")
    | otherwise = (True, "")
    where isNotUndirected matrix = any (\(i, j) -> matrix!!j!!i /= matrix!!i!!j) [(i, j) | i <- [0..length matrix - 1], j <- [0..length matrix - 1]]



-- | Parse all input files required for the package delivery system.
-- Takes command line arguments and returns a tuple containing:
--
-- * A graph of airports derived from the distance matrix
-- * The package data for deliveries
-- * A fleet of airplanes created from the constraints
--
-- The function terminates with an error message if parsing fails or if arguments are invalid.
--
-- Expects three file paths as command line arguments:
-- 1. Distance matrix JSON file
-- 2. Package data JSON file
-- 3. Constraints JSON file
parseInputFiles :: IO ([Airport], [PackageData], [Airplane])
parseInputFiles = do
    args <- getArgs
    if length args /= 3
        then die "Incorrect usage, expected: ./program distance_matrix.json package_data.json constraints.json"
        else do
            [distFile, packageFile, constrFile] <- return args
            distData <- parseDistanceMatrix distFile
            pkgData <- parsePackageData packageFile
            constrData <- parseConstraints constrFile
            let airportGraph = buildAirportGraph distData
            let airplanes = createMultipleAirplanes (speed constrData) (weightCapacity constrData) (numOfPlanes constrData)
            return (airportGraph, pkgData, airplanes)

-- | Check if a string is in the expected time format HH:MM.
-- Hours can be any non-negative integer, minutes must be 00-59.
--
-- >>> isTimeFormat "14:30"
-- True
--
-- >>> isTimeFormat "abc"
-- False
isTimeFormat :: String -> Bool
isTimeFormat str =  (str :: String) =~ ("^([0-9]+):[0-5][0-9]$" :: String)
