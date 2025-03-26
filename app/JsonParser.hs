{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, OverloadedStrings #-}

module JsonParser
(
  Constraints(..),  -- This exports the data type and all its constructors and accessors
  parseConstraints,
  parseDistanceMatrix,
  parseInputFiles,
  -- parsePackageData
) where 
  
import PackageType
import Data.Aeson
import GHC.Generics
import System.Exit (die)
import Control.Exception (catch, SomeException)
import qualified Data.ByteString.Lazy as BL
import Control.Monad (when)
import Data.Time
import Text.Read
import Data.Aeson.Types
import Data.List.Split
import System.Environment

data Constraints = Constraints
  { numOfPlanes :: Int
  , weightCapacity :: Int
  , speed :: Int
  } deriving (Show, Generic)

instance FromJSON Constraints
instance ToJSON Constraints



--instance FromJSON PackageData where
  --parseJSON = withObject "PackageData" $ \v -> do
    --id              <- v .: "id"
    --weight          <- v .: "weight"
    --arrivalTime <- v .: "arrivalTime" >>= convertTimeOFDayToMinutes
    --deadlineTime <- v .: "deadlineTime" >>= convertTimeOFDayToMinutes
    --destination  <- v .: "destination"
    --return $ PackageData id weight arrivalTime deadlineTime destination

-- Function to convert a time string in "hh:mm" format to minutes
--convertTimeOFDayToMinutes :: String -> Parser Int
--convertTimeOFDayToMinutes timeStr = 
  --case splitByDelim ":" timeStr of
    --[hourStr, minuteStr] -> do
      --hour <- readMaybe hourStr
      --minute <- readMaybe minuteStr
      --return (hour * 60 + minute)
    --_ -> fail $ "Invalid time format: " ++ timeStr ++ ". Expected format is hh:mm."


-- Splits a string based on a delimter
splitByDelim :: String -> String -> [String]
splitByDelim str delimiter = splitOn delimiter str     

--parsePackageData :: String -> IO [PackageData]
--parsePackageData filePath = do
  --fileContent <- getFileContent filePath
  --case eitherDecode fileContent :: Either String [PackageData] of
    --Left errMsg -> die $ "Error parsing JSON from package date file\n File Path \n " ++ filePath ++ ": " ++ errMsg
    --Right packagesData -> return packagesData




parseConstraints :: String -> IO Constraints
parseConstraints filePath = do
  fileContent <- getFileContent filePath
  case decode fileContent of
    Nothing -> die $ "Error parsing JSON From file Path" ++ filePath 
    Just constraints -> do
                         when ( not (verifyConstraints constraints) ) $ die $ "Constraint values must be greater than 0"
                         return constraints


verifyConstraints :: Constraints -> Bool
verifyConstraints constraints = any ( > 1) [numOfPlanes constraints, weightCapacity constraints, speed constraints]




parseDistanceMatrix :: String -> IO [[Int]]
parseDistanceMatrix filePath = do
  fileContent <- getFileContent filePath
  let distanceMatrix = decode fileContent :: Maybe [[Int]]
  case distanceMatrix of
    Nothing -> die $ "Error parsing JSON From file Path" ++ filePath 
    Just distanceMatrix -> do
                           let resultOfVerification = verifyDistanceMatrix distanceMatrix
                           when ( not (fst resultOfVerification) ) $ die $ snd resultOfVerification
                           return distanceMatrix


getFileContent :: String -> IO BL.ByteString
getFileContent filePath = do
  fileContent <- BL.readFile filePath `catch` \ (e :: SomeException) -> do
    die $ "Error reading file: " ++ filePath ++ "\nError: " ++ show (e)
  return fileContent


verifyDistanceMatrix :: [[Int]] -> (Bool, String)
verifyDistanceMatrix matrix
    | null matrix = (False, "Distance matrix cannot be empty")
    | any (/= length matrix) (map length matrix) = (False,"Matrix must be square")
    | any (any (< (-1))) matrix = (False ,"Matrix elements must be >= -1")
    | isNotUndirected matrix = (False ,"Graph is not undirected")
    | otherwise = (True, "")
    where isNotUndirected matrix = any (\(i, j) -> matrix!!j!!i /= matrix!!i!!j) [(i, j) | i <- [0..length matrix - 1], j <- [0..length matrix - 1]]


parseInputFiles :: IO ([[Int]], Constraints)
parseInputFiles = do
    args <- getArgs
    if length args /= 3
        then die "Incorrect usage, expected: ./program distance_matrix.json package_data.json constraints.json"
        else do
            [distFile, pacakgeFile, constrFile] <- return args
            distData <- parseDistanceMatrix distFile
           -- pkgData <- parsePackageData pkgFile
            constrData <- parseConstraints constrFile
            return (distData,  constrData)