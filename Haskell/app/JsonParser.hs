-- These pragmas are need to handle the parsing with the Aeson Library
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : JsonParser
--
-- Module for parsing JSON input files related to package delivery system.
-- This module provides functions to parse constraints, distance matrices, and package data.
-- 

module JsonParser
  ( parseConstraints,
    parseInputFiles,
  )
where

import AirplaneType
import AirportGraph
import Control.Exception (SomeException, catch)
import Control.Monad (unless)
import Data.Aeson (FromJSON, decode, eitherDecode)
import Data.Aeson.Types ()
import qualified Data.ByteString.Lazy as BL
import Data.List.Split ()
import GHC.Generics (Generic)
import PackageType (PackageData)
import System.Environment (getArgs)
import System.Exit (die)
import Text.Read ()
import Text.Regex.TDFA ()

-- | Constraints for the package delivery system.
-- Contains specifications for planes, weight capacity, and speed.



-- |
--  Represents constraints for the package delivery system.
--
--  === Fields:
--  * @numOfPlanes@ - Number of delivery planes available
--  * @weightCapacity@ - Maximum weight capacity per plane
--  * @speed@ - Speed of the planes
data Constraints = Constraints
  { 
    numOfPlanes :: Int,
    weightCapacity :: Int,
    speed :: Int
  }
  deriving (Show, Generic)

-- Allows Aeson library to create a generic mapping from json to constraints.Used when "decode" function is used
instance FromJSON Constraints

-- | Parse packages data from a JSON file.
-- Returns a list of PackageData objects or terminates with an error.
--
-- @param filePath@ Path to the JSON file containing packages data
parsePackageData :: String -> IO [PackageData]
parsePackageData filePath = do
  fileContent <- getFileContent filePath
  case eitherDecode fileContent :: Either String [PackageData] of
    Left errMsg -> die $ "Error parsing JSON from package date file\n File Path \n " ++ filePath ++ ": " ++ errMsg
    Right packagesData -> return packagesData

-- | Parse constraints from a JSON file.
-- Returns a Constraints object or terminates with an error.
--
-- @param filePath@ Path to the JSON file containing constraints
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
-- @param filePath@ Path to the JSON file containing the distance matrix
parseDistanceMatrix :: String -> IO [[Int]]
parseDistanceMatrix filePath = do
  fileContent <- getFileContent filePath
  let distanceMatrix = decode fileContent :: Maybe [[Int]]
  case distanceMatrix of
    Nothing -> die $ "Error parsing JSON From file Path" ++ filePath
    Just matrix -> do
      let resultOfVerification = verifyDistanceMatrix matrix
      unless (fst resultOfVerification) $ die $ snd resultOfVerification
      return matrix

-- | Read file contents safely, with error handling.
-- Returns the file content as a ByteString or terminates with an error message.
--
-- @filePath@ Path to the file to read
getFileContent :: String -> IO BL.ByteString
getFileContent filePath =
  BL.readFile filePath `catch` \(e :: SomeException) ->
    die $ "Error reading file: " ++ filePath ++ "\nError: " ++ show e

-- | Verify that a distance matrix is valid.
-- Checks if the matrix is:
-- * non-empty
-- * square
-- * has valid distances (>= -1)
-- * represents an undirected graph (symmetric)
--
-- @param matrix@ The distance matrix to check
-- Returns a tuple with a boolean result and an error message if applicable.
verifyDistanceMatrix :: [[Int]] -> (Bool, String)
verifyDistanceMatrix matrix
  | null matrix = (False, "Distance matrix cannot be empty")
  | any (/= length matrix) (map length matrix) = (False, "Matrix must be square")
  | any (any (< (-1))) matrix = (False, "Matrix elements must be >= -1")
  | isNotUndirected matrix = (False, "Graph is not undirected")
  | otherwise = (True, "")
  where
    isNotUndirected matrixData = any (\(i, j) -> matrixData !! j !! i /= matrixData !! i !! j) [(i, j) | i <- [0 .. length matrixData - 1], j <- [0 .. length matrixData - 1]]

-- | Parse all input files required for the package delivery system.
-- Takes command line arguments and returns a tuple containing:
--
-- * A graph of airports derived from the distance matrix
-- * The package data for deliveries
-- * A fleet of airplanes created with the constraints
--
-- The function terminates with an error message if parsing fails or if arguments are invalid.
--
-- Expects three file paths as command line arguments:
-- 1. Distance matrix JSON file
-- 2. Package data JSON file
-- 3. Constraints JSON file
parseInputFiles :: IO (AirportNetwork, [PackageData], [Airplane])
parseInputFiles = do
  args <- getArgs
  if length args /= 3
    then die "Incorrect usage, expected: ./program distance_matrix.json package_data.json constraints.json"
    else do
      [distFile, packageFile, constrFile] <- return args
      distData <- parseDistanceMatrix distFile
      pkgData <- parsePackageData packageFile
      constrData <- parseConstraints constrFile
      let airportNetwork = buildAirportNetwork distData
      let airplanes = createMultipleAirplanes (weightCapacity constrData) (speed constrData) (numOfPlanes constrData)
      return (airportNetwork, pkgData, airplanes)
