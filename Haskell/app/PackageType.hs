-- These pragmas are need to handle the parsing with the Aeson Library
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : PackageType
--
-- Module for package data typee and related functions.
module PackageType
  ( PackageData,
    createANewPackage,
    getIdOfPackage,
    getWeightOfPackage,
    getArrivalTimeOfPackage,
    getDeadlineTimeOfPackage,
    getDestinationOfPackage,
  )
where

import Control.Monad (unless)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Aeson.Types (Parser)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))

-- |
--  Represents a package to be delivered.
--
--  === Fields:
--  * @pkgId@ - Unique identifier for the package
--  * @arrivalTime@ - Time of arrival at depot in minutes
--  * @deadLineTime@ - Deadline for delivery in minutes
--  * @destination@ - Destination identifier
data PackageData = PackageInternal
  { pkgId :: Int,
    weight :: Int,
    arrivalTime :: Int,
    deadlineTime :: Int,
    destination :: Int
  }
  deriving (Show, Eq)

-- | Creates a new package with the specified properties.
--
-- @Int@ Unique identifier for the package
-- @Int@ Weight of the package
-- @Int@ Time when package arrives/becomes available (in minutes)
-- @Int@ Deadline by which package must be delivered (in minutes)
-- @Int@ Destination identifier for the package
--
-- @return PackageData@ New package  with specifications from the parameters
createANewPackage :: Int -> Int -> Int -> Int -> Int -> PackageData
createANewPackage pid w aTime dTime dest =
  PackageInternal
    { pkgId = pid,
      weight = w,
      arrivalTime = aTime,
      deadlineTime = dTime,
      destination = dest
    }

---------------------
-- Getter Functions
---------------------

-- | Gets the unique identifier of the package.
getIdOfPackage :: PackageData -> Int
getIdOfPackage = pkgId

-- | Gets the weight of the package in appropriate units.
getWeightOfPackage :: PackageData -> Int
getWeightOfPackage = weight

-- | Gets the arrival time of the package in minutes.
getArrivalTimeOfPackage :: PackageData -> Int
getArrivalTimeOfPackage = arrivalTime

-- | Gets the deadline time of the package in minutes.
getDeadlineTimeOfPackage :: PackageData -> Int
getDeadlineTimeOfPackage = deadlineTime

-- | Gets the destination identifier of the package.
getDestinationOfPackage :: PackageData -> Int
getDestinationOfPackage = destination

-- | JSON parser instance for 'PackageData'. Used by the "decode" function from the Aeson library to convert
--   the json object to haskell instance
-- Parses package information including ID, weight, arrival time, deadline, and destination.
instance FromJSON PackageData where
  parseJSON = withObject "Failed to Parse Package Data" $ \v -> do
    packageId <- v .: "id"
    packageWeight <- v .: "weight"
    packageArrivalTime <- v .: "arrivalTime" >>= parseTimeField
    packageDeadlineTime <- v .: "deadlineTime" >>= parseTimeField
    packageDestination <- v .: "destination"
    return $ createANewPackage packageId packageWeight packageArrivalTime packageDeadlineTime packageDestination

-- | Parse a time string in format "HH:MM" into minutes.
-- Validates time format and returns the result as minutes in a Parser context as needed by "FromJSON PackageData"
-- @String@ string to Parse
--
-- @return Int@ string converted to minutes in int
parseTimeField :: String -> Parser Int
parseTimeField timeStr = do
  unless (isTimeFormat timeStr) $ fail "Invalid time format for either arrival or deadlineTime in package data"
  case convertTimeOFDayToMinutes timeStr of
    Just minutes -> return minutes
    Nothing -> fail $ "Could not convert " ++ timeStr ++ " to minutes"

-- | Check if a string is in the expected time format HH:MM.
-- Hours can be any non-negative integer, minutes must be 00-59.
--
-- @String@ string to check
--
-- @return Bool@ True if the string is the expected format otherwise false
-- Reference: got the Regex from Chat GPT
isTimeFormat :: String -> Bool
isTimeFormat str = (str :: String) =~ ("^([0-9]+):[0-5][0-9]$" :: String)

-- | Convert a time string in format "HH:MM" to minutes past midnight.
-- Hours can be greater than 24 (e.g., "36:30").
-- Returns Nothing if the format is invalid or parsing fails.
--
-- @String@ The string to convert to minutes
--
-- @return Maybe Int@ the string in minutes or nothing if format is invalid or parsing fails
convertTimeOFDayToMinutes :: String -> Maybe Int
convertTimeOFDayToMinutes timeStr =
  case splitByDelim ":" timeStr of
    [hourStr, minuteStr] -> do
      hour <- readMaybe hourStr
      minute <- readMaybe minuteStr
      return (hour * 60 + minute)
    _ -> Nothing

-- | Splits a string based on a delimiter.
--
-- @String@ The delimiter to split on
-- @String@ The string to be split--
--
-- @return [string] list of string split based on the delimiter
splitByDelim :: String -> String -> [String]
splitByDelim = splitOn
