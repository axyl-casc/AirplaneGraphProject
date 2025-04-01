{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module for package data typee and related functions.
module PackageType
  ( -- * Data Type
    PackageData,

    -- * Constructor Functions
    createANewPackage,
    deepCopyPackage,

    -- * Getter Functions
    getIdOfPackage,
    getWeightOfPackage,
    getArrivalTimeOfPackage,
    getDeadlineTimeOfPackage,
    getDestinationOfPackage,
  )
where

import Control.Monad ( unless )
import Data.Aeson ( FromJSON(parseJSON), (.:), withObject )
import Data.Aeson.Types ( Parser )
import Data.List.Split ( splitOn )
import Text.Read ( readMaybe )
import Text.Regex.TDFA ( (=~) )

-- | Represents a package to be delivered.
data PackageData = PackageInternal
  { -- | Unique identifier for the package
    pkgId :: Int,
    -- | Weight of the package
    weight :: Int,
    -- | Time of arrival at depot in minutes
    arrivalTime :: Int,
    -- | Deadline for delivery in minutes
    deadlineTime :: Int,
    -- | Destination identifier
    destination :: Int
  }
  deriving (Show, Eq)

-- | Creates a new package with the specified properties.
--
-- @pid@ Unique identifier for the package
-- @w@ Weight of the package
-- @aTime@ Time when package arrives/becomes available (in minutes)
-- @dTime@ Deadline by which package must be delivered (in minutes)
-- @dest@ Destination identifier for the package
createANewPackage :: Int -> Int -> Int -> Int -> Int -> PackageData
createANewPackage pid w aTime dTime dest =
  PackageInternal
    { pkgId = pid,
      weight = w,
      arrivalTime = aTime,
      deadlineTime = dTime,
      destination = dest
    }

-- | Creates a deep copy of a package.
-- This is used for creating a deep copy of an existing package
deepCopyPackage :: PackageData -> PackageData
deepCopyPackage = id

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

-- | JSON parser instance for 'PackageData'.
-- Parses package information including ID, weight, arrival time, deadline, and destination.
instance FromJSON PackageData where
  parseJSON = withObject "Failed to Parse Package Data" $ \v -> do
    packageId <- v .: "id"
    packageWeight <- v .: "weight"
    packageArrivalTime <- v .: "arrivalTime" >>= parseTimeField
    packageDeadlineTime <- v .: "deadlineTime" >>= parseTimeField
    packageDestination <- v .: "destination"
    return $ createANewPackage packageId packageWeight packageArrivalTime packageDeadlineTime packageDestination

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

-- | Check if a string is in the expected time format HH:MM.
-- Hours can be any non-negative integer, minutes must be 00-59.
--
-- @param str@ string to check
-- Reference: got the Regex from Chat GPT
isTimeFormat :: String -> Bool
isTimeFormat str = (str :: String) =~ ("^([0-9]+):[0-5][0-9]$" :: String)

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
--
-- @param delim@ The delimiter to split on
-- @param str@ The string to be split--
-- >>> splitByDelim ":" "14:30"
-- ["14","30"]
splitByDelim :: String -> String -> [String]
splitByDelim = splitOn
