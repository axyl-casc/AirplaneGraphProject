module AirplaneType
(
    Airplane,
    createAnAirplane
)
where

import PackageType (PackageData)
-- Airplane Abstract Data Type (ADT)

data Airplane = Airplane {
    airplaneCapacity  :: Int,
    airplanePackages  :: [PackageData],
    currentLoad :: Int,
    totalDistanceTraveled :: Double,
    returnTimeToOrigin :: Int,
    departureTimeFromOrigin :: Int,
    speed :: Int
} deriving (Show)

-- Creates a new airplane with given capacity and initial location (node ID)
createAnAirplane :: Int -> Int -> Airplane
createAnAirplane capacity airplaneSpeed = Airplane {
    airplaneCapacity = capacity,
    airplanePackages = [],
    currentLoad = 0,
    totalDistanceTraveled = 0,
    departureTimeFromOrigin = 0,
    returnTimeToOrigin = 0,
    speed = airplaneSpeed
}
{--
-- Attempt to load a package onto the airplane
-- Returns updated airplane and Bool indicating success
loadPackage :: (String, Int) -> Airplane -> (Airplane, Bool)
loadPackage (pkgId, pkgWeight) airplane
    | capacityAvailable >= pkgWeight = (airplane {
        airplanePackages = (pkgId, pkgWeight) : airplanePackages airplane
      }, True)
    | otherwise = (airplane, False)
  where
    currentLoad = sumPackagesOnPlane airplane
    capacityAvailable = airplaneCapacity airplane - currentLoad

-- Helper to sum total weight of all packages on airplane
sumPackagesOnPlane :: Airplane -> Int
sumPackagesOnPlane airplane = sum (map snd (airplanePackages airplane))
--}