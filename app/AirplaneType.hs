-- | Module for airplane type and relate function
--
module AirplaneType
(
  -- * Data Type
  Airplane,
  -- * Constructor Functions
  createAnAirplane,
  createMultipleAirplanes,
  -- * Getter Functions
  getTotalCapacity,
  getSpeed,
  getCurrentLoad,
  getPackages,
  getDistanceTraveled,
  getReturnTimeToOrigin,
  getDepartureTimeFromOrigin,
  tryAddPackage
) where

import PackageType
import PackageType (getWeightOfPackage)

-- | Represents an airplane used for package delivery.
-- Contains information about the airplane's capacity, load, and current status.
data Airplane = AirplaneInternal {
  totalCapacity :: Int,            -- ^ Maximum weight capacity of the airplane
  packages :: [PackageData],       -- ^ List of packages currently loaded
  currentLoad :: Int,              -- ^ Current weight load of the airplane
  totalDistanceTraveled :: Double, -- ^ Total distance traveled by the airplane
  returnTimeToOrigin :: Int,       -- ^ Time when the airplane is scheduled to return to origin
  departureTimeFromOrigin :: Int,  -- ^ Time when the airplane departed from origin
  speed :: Int                     -- ^ Speed of the airplane
} deriving (Show)

--------------------------
-- Constructor Functions --
--------------------------

-- | Creates a new airplane with the specified capacity and speed.
-- All other properties are initialized to default values.
--
-- @capacity@ Maximum weight capacity of the airplane
-- @speed@ Speed of the airplane
--
-- >>> createAnAirplane 1000 100
-- AirplaneInternal {totalCapacity = 1000, packages = [], currentLoad = 0, totalDistanceTraveled = 0.0, returnTimeToOrigin = 0, departureTimeFromOrigin = 0, speed = 100}
createAnAirplane :: Int -> Int -> Airplane
createAnAirplane totalCp sp = AirplaneInternal {
  totalCapacity = totalCp,
  packages = [],
  currentLoad = 0,
  totalDistanceTraveled = 0,
  returnTimeToOrigin = 0,
  departureTimeFromOrigin = 0,
  speed = sp
}

-- | Creates multiple airplane instances with the same capacity and speed.
--
-- @capacity@ Weight capacity of each airplane
-- @speed@ Speed of each airplane
-- @numOfPlanes@ Number of airplanes to create
--
-- >>> createMultipleAirplanes 1000 100 3
-- [AirplaneInternal {...}, AirplaneInternal {...}, AirplaneInternal {...}]
createMultipleAirplanes :: Int -> Int -> Int -> [Airplane]
createMultipleAirplanes totalCp sp numOfPlanes = replicate numOfPlanes (createAnAirplane totalCp sp)

--------------------------
-- GETTER FUNCTIONS --
--------------------------

-- | Gets the total weight capacity of an airplane.
--
-- >>> getTotalCapacity someAirplane
-- 1000
getTotalCapacity :: Airplane -> Int
getTotalCapacity = totalCapacity

-- | Gets the speed of an airplane.
--
-- >>> getSpeed someAirplane
-- 100
getSpeed :: Airplane -> Int
getSpeed = speed

-- | Gets the current load weight of an airplane.
--
-- >>> getCurrentLoad someAirplane
-- 350
getCurrentLoad :: Airplane -> Int
getCurrentLoad = currentLoad

-- | Gets the list of packages currently loaded on an airplane.
--
-- >>> getPackages someAirplane
-- [PackageInternal {...}, PackageInternal {...}]
getPackages :: Airplane -> [PackageData]
getPackages = packages

-- | Gets the total distance traveled by an airplane.
--
-- >>> getDistanceTraveled someAirplane
-- 500.0
getDistanceTraveled :: Airplane -> Double
getDistanceTraveled = totalDistanceTraveled

-- | Gets the time when the airplane is scheduled to return to origin.
--
-- >>> getReturnTimeToOrigin someAirplane
-- 650
getReturnTimeToOrigin :: Airplane -> Int
getReturnTimeToOrigin = returnTimeToOrigin

-- | Gets the time when the airplane departed from the origin.
--
-- >>> getDepartureTimeFromOrigin someAirplane
-- 480
getDepartureTimeFromOrigin :: Airplane -> Int
getDepartureTimeFromOrigin = departureTimeFromOrigin

tryAddPackage :: Airplane -> PackageData -> Maybe Airplane
tryAddPackage plane pkgToAdd
  | getCurrentLoad plane + getWeightOfPackage pkgToAdd > getTotalCapacity plane = Nothing
  | otherwise = Just $ plane { 
      packages = packages plane ++ [pkgToAdd],
      currentLoad = getCurrentLoad plane + getWeightOfPackage pkgToAdd
    }

-- Add other getter or manipulator functions as required by your application...
-- For example, a function to add a package might look like:
-- addPackage :: PackageData -> Airplane -> Airplane
-- addPackage pkg plane@(AirplaneInternal {internalPackages = pkgs, internalCurrentLoad = load, internalCapacity = cap}) =
--   if load + packageWeight pkg <= cap -- Assuming PackageData has a way to get weight
--   then plane { internalPackages = pkg : pkgs, internalCurrentLoad = load + packageWeight pkg }
--   else plane -- Or handle error/overload case appropriately

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