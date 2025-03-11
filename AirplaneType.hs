module AirplaneType
(
    Airplane,
    newAirplane,
    airplaneReady,
    airplaneLocation,
    airplaneCapacity,
    sumPackagesOnPlane,
    loadPackage
)
where

-- Airplane Abstract Data Type (ADT)
data Airplane = Airplane {
    airplaneReady     :: Bool,
    airplaneLocation  :: Int,
    airplaneCapacity  :: Int,
    airplanePackages  :: [(String, Int)] -- List of packages with (packageId, weight)
} deriving (Show, Eq)

-- Creates a new airplane with given capacity and initial location (node ID)
newAirplane :: Int -> Int -> Airplane
newAirplane capacity location = Airplane {
    airplaneCapacity = capacity,
    airplaneLocation = location,
    airplaneReady = True,
    airplanePackages = []
}

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
