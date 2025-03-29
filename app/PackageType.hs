-- | Module for package data typee and related functions.
-- 
module PackageType (
  -- * Data Type
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
) where


-- | Represents a package to be delivered.
-- 
data PackageData = PackageInternal
  { pkgId :: Int        -- ^ Unique identifier for the package
  , weight :: Int       -- ^ Weight of the package
  , arrivalTime :: Int  -- ^ Time of arrival at depot in minutes
  , deadlineTime :: Int -- ^ Deadline for delivery in minutes
  , destination :: Int  -- ^ Destination identifier
  } deriving (Show, Eq)

-- | Creates a new package with the specified properties.
--
-- @pid@ Unique identifier for the package
-- @w@ Weight of the package
-- @aTime@ Time when package arrives/becomes available (in minutes)
-- @dTime@ Deadline by which package must be delivered (in minutes)
-- @dest@ Destination identifier for the package
--
-- >>> createANewPackage 1 50 480 960 3
-- PackageInternal {pkgId = 1, weight = 50, arrivalTime = 480, deadlineTime = 960, destination = 3}
createANewPackage :: Int -> Int -> Int -> Int -> Int -> PackageData
createANewPackage pid w aTime dTime dest = PackageInternal {
  pkgId = pid,
  weight = w,
  arrivalTime = aTime,
  deadlineTime = dTime,
  destination = dest
}

-- | Creates a deep copy of a package.
-- This is used for creating a deep copy of an existing package
--
-- >>> let pkg = createANewPackage 1 50 480 960 3
-- >>> deepCopyPackage pkg
-- PackageInternal {pkgId = 1, weight = 50, arrivalTime = 480, deadlineTime = 960, destination = 3}
deepCopyPackage :: PackageData -> PackageData
deepCopyPackage  = id 

--------------------------
-- GETTER FUNCTIONS --
--------------------------

-- | Gets the unique identifier of the package.
--
-- >>> getIdOfPackage (createANewPackage 1 50 480 960 3)
-- 1
getIdOfPackage :: PackageData -> Int
getIdOfPackage = pkgId

-- | Gets the weight of the package in appropriate units.
--
-- >>> getWeightOfPackage (createANewPackage 1 50 480 960 3)
-- 50
getWeightOfPackage :: PackageData -> Int
getWeightOfPackage = weight

-- | Gets the arrival time of the package in minutes.
--
-- >>> getArrivalTimeOfPackage (createANewPackage 1 50 480 960 3)
-- 480
getArrivalTimeOfPackage :: PackageData -> Int
getArrivalTimeOfPackage = arrivalTime

-- | Gets the deadline time of the package in minutes.
--
-- >>> getDeadlineTimeOfPackage (createANewPackage 1 50 480 960 3)
-- 960
getDeadlineTimeOfPackage :: PackageData -> Int
getDeadlineTimeOfPackage = deadlineTime

-- | Gets the destination identifier of the package.
--
-- >>> getDestinationOfPackage (createANewPackage 1 50 480 960 3)
-- 3
getDestinationOfPackage :: PackageData -> Int
getDestinationOfPackage = destination
