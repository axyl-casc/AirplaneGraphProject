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
