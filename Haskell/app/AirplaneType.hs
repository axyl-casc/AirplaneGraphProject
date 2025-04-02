-- |
-- Module      : AirplaneType
--
-- module for airplane type and related function
module AirplaneType
  ( Airplane,
    createAnAirplane,
    createMultipleAirplanes,
    getTotalCapacity,
    getSpeed,
    getCurrentLoad,
    getPackages,
    getDistanceTraveled,
    getReturnTimeToOrigin,
    getDepartureTimeFromOrigin,
    tryAddPackage,
  )
where

import AirportGraph (AirportNetwork, originAirPortID, getDistanceTo)
import Data.List (minimumBy)
import Data.Ord (comparing)
import PackageType

-- | Represents an airplane used for package delivery.
-- Contains information about the airplane's capacity, load, and current status.
--  === Fields:
--  * @totalCapacity@ - The total weight that the airplane can carry
--  * @packages@ - The list of packages that the airplane is to deliver, the order of the list is the delivery order as well
--  * @totalDistanceTraveled@ - Total distance that the airplane will travel to deliver the packages including the return trip
--  * @returnTimeToOrigin@ - The time in minutes when the airplane returns to the Origin Airport(1)
--  * @departureTimeFromOrigin@ - The time in minutes when the airplane departs from the Origin
--  * @speed@ - Speed in Km/h of the airplane
data Airplane = AirplaneInternal
  { totalCapacity :: Int,
    packages :: [PackageData],
    currentLoad :: Int,
    totalDistanceTraveled :: Double,
    returnTimeToOrigin :: Int,
    departureTimeFromOrigin :: Int,
    speed :: Int
  }
  deriving (Show)

-- | Creates a new airplane with the specified capacity and speed.
-- All other properties are initialized to default values. (0 or [])
--
-- @ Int@ Maximum weight capacity of the airplane
-- Int@ Speed of the airplane
--
-- @return Airplane@ A new airplane instance
createAnAirplane :: Int -> Int -> Airplane
createAnAirplane totalCp sp =
  AirplaneInternal
    { totalCapacity = totalCp,
      packages = [],
      currentLoad = 0,
      totalDistanceTraveled = 0,
      returnTimeToOrigin = 0,
      departureTimeFromOrigin = 0,
      speed = sp
    }

-- | Creates multiple airplane instances with the same capacity and speed.
--
-- @Int@ Weight capacity of each airplane
-- @Int@ Speed of each airplane
-- @Int@ Number of airplanes to create
--
-- @return [Airplane]@ List of identical airplanes
createMultipleAirplanes :: Int -> Int -> Int -> [Airplane]
createMultipleAirplanes totalCp sp numOfPlanes = replicate numOfPlanes (createAnAirplane totalCp sp)

---------------------
-- Getter Functions
---------------------

-- | Gets the total weight capacity of an airplane.
getTotalCapacity :: Airplane -> Int
getTotalCapacity = totalCapacity

-- | Gets the speed of an airplane.
getSpeed :: Airplane -> Int
getSpeed = speed

-- | Gets the current load weight of an airplane.
getCurrentLoad :: Airplane -> Int
getCurrentLoad = currentLoad

-- | Gets the list of packages currently loaded on an airplane.
getPackages :: Airplane -> [PackageData]
getPackages = packages

-- | Gets the total distance traveled by an airplane.
--
-- >>> getDistanceTraveled someAirplane
-- 500.0
getDistanceTraveled :: Airplane -> Double
getDistanceTraveled = totalDistanceTraveled

-- | Gets the time when the airplane is scheduled to return to origin.
getReturnTimeToOrigin :: Airplane -> Int
getReturnTimeToOrigin = returnTimeToOrigin

-- | Gets the time when the airplane departed from the origin.
getDepartureTimeFromOrigin :: Airplane -> Int
getDepartureTimeFromOrigin = departureTimeFromOrigin

-- | Attempts to add a package to an airplane.
-- Returns Nothing if package exceeds weight limit or no feasible route exists (i,e: some packages cant' be delivered in time).
-- If successful, package is added in optimal position to minimize distance.
--
-- @AirportNetwork@ List of airports representing the transportation network
-- @PackageData@ Package to be added
-- @Plane@ Airplane to add the package to
--
-- @return Maybe Airplane@ Updated airplane or Nothing if not possible
tryAddPackage :: AirportNetwork -> PackageData -> Airplane -> Maybe Airplane
tryAddPackage network pkg plane
  | getCurrentLoad plane + getWeightOfPackage pkg > getTotalCapacity plane = Nothing -- Basic wait check
  | null feasibleInsertions = Nothing -- it was not feasible
  | otherwise = Just updatedPlane -- it was feasible
  where
    earliestDeparture = max (getArrivalTimeOfPackage pkg) (getDepartureTimeFromOrigin plane)
    positions = [0 .. length (getPackages plane)]
    -- Map each possible insertion position to a tuple of (position, route feasibility evaluation)
    evaluated = map (\pos -> (pos, evaluateDeliveryRouteFeasibility (insertAt pos pkg (getPackages plane)) network earliestDeparture originAirPortID (getSpeed plane) 0)) positions
    feasibleInsertions = [(pos, dist) | (pos, Just dist) <- evaluated] -- remove all unfeasible positions
    (bestPos, bestDist) = minimumBy (comparing snd) feasibleInsertions -- finds the position with the smallest distance
    updatedPackages = insertAt bestPos pkg (getPackages plane)
    updatedLoad = getCurrentLoad plane + getWeightOfPackage pkg
    updatedReturn = earliestDeparture + round ((bestDist / fromIntegral (getSpeed plane)) * 60)

    updatedPlane =
      AirplaneInternal
        { totalCapacity = getTotalCapacity plane,
          packages = updatedPackages,
          currentLoad = updatedLoad,
          totalDistanceTraveled = bestDist,
          departureTimeFromOrigin = earliestDeparture,
          returnTimeToOrigin = updatedReturn,
          speed = getSpeed plane
        }

-- |
--  Evaluates the feasibility of a delivery route.
--  Checks if all packages can be delivered on time with return to origin. This is a helper function to tryAdd package so It should only be called by it.
--
--  @param [PackageData]@ List of packages to deliver
--  @param AirportNetwork@ Network of airports
--  @param Int@ Current time in minutes
--  @param Int@ Current location (airport index)
--  @param Int@ Airplane speed in distance units per hour
--  @param Double@ Cumulative distance traveled
--
--  @return Maybe Double@ Total distance if feasible, Nothing otherwise
evaluateDeliveryRouteFeasibility :: [PackageData] -> AirportNetwork -> Int -> Int -> Int -> Double -> Maybe Double
evaluateDeliveryRouteFeasibility [] network _ currLoc _ currDist -- Base case all packages have been delivered
  | currLoc == originAirPortID = Just currDist
  | otherwise = getDistanceTo network currLoc originAirPortID >>= \x -> Just (fromIntegral x + currDist) -- Add the distance from the last delivery location to origin
evaluateDeliveryRouteFeasibility (p : pkgs) network currTime currLoc airplaneSpeed currDist = do
  distToNewDest <- getDistanceTo network currLoc (getDestinationOfPackage p) -- getDistanceTo returns a maybe, so "<-" allows shortcircuting to nothing in case
  let travelTime = (fromIntegral distToNewDest / fromIntegral airplaneSpeed :: Double) * 60 -- Need to explicitly infer the type so GHc does not give a warning
  let arrivalTime = currTime + round travelTime

  if arrivalTime > getDeadlineTimeOfPackage p -- Check if deadline is met
    then Nothing -- Route is infeasible
    else
      evaluateDeliveryRouteFeasibility
        pkgs
        network
        arrivalTime
        (getDestinationOfPackage p) -- Update current location
        airplaneSpeed
        (currDist + fromIntegral distToNewDest) -- Add to accumulated distance

-- | Helper function to insert a value at a specified position in a list.
-- If the position exceeds the list length, the value is appended at the end.
--
-- @Int@ Position where the element should be inserted (0-based)
-- @a@ Value to insert
-- @[a]@ List to insert into
--
-- @return [a]@ New list with the inserted element
insertAt :: Int -> a -> [a] -> [a]
insertAt 0 x xs = x : xs
insertAt n x (y : ys) = y : insertAt (n - 1) x ys
insertAt _ x [] = [x]
