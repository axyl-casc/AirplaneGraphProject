module PrettyPrintSolution 
  (
    prettyPrintSolution
  ) 
where
import AirplaneType
import Solver
import AirportGraph
import PackageType
import Data.Maybe (fromMaybe)

prettyPrintSolution :: Solution -> [Airport] -> IO ()
prettyPrintSolution sol airports = do
  putStrLn "=== FINAL DELIVERY PLAN ==="
  putStrLn $ "Total Nodes Explored: " ++ show (nodesExplored sol)
  putStrLn $ "Valid Solutions Found: " ++ show (validCount sol)
  putStrLn $ "Optimal Distance: " ++ show (bestDistance sol) ++ "\n"
  putStrLn "==========================="
  putStrLn "Airplane Assignments:\n"
  -- Passing the list of Airplanes to printAirplanes
  mapM_ (printAirplaneWithIndex airports) (zip [0..] (bestPlanes sol))


-- Print each airplane with its index and other details
printAirplaneWithIndex :: [Airport] -> (Int, Airplane) -> IO ()
printAirplaneWithIndex airports (index, plane) = do
  putStrLn $ "~~~ AIRPLANE " ++ show index ++ " ~~~"
  putStrLn $ "Total Load: " ++ show (getCurrentLoad plane) ++ " kg"
  putStrLn $ "Total Distance Traveled: " ++ show (getDistanceTraveled plane)
  putStrLn $ "Departure Time: " ++ minutesToTimeStamp (getDepartureTimeFromOrigin plane)
  putStrLn $ "Return Time: " ++ minutesToTimeStamp (getReturnTimeToOrigin plane) ++ "\n"
  putStrLn "Route Details:"

  -- Initialize origin and current time with airplane's departure time
  let initialTime = getDepartureTimeFromOrigin plane
  let originAirport = 0  -- Starting at origin

  -- Print route details for each package while updating origin and time
  printRouteDetailsForPackages airports plane (getPackages plane) originAirport initialTime



-- Print the details of each leg in the route
printRouteDetails :: [Airport] -> PackageData -> Airplane -> Int -> Int -> IO (Int, Int)
printRouteDetails airports package plane currentAirport currentTime = do
  -- Get package details
  let destination = getDestinationOfPackage package
  let weight = getWeightOfPackage package
  let deadlineTime = getDeadlineTimeOfPackage package
  let packageId = getIdOfPackage package

  -- Get path and distance from current airport
  let path = getPathTo (fromMaybe (head airports) (findAirportById airports currentAirport)) destination
  let distance = fromMaybe 0 (getDistanceTo (fromMaybe (head airports) (findAirportById airports currentAirport)) destination)

  -- Calculate travel time and update arrival time
  let travelTimeMinutes = floor ((fromIntegral distance :: Double) / (fromIntegral (getSpeed plane) :: Double) * 60)
  let arrivalTime = currentTime + travelTimeMinutes
  let formattedArrivalTime = minutesToTimeStamp arrivalTime

  -- Print route details
  putStrLn $ "  Leg " ++ show (packageId + 1) ++ ": " ++ show path
  putStrLn $ "    Distance: " ++ show distance ++ " km"
  putStrLn $ "    Travel Time: " ++ minutesToTimeStamp travelTimeMinutes
  putStrLn $ "    Arrival Time: " ++ formattedArrivalTime
  putStrLn $ "    Delivering: Package " ++ show packageId ++ " (" ++ show weight ++ " kg)"
  putStrLn $ "    Deadline: " ++ minutesToTimeStamp deadlineTime ++ "\n"

  -- Return updated airport and time to process the next package
  return (destination, arrivalTime)

-- Print route details for all packages while updating time and origin
printRouteDetailsForPackages :: [Airport] -> Airplane -> [PackageData] -> Int -> Int -> IO ()
printRouteDetailsForPackages _ _ [] _ _ = return () -- No packages, stop recursion
printRouteDetailsForPackages airports plane (package:remainingPackages) currentAirport currentTime = do
  -- Print the current package details and get updated origin and time
  (newAirport, newTime) <- printRouteDetails airports package plane currentAirport currentTime
  -- Recur for the next package with updated state
  printRouteDetailsForPackages airports plane remainingPackages newAirport newTime

-- Find the airport in the list by its ID
findAirportById :: [Airport] -> Int -> Maybe Airport
findAirportById [] _ = Nothing
findAirportById (a:as) searchId
  | (getAirportID a) == searchId = Just a
  | otherwise = findAirportById as searchId

minutesToTimeStamp :: Int -> String
minutesToTimeStamp minutes = 
  let hours = minutes `div` 60
      mins  = minutes `mod` 60
  in  (if hours < 10 then "0" else "") ++ show hours ++ ":" ++ (if mins < 10 then "0" else "") ++ show mins




