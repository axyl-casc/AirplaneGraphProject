module PrettyPrintSolution
  ( prettyPrintSolution,
  )
where

import AirplaneType
import AirportGraph
import Data.Maybe (fromMaybe)
import PackageType
import Solver

prettyPrintSolution :: Solution -> [Airport] -> IO ()
prettyPrintSolution sol airports = do
  putStrLn "=== FINAL DELIVERY PLAN ==="
  putStrLn $ "Number of Valid solutions: " ++ show (validCount sol)
  putStrLn $ "Number of Nodes Explored: " ++ show (nodesExplored sol)
  putStrLn $ "Total Distance for the Optimal Solution: " ++ show (bestDistance sol) ++ " km \n"

  putStrLn "==========================="
  putStrLn "Airplane Assignments"
  putStrLn "==========================="

  -- Passing the list of Airplanes to printAirplanes
  mapM_ (printAirplaneWithIndex airports) (zip [0 ..] (bestPlanes sol))

-- Print each airplane with its index and other details
printAirplaneWithIndex :: [Airport] -> (Int, Airplane) -> IO ()
printAirplaneWithIndex airports (index, plane) = do
  putStrLn $ "~~~ AIRPLANE " ++ show index ++ " ~~~"
  putStrLn $ "Total Load: " ++ show (getCurrentLoad plane) ++ " kg"
  putStrLn $ "Total Distance Traveled: " ++ show (getDistanceTraveled plane) ++ " km"
  putStrLn $ "Departure Time: " ++ minutesToTimeStamp (getDepartureTimeFromOrigin plane) ++ "HH:MM\n"

  putStrLn "Route Details:"

  -- Initialize origin and current time with airplane's departure time
  let initialTime = getDepartureTimeFromOrigin plane
  let originAirport = 0 -- Starting at origin
  

  -- Print route details for each package while updating origin and time
  (finalAirport, finalTime) <- printRouteDetailsForPackages airports plane (getPackages plane) originAirport initialTime 1
  
  -- Print return trip details if not already at origin
  if finalAirport /= originAirport then do
    let returnDistance = fromMaybe 0 (getDistanceTo (fromMaybe (head airports) (findAirportById airports finalAirport)) originAirport)
    let returnTimeMinutes = if returnDistance > 0
                           then floor ((fromIntegral returnDistance :: Double) / (fromIntegral (getSpeed plane) :: Double) * 60)
                           else 0
    let returnArrivalTime = finalTime + returnTimeMinutes
    
    putStrLn $ "  Return Trip: Airport " ++ show finalAirport ++ " → Airport " ++ show originAirport
    putStrLn $ "    Distance: " ++ show returnDistance ++ " km"
    putStrLn $ "    Travel Time: " ++ minutesToTimeStamp returnTimeMinutes ++ "HH:MM"
    putStrLn $ "    Arrival Time at Origin: " ++ minutesToTimeStamp returnArrivalTime ++ "HH:MM"
  else
    putStrLn $ "  Already at Origin (Airport " ++ show originAirport ++ ")"
  
  putStrLn ""

-- Print the details of each leg in the route
printRouteDetails :: [Airport] -> PackageData -> Airplane -> Int -> Int -> Int -> IO (Int, Int)
printRouteDetails airports package plane currentAirport currentTime legNumber = do
  -- Get package details
  let destination = getDestinationOfPackage package
  let weight = getWeightOfPackage package
  let deadlineTime = getDeadlineTimeOfPackage package
  let packageId = getIdOfPackage package

  -- Check if package is already at its destination
  if currentAirport == destination then do
    putStrLn $ "  Leg " ++ show legNumber ++ ": Delivering Package " ++ show packageId
    putStrLn $ "    Already at destination (Airport " ++ show destination ++ ")"
    putStrLn $ "    Arrival Time: " ++ minutesToTimeStamp currentTime
    putStrLn $ "    Package Weight: " ++ show weight ++ " kg"
    putStrLn $ "    Deadline: " ++ minutesToTimeStamp deadlineTime
    
    -- Return unchanged airport and time
    return (destination, currentTime)
  else do
    -- Package needs to be delivered to a different location
    let currentAirportObj = fromMaybe (head airports) (findAirportById airports currentAirport)
    
    -- Get path and distance from current airport to destination
    let path = getPathTo currentAirportObj destination
    let distance = fromMaybe 0 (getDistanceTo currentAirportObj destination)

    -- Calculate travel time and update arrival time
    let travelTimeMinutes = if distance > 0
                           then floor ((fromIntegral distance :: Double) / (fromIntegral (getSpeed plane) :: Double) * 60)
                           else 0
    let arrivalTime = currentTime + travelTimeMinutes

    -- Print route details
    putStrLn $ "  Leg " ++ show legNumber ++ ": " ++ show currentAirport ++ " -> " ++ show destination
    putStrLn $ "    Distance: " ++ show distance ++ " km"
    putStrLn $ "    Travel Time: " ++ minutesToTimeStamp travelTimeMinutes
    putStrLn $ "    Departure: " ++ minutesToTimeStamp currentTime
    putStrLn $ "    Arrival: " ++ minutesToTimeStamp arrivalTime
    putStrLn $ "    Delivering Package " ++ show packageId ++ show weight ++ " kg"
    putStrLn $ "    Deadline: " ++ minutesToTimeStamp deadlineTime

    -- Return updated airport and time to process the next package
    return (destination, arrivalTime)

-- Print route details for all packages while updating time and origin
printRouteDetailsForPackages :: [Airport] -> Airplane -> [PackageData] -> Int -> Int -> Int -> IO (Int, Int)
printRouteDetailsForPackages _ _ [] currentAirport currentTime _ = 
  return (currentAirport, currentTime) -- No packages, return current state
printRouteDetailsForPackages airports plane (package : remainingPackages) currentAirport currentTime legNumber = do
  -- Print the current package details and get updated origin and time
  (newAirport, newTime) <- printRouteDetails airports package plane currentAirport currentTime legNumber
  -- Recur for the next package with updated state
  printRouteDetailsForPackages airports plane remainingPackages newAirport newTime (legNumber + 1)
  
-- Find the airport in the list by its ID
findAirportById :: [Airport] -> Int -> Maybe Airport
findAirportById [] _ = Nothing
findAirportById (a : as) searchId
  | (getAirportID a) == searchId = Just a
  | otherwise = findAirportById as searchId

minutesToTimeStamp :: Int -> String
minutesToTimeStamp minutes =
  let hours = minutes `div` 60
      mins = minutes `mod` 60
   in (if hours < 10 then "0" else "") ++ show hours ++ ":" ++ (if mins < 10 then "0" else "") ++ show mins