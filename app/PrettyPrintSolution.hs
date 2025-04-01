{-|
Module      : PrettyPrintSolution

This module provides functions for pretty-printing the final delivery plan,
including airplane assignments, route details, and package deliveries.
-}
module PrettyPrintSolution
  ( prettyPrintSolution,
  )
where

import AirplaneType
import AirportGraph
import Data.Maybe (fromMaybe)
import PackageType
import Solver

{-|
  Prints the final delivery plan and airplane assignments.

  @prettyPrintSolution sol airports@ takes the following parameters:

  * @Solution@ - The final solution with details about airplanes, packages, and routes.
  * @[Airport]@ - A list of available airports to identify locations and connections.

  === Output
  - Prints information about:
    - Number of valid solutions and nodes explored.
    - Total distance for the optimal solution.
    - Details of each airplane’s route and package deliveries.
-}
prettyPrintSolution :: Solution -> [Airport] -> IO ()
prettyPrintSolution sol airports = do
  putStrLn "=== FINAL DELIVERY PLAN ==="
  putStrLn $ "Number of Valid solutions: " ++ show (validCount sol)
  putStrLn $ "Number of Nodes Explored: " ++ show (nodesExplored sol)
  putStrLn $ "Total Distance for the Optimal Solution: " ++ show (bestDistance sol) ++ " km \n"

  putStrLn "==========================="
  putStrLn "Airplane Assignments"
  putStrLn "==========================="

  -- Print details for each airplane with index
  mapM_ (printAirplaneWithIndex airports) (zip [0 ..] (bestPlanes sol))

{-|
  Prints details for each airplane, including total load, distance, and departure time.

  @printAirplaneWithIndex airports (index, plane)@ takes the following parameters:

  * @[Airport]@ - The list of available airports.
  * @(Int, Airplane)@ - A tuple containing the index of the airplane and the airplane itself.

  === Output
  - Prints the airplane’s current load, distance traveled, and departure time.
  - Displays route details for all packages assigned to the airplane.
  - Returns to the origin if not already there.
-}
printAirplaneWithIndex :: [Airport] -> (Int, Airplane) -> IO ()
printAirplaneWithIndex airports (index, plane) = do
  putStrLn $ "~~~ AIRPLANE " ++ show index ++ " ~~~"
  putStrLn $ "Total Load: " ++ show (getCurrentLoad plane) ++ " kg"
  putStrLn $ "Total Distance Traveled: " ++ show (getDistanceTraveled plane) ++ " km"
  putStrLn $ "Departure Time: " ++ minutesToTimeStamp (getDepartureTimeFromOrigin plane) ++ "HH:MM\n"

  putStrLn "Route Details:"
  
  -- Initialize origin and departure time
  let initialTime = getDepartureTimeFromOrigin plane
  let originAirport = 0 -- Start from the origin

  -- Print route details and return state after delivery
  (finalAirport, finalTime) <- printRouteDetailsForPackages airports plane (getPackages plane) originAirport initialTime 1

  -- Check if return trip to origin is needed
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

{-|
  Prints route details for delivering a package and returns updated state.

  @printRouteDetails airports package plane currentAirport currentTime legNumber@ takes the following parameters:

  * @[Airport]@ - List of airports to search for connections.
  * @PackageData@ - The package being delivered.
  * @Airplane@ - The airplane carrying the package.
  * @Int@ - The current airport where the airplane is located.
  * @Int@ - The current time (in minutes since departure).
  * @Int@ - The leg number of the delivery.

  === Return Value
  - @(Int, Int)@ - A tuple containing the new airport location and updated time.

  === Output
  - Prints delivery details including distance, travel time, and arrival time.
-}
printRouteDetails :: [Airport] -> PackageData -> Airplane -> Int -> Int -> Int -> IO (Int, Int)
printRouteDetails airports package plane currentAirport currentTime legNumber = do
  let destination = getDestinationOfPackage package
  let weight = getWeightOfPackage package
  let deadlineTime = getDeadlineTimeOfPackage package
  let packageId = getIdOfPackage package

  if currentAirport == destination then do
    putStrLn $ "  Leg " ++ show legNumber ++ ": Delivering Package " ++ show packageId
    putStrLn $ "    Already at destination (Airport " ++ show destination ++ ")"
    putStrLn $ "    Arrival Time: " ++ minutesToTimeStamp currentTime
    putStrLn $ "    Package Weight: " ++ show weight ++ " kg"
    putStrLn $ "    Deadline: " ++ minutesToTimeStamp deadlineTime
    return (destination, currentTime)
  else do
    let currentAirportObj = fromMaybe (head airports) (findAirportById airports currentAirport)
    let path = getPathTo currentAirportObj destination
    let distance = fromMaybe 0 (getDistanceTo currentAirportObj destination)
    let travelTimeMinutes = if distance > 0
                             then floor ((fromIntegral distance :: Double) / (fromIntegral (getSpeed plane) :: Double) * 60)
                             else 0
    let arrivalTime = currentTime + travelTimeMinutes

    putStrLn $ "  Leg " ++ show legNumber ++ ": " ++ show currentAirport ++ " -> " ++ show destination
    putStrLn $ "    Distance: " ++ show distance ++ " km"
    putStrLn $ "    Travel Time: " ++ minutesToTimeStamp travelTimeMinutes
    putStrLn $ "    Departure: " ++ minutesToTimeStamp currentTime
    putStrLn $ "    Arrival: " ++ minutesToTimeStamp arrivalTime
    putStrLn $ "    Delivering Package " ++ show packageId ++ " (" ++ show weight ++ " kg)"
    putStrLn $ "    Deadline: " ++ minutesToTimeStamp deadlineTime

    return (destination, arrivalTime)

{-|
  Prints route details for all packages assigned to an airplane.

  @printRouteDetailsForPackages airports plane packages currentAirport currentTime legNumber@ takes the following parameters:

  * @[Airport]@ - List of airports for connection lookup.
  * @Airplane@ - The airplane transporting packages.
  * @[PackageData]@ - A list of packages to be delivered.
  * @Int@ - The current airport location.
  * @Int@ - The current time in minutes.
  * @Int@ - The current leg number.

  === Return Value
  - @(Int, Int)@ - The final airport and arrival time after all packages are delivered.

  === Output
  - Prints package delivery details for each leg.
-}
printRouteDetailsForPackages :: [Airport] -> Airplane -> [PackageData] -> Int -> Int -> Int -> IO (Int, Int)
printRouteDetailsForPackages _ _ [] currentAirport currentTime _ = 
  return (currentAirport, currentTime)
printRouteDetailsForPackages airports plane (package : remainingPackages) currentAirport currentTime legNumber = do
  (newAirport, newTime) <- printRouteDetails airports package plane currentAirport currentTime legNumber
  printRouteDetailsForPackages airports plane remainingPackages newAirport newTime (legNumber + 1)

{-|
  Finds an airport by its ID from a list of airports.

  @findAirportById airports searchId@ takes the following parameters:

  * @[Airport]@ - List of available airports.
  * @Int@ - The ID of the airport to search for.

  === Return Value
  - @Maybe Airport@ - Returns the airport if found, otherwise `Nothing`.
-}
findAirportById :: [Airport] -> Int -> Maybe Airport
findAirportById [] _ = Nothing
findAirportById (a : as) searchId
  | (getAirportID a) == searchId = Just a
  | otherwise = findAirportById as searchId

{-|
  Converts minutes to a timestamp format (HH:MM).

  @minutesToTimeStamp minutes@ takes the following parameter:

  * @Int@ - The total number of minutes.

  === Return Value
  - @String@ - A formatted time string (HH:MM).
-}
minutesToTimeStamp :: Int -> String
minutesToTimeStamp minutes =
  let hours = minutes `div` 60
      mins = minutes `mod` 60
   in (if hours < 10 then "0" else "") ++ show hours ++ ":" ++ (if mins < 10 then "0" else "") ++ show mins
