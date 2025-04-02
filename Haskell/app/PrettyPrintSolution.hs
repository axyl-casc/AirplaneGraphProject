{-# LANGUAGE BlockArguments #-} -- makes nested do's cleaner

-- |
-- Module      : PrettyPrintSolution
--
-- This module provides functions for pretty-printing the final delivery plan,
module PrettyPrintSolution
  ( prettyPrintSolution,
  )
where

import AirplaneType
import AirportNetwork
import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import PackageType
  ( PackageData,
    getDeadlineTimeOfPackage,
    getDestinationOfPackage,
    getIdOfPackage,
    getWeightOfPackage,
  )
import Solver

-- |
--  Prints the final delivery plan and assignments.
--
--  @Solution@ Solution with airplane and package assignments
--  @AirportNetwork@ Network of Airports
--
--  @return IO ()@ No return value, prints to console
prettyPrintSolution :: Solution -> AirportNetwork -> IO ()
prettyPrintSolution sol network = do
  putStrLn "=== DELIVERY SOLUTION ==="
  putStrLn $ "Number of Valid solutions: " ++ show (validCount sol)
  putStrLn $ "Number of Nodes Explored: " ++ show (nodesExplored sol)
  if validCount sol == 0
    then putStrLn $ "No Solution Found"
    else do
      putStrLn $ "Total Distance for the Optimal Solution: " ++ show (bestDistance sol) ++ " km \n"
      putStrLn "==========================="
      putStrLn "Airplane Assignments"
      putStrLn "==========================="

      -- Print details for each airplane with index
      mapM_ (printAirplaneWithIndex network) (zip [0 ..] (bestPlanes sol))

-- |
--  Prints details for an airplane including load, distance, and route(package delivery Order).
--
--  @param AirportNetwork@ Network of Airport
--  @param (Int, Airplane)@ Airplane index and the airplane itself
--
--  @return IO ()@ No return value, prints to console
printAirplaneWithIndex :: AirportNetwork -> (Int, Airplane) -> IO ()
printAirplaneWithIndex network (index, plane) = do
  putStrLn $ "~~~ AIRPLANE " ++ show index ++ " ~~~"
  putStrLn $ "Total Load: " ++ show (getCurrentLoad plane) ++ " kg"
  putStrLn $ "Total Distance Traveled: " ++ show (getDistanceTraveled plane) ++ " km"

  unless (null (getPackages plane)) do
    putStrLn $ "Departure Time: " ++ minutesToTimeStamp (getDepartureTimeFromOrigin plane) ++ "(HH:MM)"
    putStrLn "Route Details:\n"
    -- Initialize origin and departure time
    let initialTime = getDepartureTimeFromOrigin plane

    -- Print route details and return state after delivery
    (finalAirport, finalTime) <- printRouteDetailsForPackages network plane (getPackages plane) originAirPortID initialTime 1
    let returnDistance = fromMaybe 0 (getDistanceTo network finalAirport originAirPortID)
    let returnTimeMinutes = floor ((fromIntegral returnDistance :: Double) / (fromIntegral (getSpeed plane) :: Double) * 60)
    let returnArrivalTime = finalTime + returnTimeMinutes
    putStrLn $ "  Return To Origin: Airport " ++ intListToArrowString (fromMaybe [] (getPathTo network finalAirport originAirPortID))
    putStrLn $ "    Distance: " ++ show returnDistance ++ " km"
    putStrLn $ "    Travel Time: " ++ minutesToTimeStamp returnTimeMinutes ++ "(HH:MM)"
    putStrLn $ "    Arrival Time at Origin: " ++ minutesToTimeStamp returnArrivalTime ++ "(HH:MM)"
    putStrLn ""

-- |
--  Prints route details for a package delivery.
--
--  @param AirportNetwork@ Network of Airports
--  @param PackageData@ Package being delivered
--  @param Airplane@ Airplane carrying the package
--  @param Int@ Current airport location
--  @param Int@ Current time in minutes
--  @param Int@ Leg number in the route
--
--  @return IO (Int, Int)@ Prints to the console and returns new airport location and updated time
printRouteDetails :: AirportNetwork -> PackageData -> Airplane -> Int -> Int -> Int -> IO (Int, Int)
printRouteDetails network package plane currentAirportID currentTime legNumber = do
  let destination = getDestinationOfPackage package
  let weight = getWeightOfPackage package
  let deadlineTime = getDeadlineTimeOfPackage package
  let packageId = getIdOfPackage package
  let path = fromMaybe [] $ getPathTo network currentAirportID destination
  let distance = fromMaybe 0 $ getDistanceTo network currentAirportID destination
  let travelTimeMinutes =
        if distance > 0
          then round ((fromIntegral distance :: Double) / (fromIntegral (getSpeed plane) :: Double) * 60)
          else 0
  let arrivalTime = currentTime + travelTimeMinutes

  putStrLn $ "Leg " ++ show legNumber ++ ": " ++ intListToArrowString path
  putStrLn $ "    Distance: " ++ show distance ++ " km"
  putStrLn $ "    Travel Time: " ++ minutesToTimeStamp travelTimeMinutes ++ " (HH:MM)"
  putStrLn $ "    Arrival Time: " ++ minutesToTimeStamp arrivalTime ++ " (HH:MM)"
  putStrLn $ "    Delivering Package " ++ show packageId ++ " (" ++ show weight ++ " kg)"
  putStrLn $ "    Deadline " ++ minutesToTimeStamp deadlineTime ++ " (HH:MM)"

  return (destination, arrivalTime)

-- |
--  Prints route details for all packages assigned to an airplane.
--  Returns final location and time after all deliveries.
--
--  @param AirportNetwork@ Network of Airport
--  @param Airplane@ Airplane transporting packages
--  @param [PackageData]@ Packages to be delivered
--  @param Int@ Current airport location
--  @param Int@ Current time in minutes
--  @param Int@ Starting leg number
--
--  @return IO (Int, Int)@ Final airport and arrival time
printRouteDetailsForPackages :: AirportNetwork -> Airplane -> [PackageData] -> Int -> Int -> Int -> IO (Int, Int)
printRouteDetailsForPackages _ _ [] currentAirport currentTime _ =
  return (currentAirport, currentTime)
printRouteDetailsForPackages network plane (package : remainingPackages) currentAirport currentTime legNumber = do
  (newAirport, newTime) <- printRouteDetails network package plane currentAirport currentTime legNumber
  printRouteDetailsForPackages network plane remainingPackages newAirport newTime (legNumber + 1)

-- |
--  Converts minutes to a timestamp format (HH:MM).
--
--  @param Int@ Total minutes
--
--  @return String@ Formatted time string
minutesToTimeStamp :: Int -> String
minutesToTimeStamp minutes =
  let hours = minutes `div` 60
      mins = minutes `mod` 60
   in (if hours < 10 then "0" else "") ++ show hours ++ ":" ++ (if mins < 10 then "0" else "") ++ show mins

-- |
--  Converts a list of integers to an arrow-separated string.
--  Example: [1,2,3] becomes "1 -> 2 -> 3"
--
--  @param [Int]@ List to convert
--
--  @return String@ Formatted string
intListToArrowString :: [Int] -> String
intListToArrowString [] = ""
intListToArrowString [x] = show x
intListToArrowString (x : xs) = show x ++ " -> " ++ intListToArrowString xs
