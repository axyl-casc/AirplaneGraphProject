module Main where

import JsonParser (parseInputFiles)
import PackageType
import AirplaneType
import AirportGraph (Airport, buildAirportGraph)
import Solver (scheduleDeliveries, Solution(..))

main :: IO ()
main = do
    (airportGraph, pkgData, airplanes) <- parseInputFiles

    let initialSolution = Solution {
          bestDistance = 1/0,
          bestPlanes = [],
          validCount = 0,
          nodesExplored = 0
        }

    let finalSolution = scheduleDeliveries pkgData airplanes 0.0 initialSolution airportGraph

    putStrLn "=== FINAL DELIVERY PLAN ==="
    putStrLn $ "Total Nodes Explored: " ++ show (nodesExplored finalSolution)
    putStrLn $ "Valid Solutions Found: " ++ show (validCount finalSolution)
    putStrLn $ "Optimal Distance: " ++ show (bestDistance finalSolution)
    putStrLn $ "Airplane Assignments:"
    mapM_ print (bestPlanes finalSolution)
