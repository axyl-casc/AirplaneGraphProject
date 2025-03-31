module Main where
import JsonParser 
import Solver
import PrettyPrintSolution

-- | The main entry point for the application. This function initializes the
main :: IO ()

-- TO Do : figure out the package time problem
main = 
  do
    (airportGraph, pkgData, airplanes) <- parseInputFiles

    let initialSolution = Solution {
          bestDistance = 1/0,
          bestPlanes = [],
          validCount = 0,
          nodesExplored = 0
        }

    let finalSolution = scheduleDeliveries pkgData airplanes 0.0 initialSolution airportGraph
    prettyPrintSolution finalSolution airportGraph


