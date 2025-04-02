module Main where

import JsonParser
import PrettyPrintSolution
import Solver

-- | The main entry point for the application. This function initializes the
main :: IO ()
main =
  do
    (airportGraph, pkgData, airplanes) <- parseInputFiles
    let initialSolution =
          Solution
            { bestDistance = 1 / 0, -- initial distance is infinity
              bestPlanes = [],      -- keeps the tab of what package assignment for each plane
              validCount = 0,
              nodesExplored = 0
            }

    let finalSolution = scheduleDeliveries pkgData airplanes 0.0 initialSolution airportGraph
    prettyPrintSolution finalSolution airportGraph
