-- |
-- Module      : Solver
--
-- This module implements the main recursive algorithm to assign packages
-- to airplanes while minimizing the total distance traveled. It explores
-- possible delivery routes and updates the best solution found.
module Solver
  ( scheduleDeliveries,
    Solution (..),
  )
where

import AirplaneType
import AirportNetwork (AirportNetwork)
import Data.Function (on)
import Data.List (delete, minimumBy)
import PackageType

-- |
--  Represents the state of the best solution found so far during the search.
--
--  === Fields:
--  * @bestDistance@ - The minimum distance traveled by all airplanes combined in the optimal solution.
--  * @bestPlanes@ - The list of airplanes with their assigned packages and updated states.
--  * @validCount@ - The number of valid solutions evaluated so far.
--  * @nodesExplored@ - The total number of nodes (states) explored in the search tree.
data Solution = Solution
  { bestDistance :: Double,
    bestPlanes :: [Airplane],
    validCount :: Int,
    nodesExplored :: Int
  }
  deriving (Show)

-- |
--  Main recursive scheduling function that assigns packages to airplanes.
--
--  * @[PackageData]@ - List of unassigned packages to be delivered.
--  * @[Airplane]@ - List of available airplanes with their current state.
--  * @Double@ - The total distance traveled so far by all airplanes or the total distance for the current solution
--  * @Solution@ - The best solution found so far.
--  * @AirportNetwork@ - THe network of airports.
--
--  === Base Case
--  - When all packages are assigned, the current solution is evaluated.
--  - If the total distance is lower than the best so far, the solution is updated.
--
--  === Backtracking
--  - If the current distance exceeds the best solution’s distance, backtracking occurs.
--
--  === Try Package Assignment
--  - Attempts to assign the next package to each airplane and explores possible solutions recursively.
scheduleDeliveries :: [PackageData] -> [Airplane] -> Double -> Solution -> AirportNetwork -> Solution
scheduleDeliveries [] airplanes distSoFar solution _ =
  -- Base case: all packages have been assigned
  if distSoFar < bestDistance solution  
    then
      solution
        { bestDistance = distSoFar,
          bestPlanes = airplanes,
          validCount = validCount solution + 1,
          nodesExplored = nodesExplored solution + 1
        }
    else
      solution
        { validCount = validCount solution + 1,
          nodesExplored = nodesExplored solution + 1
        }
scheduleDeliveries unassigned airplanes distSoFar solution network
  | distSoFar > bestDistance solution =
      solution {nodesExplored = nodesExplored solution + 1}       -- Backtrack if current distance exceeds the best solution
  | otherwise =
      foldl tryPlane updatedSolution (zip [0 ..] airplanes) -- Explore package assignment for each airplane
  where
    updatedSolution = solution {nodesExplored = nodesExplored solution + 1}
    pkg = selectNextPackage unassigned -- Select package with the earliest deadline
    rest = delete pkg unassigned

  -- |
  --  Tries to assign the selected package to a plane and explores further solutions. 
  --  THis is a inner helper function inside the where clause of schedule deliveries.
  --
  --  * @Solution@ - The current solution being explored.
  --  * @(Int, Airplane)@ - A tuple with the airplane’s index and the airplane itself.
  --
  --  === Package Assignment
  --  - If the package is successfully added to the airplane, the solution is updated.
  --  - Recursively explores the next state with the updated airplane and package list.
  --
  --  === Backtracking
  --  - If the package cannot be added, it backtracks to the previous state.
  --
  --  === Return Value
  --  - The best solution found after trying the package with all airplanes.
  --
    tryPlane :: Solution -> (Int, Airplane) -> Solution
    tryPlane sol (idx, plane) = 
      case tryAddPackage network pkg plane of        -- pkg is the in the where clause of the outer function
        Just updatedPlane ->
          let newPlanes = updatePlane idx updatedPlane airplanes 
              newDist = (distSoFar - getDistanceTraveled plane) + getDistanceTraveled updatedPlane
           in scheduleDeliveries rest newPlanes newDist sol network -- recursive call
        Nothing -> sol -- Backtrack if package cannot be assigned

-- |
--  Selects the next package to be delivered based on the earliest deadline.
--
--  * @[PackageData]@ - List of unassigned packages.
--
--  === Return Value
--  - @PackageData@ - The package with the earliest deadline.
selectNextPackage :: [PackageData] -> PackageData
selectNextPackage = minimumBy (compare `on` getDeadlineTimeOfPackage)

-- |
--  Replaces an airplane at a specific index with an updated version.
--
--  * @Int@ - The index of the airplane to be replaced.
--  * @Airplane@ - The updated airplane with the new package assignment.
--  * @[Airplane]@ - The list of airplanes.
--
--  === Return Value
--  - @[Airplane]@ - Updated list of airplanes.
updatePlane :: Int -> Airplane -> [Airplane] -> [Airplane]
updatePlane idx newPlane planes =
  take idx planes ++ [newPlane] ++ drop (idx + 1) planes
