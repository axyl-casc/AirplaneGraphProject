module Solver
  ( scheduleDeliveries,
    Solution (..),
  )
where

import AirplaneType
import AirportGraph
import Data.Function (on)
import Data.List (delete, minimumBy)
import qualified Data.Map as Map
import PackageType

-- | Represents the state of the best solution found so far.
data Solution = Solution
  { bestDistance :: Double,
    bestPlanes :: [Airplane],
    validCount :: Int,
    nodesExplored :: Int
  }
  deriving (Show)

-- | Main recursive scheduling function.
scheduleDeliveries :: [PackageData] -> [Airplane] -> Double -> Solution -> [Airport] -> Solution
scheduleDeliveries [] airplanes distSoFar solution _ =
  -- Base case if all packages have been assigned
  if distSoFar < bestDistance solution -- Only replace the beset solution if the current one is better
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
  | distSoFar > bestDistance solution = solution {nodesExplored = nodesExplored solution + 1} -- Backtrack if the current solution exceeds the best total
  | otherwise = foldl tryPlane solutionWithNodeCount (zip [0 ..] airplanes) -- Whats going in here?
  where
    solutionWithNodeCount = solution {nodesExplored = nodesExplored solution + 1}
    pkg = selectNextPackage unassigned
    rest = delete pkg unassigned

    tryPlane :: Solution -> (Int, Airplane) -> Solution -- Tries to assign the selected package to the plane
    tryPlane sol (idx, plane) =
      case tryAddPackage network pkg plane of
        Just updatedPlane ->
          let newPlanes = updatePlane idx updatedPlane airplanes
              newDist = (distSoFar - getDistanceTraveled plane) + getDistanceTraveled updatedPlane
           in scheduleDeliveries rest newPlanes newDist sol network
        Nothing -> sol -- Backtracks if tryAddPackage fails

-- | Select the package with the earliest deadline.
selectNextPackage :: [PackageData] -> PackageData
selectNextPackage = minimumBy (compare `on` getDeadlineTimeOfPackage)

-- | Replaces a plane at a specific index in the airplane list.
updatePlane :: Int -> Airplane -> [Airplane] -> [Airplane]
updatePlane idx newPlane planes =
  take idx planes ++ [newPlane] ++ drop (idx + 1) planes
