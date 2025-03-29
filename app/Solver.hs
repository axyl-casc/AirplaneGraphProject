module Solver (
    scheduleDeliveries,
    Solution(..)  
) where

import PackageType
import AirplaneType
import AirportGraph

import qualified Data.Map as Map
import Data.List (delete, minimumBy)
import Data.Function (on)

-- | Represents the state of the best solution found so far.
data Solution = Solution {
    bestDistance :: Double,
    bestPlanes :: [Airplane],
    validCount :: Int,
    nodesExplored :: Int
} deriving (Show)

-- | Main recursive scheduling function.
scheduleDeliveries :: [PackageData] -> [Airplane] -> Double -> Solution -> [Airport] -> Solution
scheduleDeliveries [] airplanes distSoFar solution _ =
    if distSoFar < bestDistance solution
        then solution {
            bestDistance = distSoFar,
            bestPlanes = airplanes,
            validCount = validCount solution + 1,
            nodesExplored = nodesExplored solution + 1
        }
        else solution {
            validCount = validCount solution + 1,
            nodesExplored = nodesExplored solution + 1
        }
scheduleDeliveries unassigned airplanes distSoFar solution network
    | distSoFar > bestDistance solution = solution { nodesExplored = nodesExplored solution + 1 }
    | otherwise = foldl tryPlane solutionWithNodeCount (zip [0..] airplanes)
  where
    solutionWithNodeCount = solution { nodesExplored = nodesExplored solution + 1 }
    pkg = selectNextPackage unassigned
    rest = delete pkg unassigned


    tryPlane :: Solution -> (Int, Airplane) -> Solution
    tryPlane sol (idx, plane) =
        case tryAddPackage network pkg plane of
            Just updatedPlane ->
                let newPlanes = updatePlane idx updatedPlane airplanes
                    distDelta = estimateExtraDistance plane updatedPlane network
                    newDist = distSoFar + distDelta
                in scheduleDeliveries rest newPlanes newDist sol network
            Nothing -> sol


-- | Select the package with the earliest deadline.
selectNextPackage :: [PackageData] -> PackageData
selectNextPackage = minimumBy (compare `on` getDeadlineTimeOfPackage)

-- | Replaces a plane at a specific index in the airplane list.
updatePlane :: Int -> Airplane -> [Airplane] -> [Airplane]
updatePlane idx newPlane planes =
    take idx planes ++ [newPlane] ++ drop (idx + 1) planes

-- | Rough estimate of how much distance was added by inserting the package.
-- We assume it's the distance from the last package destination to the new one.
estimateExtraDistance :: Airplane -> Airplane -> [Airport] -> Double
estimateExtraDistance oldPlane newPlane network =
    let oldDest = lastOrOrigin (getPackages oldPlane)
        newDest = getDestinationOfPackage $ last (getPackages newPlane)
        sourceAirport = network !! oldDest
    in maybe 0 fromIntegral (getDistanceTo sourceAirport newDest)

-- | Helper: gets last destination or origin (0) if plane is empty.
lastOrOrigin :: [PackageData] -> Int
lastOrOrigin [] = 0
lastOrOrigin pkgs = getDestinationOfPackage (last pkgs)