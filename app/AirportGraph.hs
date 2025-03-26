module AirportGraph
(
    Airport,
    PathInfo,
    buildAirportGraph,
    connections,
    shortestPaths
)
where

import qualified Data.Map as Map
import Data.Map (Map)

-- Airport Abstract Data Type
data Airport = Airport {
    airportID   :: Int,
    connections :: Map Int PathInfo
} deriving (Show)

-- Path Information Abstract Data Type
data PathInfo = PathInfo {
    path     :: [Int],
    distance :: Int
} deriving (Show)

-- Floyd-Warshall algorithm implementation to compute shortest paths
shortestPaths :: [[Int]] -> [[(Int, Maybe Int)]]
shortestPaths distanceMatrix = foldl update initialPaths [0..numNodes-1]
  where
    numNodes = length distanceMatrix
    initialPaths = [[(if distanceMatrix!!i!!j == -1 then maxBound else distanceMatrix!!i!!j, Nothing) 
                      | j <- [0..numNodes-1]] | i <- [0..numNodes-1]]

    update paths k =
        [[minimumDistance i j k paths | j <- [0..numNodes-1]] | i <- [0..numNodes-1]]

    minimumDistance i j k paths =
        let (distIJ, viaIJ) = paths!!i!!j
            (distIK, _) = paths!!i!!k
            (distKJ, _) = paths!!k!!j
            newDist = distIK + distKJ
        in if distIK /= maxBound && distKJ /= maxBound && distIJ > newDist
           then (newDist, Just k)
           else (distIJ, viaIJ)

-- Reconstruct path from Floyd-Warshall output
pathReconstruct :: Int -> Int -> [[(Int, Maybe Int)]] -> [Int]
pathReconstruct i j paths
  | i == j = [i]
  | otherwise = case snd (paths !! i !! j) of
                  Nothing -> [i, j]
                  Just k  -> pathReconstruct i k paths ++ tail (pathReconstruct k j paths)

-- Builds Airport network from distance matrix
buildAirportGraph :: [[Int]] -> [Airport]
buildAirportGraph distanceMatrix =
    let pathsMatrix = shortestPaths distanceMatrix
        numNodes = length distanceMatrix
    in [ Airport {
            airportID = i,
            connections = Map.fromList [
                (j, PathInfo (pathReconstruct i j pathsMatrix) (fst (pathsMatrix !! i !! j)))
                | j <- [0..numNodes-1], i /= j, (fst (pathsMatrix!!i!!j)) /= maxBound
            ]
        } | i <- [0..numNodes-1]]

-- Getter function for Airport connections
getConnections :: Airport -> Map Int PathInfo
getConnections = connections
