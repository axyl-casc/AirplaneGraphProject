-- | Module for creating and managing airport graph
-- Provides functionality for building airport network,
-- and retrieving distances and paths between airports.
module AirportGraph
(
  -- * Data Types
  Airport,
  -- * Graph Construction
  buildAirportGraph,
  -- * Graph Operations
  getDistanceTo,
  getPathTo
)
where

import qualified Data.Map as Map
import Data.Map (Map)


-- | Represents an airport node in the transportation network.
-- 
data Airport = AirportInternal {
  airportID :: Int,                -- ^ Unique identifier for the airport
  connections :: Map Int PathInfo  -- ^ Map of connections to other airports
} deriving (Show)


-- | Contains information about paths between airports.
-- 
data PathInfo = PathInfo {
  path :: [Int],    -- ^ Sequence of airport IDs forming the path
  distance :: Int   -- ^ Total distance of the path
} deriving (Show)



-- | Implements the Floyd-Warshall algorithm to compute shortest paths.
-- Takes a distance matrix and returns a matrix of distances and intermediate nodes.
--
-- @distanceMatrix@ Matrix of distances between airports (-1 indicates no direct connection)
--
-- @Returns a matrix where each cell contains the shortest distance and the intermediate node (if any).
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


-- | Reconstruct the shortest path between two airports.
-- Uses the output from the Floyd-Warshall algorithm to trace the path.
--
-- @i@ Source airport ID
-- @j@ Destination airport ID
-- @paths@ Matrix of shortest distances and intermediate nodes
--
-- Returns a list of airport IDs representing the path.
pathReconstruct :: Int -> Int -> [[(Int, Maybe Int)]] -> [Int]
pathReconstruct i j paths
  | i == j    = [i]
  | otherwise = case snd (paths !! i !! j) of
      Nothing -> [i, j]
      Just k  -> pathReconstruct i k paths ++ tail (pathReconstruct k j paths)



-- | Builds an airport network from a distance matrix.
-- Computes shortest paths and creates Airport objects with connection information.
--
-- @distanceMatrix@ Matrix of distances between airports (-1 indicates no direct connection)
--
-- Returns a list of Airport objects representing the network.
buildAirportGraph :: [[Int]] -> [Airport]
buildAirportGraph distanceMatrix =
  let pathsMatrix = shortestPaths distanceMatrix
      numNodes = length distanceMatrix
  in [ AirportInternal {
      airportID = i,
      connections = Map.fromList [
        (j, PathInfo (pathReconstruct i j pathsMatrix) (fst (pathsMatrix !! i !! j)))
        | j <- [0..numNodes-1], i /= j, (fst (pathsMatrix!!i!!j)) /= maxBound
      ]
    } | i <- [0..numNodes-1]]



-- | Gets the distance to a specific destination airport.
-- Returns -1 if there is no path to the destination.
--
-- @airport@ Source airport
-- @to@ Destination airport ID
--
-- >>> getDistanceTo sourceAirport 5
-- 320
getDistanceTo :: Airport -> Int -> Maybe Int
getDistanceTo (AirportInternal {connections = con}) to =  fmap distance (Map.lookup to con)

-- | Gets the path to a specific destination airport.
-- Returns an empty list if there is no path to the destination.
--
-- @airport@ Source airport
-- @to@ Destination airport ID
--
-- >>> getPathTo sourceAirport 5
-- [0, 2, 5]
getPathTo :: Airport -> Int -> [Int]
getPathTo (AirportInternal {connections = con}) to = maybe [] path (Map.lookup to con)