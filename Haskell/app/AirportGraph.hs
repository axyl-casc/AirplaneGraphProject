-- | Module for creating and managing airport graph
-- Provides functionality for building airport network,
-- and retrieving distances and paths between airports.
module AirportGraph
  ( -- * Data Types
    AirportNetwork,
    Airport,
    buildAirportNetwork,
    getDistanceTo,
    getPathTo,
    originAirPortID

  )
where

import Data.Map (Map)
import qualified Data.Map as Map



originAirPortID :: Int
originAirPortID = 0
type AirportNetwork = Map Int Airport

-- | Represents an airport node in the transportation network.
data Airport = AirportInternal
  { -- | Unique identifier for the airport
    airportID :: Int,
    -- | Map of connections to other airports
    connections :: Map Int PathInfo
  } deriving (Show)





-- | Contains information about paths between airports.
data PathInfo = PathInfo
  { -- | Sequence of airport IDs forming the path
    path :: [Int],
    -- | Total distance of the path
    distance :: Int
  }
  deriving (Show)

-- | Implements the Floyd-Warshall algorithm to compute shortest paths.
-- Takes a distance matrix and returns a matrix of distances and intermediate nodes.
--
-- @distanceMatrix@ Matrix of distances between airports (-1 indicates no direct connection)
--
-- @Returns a matrix where each cell contains the shortest distance and the intermediate node (if any).
shortestPaths :: [[Int]] -> [[(Int, Maybe Int)]]
shortestPaths distanceMatrix = foldl update initialPaths [0 .. numNodes - 1]
  where
    numNodes = length distanceMatrix
    initialPaths =
      [ [ (if distanceMatrix !! i !! j == -1 then maxBound else distanceMatrix !! i !! j, Nothing)
        | j <- [0 .. numNodes - 1]
        ]
      | i <- [0 .. numNodes - 1]
      ]
    update paths k =
      [[minimumDistance i j k paths | j <- [0 .. numNodes - 1]] | i <- [0 .. numNodes - 1]]
    minimumDistance i j k paths =
      let (distIJ, viaIJ) = paths !! i !! j
          (distIK, _) = paths !! i !! k
          (distKJ, _) = paths !! k !! j
          newDist = distIK + distKJ
       in if distIK /= maxBound && distKJ /= maxBound && distIJ > newDist
            then (newDist, Just k)
            else (distIJ, viaIJ)


-- | Builds an airport network from a distance matrix
--
-- @param distanceMatrix@ Matrix of distances between airports (-1 indicates no connection)
--
-- @return AirportNetwork@ Complete network with computed paths
--
buildAirportNetwork :: [[Int]] -> AirportNetwork
buildAirportNetwork distanceMatrix =
  Map.fromList $ map (\a -> (airportID a, a)) $ buildAirports distanceMatrix





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
  | i == j = [i]
  | otherwise = case snd (paths !! i !! j) of
      Nothing -> [i, j]
      Just k -> pathReconstruct i k paths ++ tail (pathReconstruct k j paths)

-- | Helper function to build airports from a distance matrix
-- Computes shortest paths and creates Airport objects with connection information.
--
-- @distanceMatrix@ Matrix of distances between airports (-1 indicates no direct connection)
--
-- Returns a list of Airport objects representing the network.
buildAirports :: [[Int]] -> [Airport]
buildAirports distanceMatrix =
  let pathsMatrix = shortestPaths distanceMatrix
      numNodes = length distanceMatrix
   in [ AirportInternal
          { airportID = i,
            connections =
              Map.fromList
                [ (j, PathInfo (pathReconstruct i j pathsMatrix) (fst (pathsMatrix !! i !! j)))
                | j <- [0 .. numNodes - 1],
                  i /= j,
                  (fst (pathsMatrix !! i !! j)) /= maxBound
                ]
          }
      | i <- [0 .. numNodes - 1]
      ]

-- | Gets the distance to a specific destination airport.
--
-- @param airportNetwork@ The network of airports
-- @param sourceId@ Source airport ID
-- @param destinationId@ Destination airport ID
--
-- @return Maybe Int@ Distance if path exists, Nothing otherwise
getDistanceTo :: AirportNetwork -> Int -> Int -> Maybe Int
getDistanceTo airportNetwork sourceId destinationId
  | sourceId == destinationId = Just 0
  | otherwise = Map.lookup sourceId airportNetwork >>= \airport -> 
                Map.lookup destinationId (connections airport) >>= \pathInfo ->
                return (distance pathInfo)

-- | Gets the path to a specific destination airport from the source
--
-- @param airportNetwork@ The network of airports
-- @param sourceId@ Source airport ID
-- @param destinationId@ Destination airport ID
--
-- @return Maybe [Int]@ Path if it exists, Nothing otherwise
getPathTo :: AirportNetwork -> Int -> Int -> Maybe [Int]
getPathTo airportNetwork sourceId destinationId
  | sourceId == destinationId = Just [destinationId]
  | otherwise = Map.lookup sourceId airportNetwork >>= \airport -> 
                Map.lookup destinationId (connections airport) >>= \pathInfo ->
                return (path pathInfo)
