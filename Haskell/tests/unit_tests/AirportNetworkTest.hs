module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import AirportNetwork
import Data.String (String)
import Control.Monad (forM_)
import Data.ByteString.Char8 (putStrLn)

type TestDescription = String
-- Description of the test, the input, and the verifier
testCasesForAirportNetworkBuilder :: [(TestDescription, [[Int]], AirportNetwork -> Bool)]
testCasesForAirportNetworkBuilder = [
    ( "Test Case 1: Fully connected 3-node graph"
    , [ [0, 10, 5]
      , [10, 0, 2]
      , [5, 2, 0]
      ]
    , \airportNetwork ->
        Map.size airportNetwork == 3 &&
        Just [0, 1] == getPathTo airportNetwork 0 1 &&
        Just [0, 2] == getPathTo airportNetwork 0 2
    ),

    ( "Test Case 2: Disconnected graph"
    , [ [0, 5, -1]
      , [5, 0, -1]
      , [-1, -1, 0]
      ]
    , \airportNetwork ->
        Map.size airportNetwork == 3 &&
        Just [0, 1] == getPathTo airportNetwork 0 1 &&
        isNothing (getPathTo airportNetwork 0 2) &&
        isNothing (getPathTo airportNetwork 1 2)
    ),

    ( "Test Case 3: 4-node graph with indirect paths"
    , [ [0, 10, -1, 50]
      , [10, 0, 15, -1]
      , [-1, 15, 0, 20]
      , [50, -1, 20, 0]
      ]
    , \airportNetwork ->
        Map.size airportNetwork == 4 &&
        Just [0, 1] == getPathTo airportNetwork 0 1 &&
        Just [0, 1, 2] == getPathTo airportNetwork 0 2 &&
        Just [0, 3] == getPathTo airportNetwork 0 3 &&
        Just [2, 3] == getPathTo airportNetwork 2 3
    ),

    ( "Test Case 4: Empty graph"
    , []
    , \airportNetwork -> Map.size airportNetwork == 0
    ),

    ( "Test Case 5: Single node graph"
    , [[0]]
    , \airportNetwork ->
        Map.size airportNetwork == 1 &&
        getPathTo airportNetwork 0 0 == Just [0]
    )
  ]



main :: IO ()
main = do
    print "Testing Airport Network Builder"
    forM_ testCasesForAirportNetworkBuilder $ \(testDesc, input, validatingFunction) -> do
        print $ "Test Case: " ++ testDesc
        if validatingFunction $ buildAirportNetwork input
            then print "Test Result: Passed"
            else print "Test Result: Failed"
