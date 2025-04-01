module Main where

import AirportGraph

testCasesForAirportNetworkBuilder :: [([[Int]], AirportNetwork -> Bool)]
testCasesForAirportGraphBuilder =
  [ ( -- Test Case 1: Basic connectivity
      [ [0, 10, 5]
      , [10, 0, 2]
      , [5, 2, 0]
      ]
    , \airportNetwork ->
        Map.size airportNetwork  == 3
          && getDistanceTo (airports !! 0) 1 == Just 7
          && getDistanceTo (airports !! 0) 2 == Just 5
          && getPathTo (airports !! 0) 1 == [0, 2, 1]
          && getDistanceTo (airports !! 1) 0 == Just 7
          && getDistanceTo (airports !! 1) 2 == Just 2
          && getDistanceTo (airports !! 2) 0 == Just 5
          && getDistanceTo (airports !! 2) 1 == Just 2
    )
  , ( -- Test Case 2: Disconnected graph
      [ [0, 5, -1]
      , [5, 0, -1]
      , [-1, -1, 0]
      ]
    , \airports ->
        length airports == 3
          && getDistanceTo (airports !! 0) 1 == Just 5
          && getDistanceTo (airports !! 0) 2 == Nothing
          && getDistanceTo (airports !! 1) 2 == Nothing
          && getDistanceTo (airports !! 2) 0 == Nothing
    )
  , ( -- Test Case 3: 4-node graph with indirect paths
      [ [0, 10, -1, 50]
      , [10, 0, 15, -1]
      , [-1, 15, 0, 20]
      , [50, -1, 20, 0]
      ]
    , \airports ->
        length airports == 4
          && getDistanceTo (airports !! 0) 2 == Just 25
          && getPathTo (airports !! 0) 2 == [0, 1, 2]
          && getDistanceTo (airports !! 0) 3 == Just 45
          && getPathTo (airports !! 0) 3 == [0, 1, 2, 3]
    ),
    ( -- Test Case 4: Empty graph
      []
    , \airports ->
        length airports == 0
    )
  ]

main :: IO ()
main = do
  putStrLn "Testing Airport Graph Builder"
  let results = map (\(input, expected) -> expected (buildAirportGraph input)) testCasesForAirportGraphBuilder
      allPassed = all id results
  if allPassed
    then do
      putStrLn "All tests passed!"
    else do
      putStrLn "Some tests failed:"
      mapM_ (\(i, passed) ->
        putStrLn $ "  Test " ++ show (i:: Int) ++ ": " ++ if passed then "Passed" else "Failed")
        (zip [1..] results)

