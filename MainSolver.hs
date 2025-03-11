module Main where

import FileParser
import AirportGraph
import System.Exit (die)

main :: IO ()
main = do
    -- Replace this with actual file parsing if needed
    let distanceMatrix = [
            [0, 2, -1, 5],
            [2, 0, 3, -1],
            [-1, 3, 0, 2],
            [5, -1, 2, 0]
            ]

    -- Verify the distance matrix
    case verifyDistanceMatrix distanceMatrix of
        Left err -> die err
        Right _ -> return ()

    -- Build airport network
    let airports = buildAirportGraph distanceMatrix

    -- Print airport network to the console
    mapM_ print airports