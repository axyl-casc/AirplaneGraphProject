module Main where
import JsonParser 
import PackageType
import AirplaneType
import AirportGraph (buildAirportGraph)


main :: IO ()

-- TO Do : figure out the package time problem
main = 
    do 
    (distanceMatrix, constraintsData) <- parseInputFiles
    let airportGraph = buildAirportGraph distanceMatrix
    let airplanes =  [createAnAirplane (weightCapacity constraintsData)  (speed constraintsData)| _ <- [1..numOfPlanes constraintsData]]






    print airplanes                      