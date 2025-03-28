module Main where
import JsonParser 
import PackageType
import AirplaneType
import AirportGraph (buildAirportGraph)


main :: IO ()

-- TO Do : figure out the package time problem
main = 
    do 
    (distanceMatrix, pkgData, airplanes) <- parseInputFiles
    let firstPlane = head airplanes
    let newPlane = tryAddPackage firstPlane (head pkgData)



    print newPlane                     