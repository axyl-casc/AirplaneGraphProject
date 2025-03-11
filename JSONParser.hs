module JSONParser
(
    parseSimpleJSON
) where

import System.IO
import Data.Char (isSpace)
import Data.List (dropWhileEnd)

-- Parses a simple JSON file into key-value pairs
parseSimpleJSON :: FilePath -> IO [(String, String)]
parseSimpleJSON file = do
    content <- readFile file
    return $ parsePairs (lines content)

-- Parses lines into key-value pairs
parsePairs :: [String] -> [(String, String)]
parsePairs = map parsePair . filter (':' `elem`)

-- Parses a single JSON key-value pair
parsePair :: String -> (String, String)
parsePair line =
    let (k, rest) = break (== ':') line
        key = trim $ filter (/= '"') k
        val = trim $ filter (`notElem` ",\"") (drop 1 rest)
    in (key, val)

-- Helper to trim whitespace
trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace