module Main where

import System.Cmd (system)
import System.Environment (getArgs)
import Jahti (Table, Path, findPaths, pathToWord)
import Adb (makeCmds, Coordinate)
import Data.List (null, nubBy)

main :: IO ()
main = do
  args <- getArgs
  let table = parseTable (args !! 0)
  wordList <- fmap lines (readFile "words.txt")
  let paths = getPaths table wordList
  putStrLn "Found words:"
  mapM_ putStrLn $ map (pathToWord table) paths
  putStr "Inputting words... "
  mapM_ inputPath paths
  putStrLn "Done!"

getPaths :: Table -> [String] -> [Path]
getPaths table = (removeDuplicates table) . firstElements . (map (findPaths table))

firstElements :: [[a]] -> [a]
firstElements = (map head) . (filter (not . null))

parseTable :: String -> Table
parseTable [] = []
parseTable cs = [(take 4 cs)] ++ parseTable (drop 4 cs)

removeDuplicates :: Table -> [Path] -> [Path]
removeDuplicates table = nubBy (sameWords table)
  where 
    sameWords table a b = (toWord a) == (toWord b)
    toWord = pathToWord table

inputPath :: Path -> IO ()
inputPath = (mapM_ system) . makeCmds . pathToCoords

pathToCoords :: Path -> [Coordinate]
pathToCoords = map toCoord
  where toCoord (x, y) = (159 + x * 254, 753 + y * 287)
