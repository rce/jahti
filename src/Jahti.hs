module Jahti (Table, Path, findPaths, pathToWord) where

import Data.List (concat, (\\))

type Table = [[Char]]
type Path = [Point]
type Point = (Int, Int)

-- Checks if given table contains a string
findPaths :: Table -> String -> [Path]
findPaths table (c:t) = concat $ map (findPaths' table t) starts
  where
    starts = map (:[]) $ filter (\p -> (charAt table p) == c) startingPoints
    startingPoints = [ (x, y) | x <- [0..3], y <- [0..3] ]

findPaths' :: Table -> String -> [Point] -> [Path]
findPaths' _ [] path = [path]
findPaths' table (c:t) path = concat $ map (findPaths' table t) nextPaths
  where
    nextPaths = map (\p -> path ++ [p]) goodNeighbors
    goodNeighbors = filter ((c==) . (charAt table)) notVisited
    notVisited = (neighbors (last path)) \\ path


pathToWord :: Table -> Path -> String
pathToWord table = map (charAt table)

neighbors :: Point -> [Point]
neighbors (x, y) = filter valid all
  where all = [ (x-1, y-1), (x, y-1), (x+1, y-1)
              , (x-1, y  ),           (x+1, y  )
              , (x-1, y+1), (x, y+1), (x+1, y+1)
              ]

valid :: Point -> Bool
valid (x, y)
  | x < 0 = False
  | y < 0 = False
  | x > 3 = False
  | y > 3 = False
  | otherwise = True

charAt :: Table -> Point -> Char
charAt table (x, y) = table !! y !! x
