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
neighbors (x, y) = filter isValid all where
    isValid (x, y) = (0 <= x) && (x <= 3) && (0 <= y) && (y <= 3)
    all = [ (x-1, y-1), (x, y-1), (x+1, y-1)
          , (x-1, y  ),           (x+1, y  )
          , (x-1, y+1), (x, y+1), (x+1, y+1) ]

charAt :: Table -> Point -> Char
charAt table (x, y) = table !! y !! x
