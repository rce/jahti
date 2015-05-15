module Jahti (Table, generateWords) where

import Trie (Trie, contains)
import qualified Data.Set as Set

type Point = (Int, Int)
type Path = [Point]
type Table = [[Char]]

generateWords :: Table -> [String]
generateWords table = words
  where
    words = map (pathToWord table) validPaths
    validPaths = filter noDuplicates paths
    paths = concat $ map (getPaths table 10) startingPoints

pathToWord :: Table -> Path -> String
pathToWord table = map (charAt table)

-- How does this even work?
noDuplicates :: Path -> Bool
noDuplicates = f Set.empty where
  f _ [] = True
  f xs (h : t) = if Set.member h xs
    then False
    else f (Set.insert h xs) t

getPaths :: Table -> Int -> Point -> [Path]
getPaths table 1 point = [[point]]
getPaths table n point = [[point]] ++ map (point:) tails
  where
    c = charAt table point
    tails = concat $ map getTails next
    getTails = getPaths table (n-1)
    next = neighbors point

neighbors :: Point -> [Point]
neighbors (x, y) = filter (valid 4 4) all
  where all = [ (x-1, y-1), (x, y-1), (x+1, y-1)
              , (x-1, y),             (x+1, y)
              , (x-1, y+1), (x, y+1), (x+1, y+1)
              ]

valid :: Int -> Int -> Point -> Bool
valid w h (x, y)
  | x < 0 = False
  | y < 0 = False
  | x > 3 = False
  | y > 3 = False
  | otherwise = True

charAt :: Table -> Point -> Char
charAt table (x, y) = table !! y !! x

startingPoints :: [Point]
startingPoints = [ (x, y) | x <- [0..3], y <- [0..3] ]
