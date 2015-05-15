module Main where

import System.Cmd (system)
import System.Environment (getArgs)
import Trie (Trie, mkTrie, contains)
import Jahti (Table, Path, Point, generateWords)
import Adb (makeCmds, Coordinate)

main :: IO ()
main = do
  args <- getArgs
  let table = parseTable (args !! 0)
  content <- readFile "words.txt"
  let trie = mkTrie (lines content)
  mapM_ handleWord $ findRealWords table trie

handleWord :: (Path, String) -> IO ()
handleWord (p, word) = do
  let c = pathToCoords p
  mapM_ system $ makeCmds c

parseTable :: String -> Table
parseTable [] = []
parseTable cs = [(take 4 cs)] ++ parseTable (drop 4 cs)

findRealWords :: Table -> Trie -> [(Path, String)]
findRealWords table wordTrie = filter isValid $ generateWords table
  where isValid = \(_, word) -> contains word wordTrie

pathToCoords :: Path -> [Coordinate]
pathToCoords path = map pointToCoords path

pointToCoords :: Point -> Coordinate
pointToCoords (x, y) = (193 + x * 231, 807 + y * 231)
