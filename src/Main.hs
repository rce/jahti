module Main where

import System.Environment (getArgs)
import Trie (Trie, mkTrie, contains)
import Jahti (Table, generateWords)

main :: IO ()
main = do
  args <- getArgs
  let table = parseTable (args !! 0)
  content <- readFile "words.txt"
  let trie = mkTrie (lines content)
  mapM_ putStrLn $ findRealWords table trie

parseTable :: String -> Table
parseTable [] = []
parseTable cs = [(take 4 cs)] ++ parseTable (drop 4 cs)

findRealWords :: Table -> Trie -> [String]
findRealWords table wordTrie = filter isValid $ generateWords table
  where isValid = \word -> contains word wordTrie
