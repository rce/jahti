module Main where

import Trie (Trie, mkTrie, contains)
import Jahti (Table, generateWords)

table :: Table
table =
  [ "ielh"
  , "jvti"
  , "aäun"
  , "hkio"
  ]

main :: IO ()
main = do
  content <- readFile "words.txt"
  let trie = mkTrie (lines content)
  mapM_ putStrLn $ findRealWords table trie

findRealWords :: Table -> Trie -> [String]
findRealWords table wordTrie = filter isValid $ generateWords table
  where isValid = \word -> contains word wordTrie
