module Main where

import Trie (mkTrie, contains)
import Jahti (Table, generateWords)

table :: Table
table =
  [ "ielh"
  , "jvti"
  , "aäun"
  , "hkio"
  ]

wordList :: [String]
wordList =
  [ "evakuointi" , "tunika" , "lintu" , "leija" , "uinti" , "tuoni" , "lino"
  , "kuve" , "kuva" , "kuin" , "nuti" , "veli"]

main :: IO ()
main = mapM_ putStrLn words
  where
    words = generateWords table wordTrie
    wordTrie = mkTrie wordList
