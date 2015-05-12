module Main where

import Trie (mkTrie)

main :: IO ()
main = print $ mkTrie wordList

wordList :: [String]
wordList =
  [ "tie"
  , "tieto"
  , "maku"
  , "makkara"
  ]
