module Trie where

import Data.List (sort, groupBy)

data Trie = Root [Trie]
          | Node Char [Trie]
          | End
          deriving Show

mkTrie :: [String] -> Trie
mkTrie = Root . (map mkNode) . groupWords

mkNode :: [String] -> Trie
mkNode [] = End
mkNode words = Node char $ end ++ (map mkNode $ groupWords tails)
  where
    char = (head . head) words
    end = if' isEnd [End] []
    isEnd = (length words) /= (length tails)
    tails = getTails words

getTails :: [String] -> [String]
getTails = (filter (/= "")) . (map tail)

groupWords :: [String] -> [[String]]
groupWords = (groupBy sameInitial) . sort

sameInitial :: String -> String ->  Bool
sameInitial a b = (head a) == (head b)

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y
