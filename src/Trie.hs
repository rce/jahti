module Trie (Trie, mkTrie, contains) where

import Data.List (find, sort, groupBy)

data Trie = Root [Trie]
          | Node Char [Trie]
          | End
          deriving Show

contains :: String -> Trie -> Bool
contains word (Root nodes) = contains' word nodes
contains [] (Node _ nodes) = any isEnd nodes
contains word (Node _ nodes) = contains' word nodes

contains' :: String -> [Trie] -> Bool
contains' (h:t) nodes = case findSubtrie h nodes of
  Just(node) -> contains t node
  Nothing -> False

findSubtrie :: Char -> [Trie] -> Maybe Trie
findSubtrie c = (find match) . (filter isNode)
  where match = \(Node val _) -> val == c

isNode :: Trie -> Bool
isNode (Node _ _) = True
isNode _ = False

isEnd :: Trie -> Bool
isEnd End = True
isEnd _ = False

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
