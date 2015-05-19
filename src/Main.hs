module Main where

import Codec.Picture (Image, DynamicImage (ImageRGBA8), pixelAt)
import Codec.Picture.Types (PixelRGBA8 (PixelRGBA8))
import Control.Concurrent (threadDelay)
import Data.List (null, length, sortBy, nubBy)
import Data.Ord (comparing)
import System.Cmd (system)
import System.Directory (renameFile)
import System.Environment (getArgs)

import Adb (makeCmds, Coordinate, screenshot)
import Characters (pairs)
import Jahti (Table, Path, findPaths, pathToWord)

main :: IO ()
main = do
  screen <- screenshot "tmp.png"
  case screen of
    Left(err) -> putStrLn err
    Right(image) -> runBot (toImage image)

{--
-- Print characters for debugging
main :: IO ()
main = do
  args <- getArgs
  screen <- readPng (args !! 0)
  case screen of
    Left(err) -> putStrLn err
    Right(image) -> printGrid (toImage image)

printGrid :: Image PixelRGBA8 -> IO ()
printGrid image = mapM_ (printAscii image) grid where
  grid = [ (x, y) | y <- [0..3], x <- [0..3] ]

printAscii :: Image PixelRGBA8 -> (Int, Int) -> IO ()
printAscii image pos = do
  putStrLn "-----"
  mapM_ print $ groupN 20 $ c pos
  putStrLn "-----"
  where c = (render image) . charLocation
--}

runBot :: Image PixelRGBA8 -> IO ()
runBot image = do
  let grid = readGrid image
  -- Store the image and result for debugging
  renameFile "tmp.png" (grid ++ ".png")
  putStrLn $ "Read table from screen: " ++ grid
  threadDelay (sec 1.5)
  let table = parseTable grid
  wordList <- fmap lines (readFile "words.txt")
  let paths = sortPaths $ getPaths table wordList
  mapM_ inputPath paths

toImage :: DynamicImage -> Image PixelRGBA8
toImage image = case image of
  ImageRGBA8(image) -> image

readGrid :: Image PixelRGBA8 -> [Char]
readGrid image = map c grid where
  grid = [ (x, y) | y <- [0..3], x <- [0..3] ]
  c = (readChar image) . charLocation

readChar :: Image PixelRGBA8 -> Coordinate -> Char
readChar image coord = bestMatch $ map (commonArea rendered) pairs
  where bestMatch = fst . head . reverse . (sortBy (comparing snd))
        rendered = render image coord

commonArea :: [Char] -> (Char, [Char]) -> (Char, Int)
commonArea a (c, b) = (c, common) where
  common = length $ filter eq (zip a b )
  eq = \(a, b) -> a == b

render :: Image PixelRGBA8 -> Coordinate -> [Char]
render image coord = toString pixels where
  toString = (map (choose '*' ' ')) . (map isBlue)
  pixels = map reader refPoints
  reader = offsetReader image coord

offsetReader :: Image PixelRGBA8 -> (Int, Int) -> (Int, Int) -> PixelRGBA8
offsetReader img (ox, oy) =
  \(x, y) -> pixelAt img (ox + x) (oy + y)

choose :: a -> a -> Bool -> a
choose x _ True = x
choose _ y False = y

isBlue :: PixelRGBA8 -> Bool
isBlue (PixelRGBA8 r g b _)
  | b > 200 && r < 200 && g < 200 = True
  | otherwise = False

-- 20x25 grid of reference points for detecting characters in the grid.
-- The math is pretty much black magic.
refPoints :: [Coordinate]
refPoints = [ (60 + 6 * x, 45 + 6 * y) | y <- [0..24], x <- [0..19] ]

sortPaths :: [Path] -> [Path]
sortPaths = reverse . (sortBy (comparing length))

getPaths :: Table -> [String] -> [Path]
getPaths table = (removeDuplicates table) . firstElements . (map (findPaths table))

removeDuplicates :: Table -> [Path] -> [Path]
removeDuplicates table = nubBy (sameWords table) where
  sameWords table a b = (pathToWord table a) == (pathToWord table b)

firstElements :: [[a]] -> [a]
firstElements = (map head) . (filter (not . null))

parseTable :: String -> Table
parseTable = groupN 4

groupN :: Int -> [a] -> [[a]]
groupN _ [] = []
groupN n cs = [(take n cs)] ++ groupN n (drop n cs)

inputPath :: Path -> IO ()
inputPath path = do
  input path
  threadDelay (sec 0.02)
  where input = (mapM_ system) . makeCmds . pathToCoords

pathToCoords :: Path -> [Coordinate]
pathToCoords = map toCoord where
  toCoord (x, y) = (159 + x * 254, 753 + y * 287)

charLocation :: (Int, Int) -> Coordinate
charLocation (x, y) = (41 + x * 254, 615 + y * 287)

sec :: Float -> Int
sec = round . (*1000000)
