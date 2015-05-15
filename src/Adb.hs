module Adb (Coordinate, makeCmds) where

adb = "C:\\Users\\rce\\AppData\\Local\\Android\\sdk\\platform-tools\\adb.exe shell sendevent /dev/input/event1 "

type Coordinate = (Int, Int)

makeCmds :: [Coordinate] -> [String]
makeCmds coords = init ++ moves ++ end
  where
    init = [ adb ++ "3 57 21860" ]
    moves = concat $ zipWith moveCmd coords (tail coords)
    end = [ adb ++ "3 57 -1", adb ++ "0 0 0" ]

moveCmd :: Coordinate -> Coordinate -> [String]
moveCmd (x1, y1) (x2, y2) =
  [ adb ++ "3 53 " ++ (show x1)
  , adb ++ "3 54 " ++ (show y1)
  , adb ++ "0 0 0"
  , adb ++ "3 53 " ++ (show x2)
  , adb ++ "3 54 " ++ (show y2)
  , adb ++ "0 0 0"
  ]
