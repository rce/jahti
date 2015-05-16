module Adb (Coordinate, makeCmds) where

adb :: String
adb = "C:\\Users\\rce\\AppData\\Local\\Android\\sdk\\platform-tools\\adb.exe shell sendevent /dev/input/event1 "

type Coordinate = (Int, Int)

makeCmds :: [Coordinate] -> [String]
makeCmds coords = init ++ moves ++ end
  where
    init = [ adb ++ "3 57 21860" ]
    moves = concat $ map moveCmd coords
    end = [ adb ++ "3 57 -1"
          , adb ++ "0 0 0"
          , "sleep 0.2" ]

moveCmd :: Coordinate -> [String]
moveCmd (x, y) =
  [ adb ++ "3 53 " ++ (show x)
  , adb ++ "3 54 " ++ (show y)
  , adb ++ "0 0 0"
  ]
