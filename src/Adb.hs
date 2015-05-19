module Adb (Coordinate, makeCmds, screenshot) where

import System.Cmd (system)
import Codec.Picture (readPng)
import Codec.Picture.Types (DynamicImage)

adb :: String
adb = "C:\\Users\\rce\\AppData\\Local\\Android\\sdk\\platform-tools\\adb.exe"

sendevent :: String
sendevent = adb ++ " shell sendevent /dev/input/event1"

type Coordinate = (Int, Int)

screenshot :: String -> IO (Either String DynamicImage)
screenshot filename = do
  mapM_ system cmds
  readPng filename where
      cmds =
        [ adb ++ " shell screencap -p /sdcard/" ++ filename
        , adb ++ " pull /sdcard/" ++ filename ++ " " ++ filename
        , adb ++ " shell rm /sdcard/" ++ filename ]

makeCmds :: [Coordinate] -> [String]
makeCmds coords = init ++ moves ++ end
  where
    init = [ sendevent ++ " 3 57 21860" ]
    moves = concat $ map moveCmd coords
    end = [ sendevent ++ " 3 57 -1"
          , sendevent ++ " 0 0 0" ]

moveCmd :: Coordinate -> [String]
moveCmd (x, y) =
  [ sendevent ++ " 3 53 " ++ (show x)
  , sendevent ++ " 3 54 " ++ (show y)
  , sendevent ++ " 0 0 0"
  ]
