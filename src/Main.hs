module Main where

import Graphics.Gloss

main :: IO ()
main = display (InWindow "Vehicles" (800, 600) (0, 0)) white vehicle

vehicle :: Picture
vehicle = scale 2 2 $ pictures
    [ rectangleWire 30 20
    , translate (-12) (-13) $ wheel
    , translate (-12) 13 $ wheel
    , translate 0 6 $ sensor
    , translate 0 (-6) $ sensor ]
    where wheel = rectangleWire 12 6
          sensor = translate 15 0 $ pictures
              [ line [(0, 0), (10, 0)]
              , translate 13 0 $ arc 90 270 3 ]