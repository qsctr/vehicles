module Vehicles.Parts where

import Graphics.Gloss

wheel :: Picture
wheel = rectangleWire 24 12

sensor :: Picture
sensor = pictures
    [ line [(0, 0), (sensorLineLength, 0)]
    , translate sensorOffset 0 $ arc 90 270 sensorRadius ]

sensorLineLength, sensorRadius, sensorOffset :: Float
sensorLineLength = 20
sensorRadius = 6
sensorOffset = sensorLineLength + sensorRadius