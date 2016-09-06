{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Vehicles.Math where

import Constants

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

type Distance = Float
type Angle = Float
type SensorValue = Float
type MotorVelocity = Float

sensorRule :: Distance -> SensorValue
sensorRule x
    | x < mouseCircleRadius = (1 / x ^ 2) * 1e6 -- inverse square law
    | otherwise = 0

inverse :: SensorValue -> MotorVelocity
inverse 0 = 0
inverse x = 1e4 / x

distance :: Point -> Point -> Distance
distance (x1, y1) (x2, y2) = sqrt $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2

midpoint :: Point -> Point -> Point
midpoint (x1, y1) (x2, y2) = ((x1 + x2) / 2, (y1 + y2) / 2)

moveAtAngle :: Angle -> Distance -> Point -> Point
moveAtAngle angle d (x, y) = (x + cos' angle * d, y + sin' angle * d)

rotatePoint :: Angle -> Point -> Point
rotatePoint angle (x, y) =
    moveAtAngle (addAngles angle $ atan' $ y / x) (distance (0, 0) (x, y)) (0, 0)

addAngles :: Angle -> Angle -> Angle
addAngles a b =
    let wrap x
            | x > 360 = wrap $ x - 360
            | x < 0 = wrap $ x + 360
            | otherwise = x
    in  wrap $ a + b

addPoints :: Point -> Point -> Point
addPoints (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

subtractPoints :: Point -> Point -> Point
subtractPoints (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)

sin', cos', atan' :: Float -> Float
sin' = sin . degToRad
cos' = cos . degToRad
atan' = radToDeg . atan