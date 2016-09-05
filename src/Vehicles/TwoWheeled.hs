{-# LANGUAGE NamedFieldPuns #-}

module Vehicles.TwoWheeled where

import Vehicles.Vehicle
import Vehicles.Parts
import Vehicles.Math

import Graphics.Gloss

vehiclePicture :: Picture
vehiclePicture = pictures
    [ rectangleWire width 40
    , uncurry translate lWheelPosition wheel
    , uncurry translate rWheelPosition wheel
    , translate (width / 2) sensorPositionsY sensor
    , translate (width / 2) (-sensorPositionsY) sensor ]

width, sensorPositionsX, sensorPositionsY :: Float
width = 60
sensorPositionsX = width / 2 + sensorOffset
sensorPositionsY = 12

sensorPositions :: (SensorPosition, SensorPosition)
sensorPositions = ((sensorPositionsX, sensorPositionsY), (sensorPositionsX, -sensorPositionsY))

wheelPositionsX, wheelPositionsY :: Float
wheelPositionsX = -24
wheelPositionsY = 26

lWheelPosition, rWheelPosition :: Point
lWheelPosition = (wheelPositionsX, wheelPositionsY)
rWheelPosition = (wheelPositionsX, -wheelPositionsY)

sense :: SourceLocation -> VehicleState -> (SensorValue, SensorValue)
sense sourceLoc vSt = mapPair (applySensorRule sourceLoc vSt) sensorPositions

act :: TimePassed -> VehicleState -> (MotorVelocity, MotorVelocity) -> VehicleState
act t (VehicleState { location, angle }) (vl, vr) =
    let (dl, dr) = (vl * t, vr * t)
        moveWheel d wheelPos = moveAtAngle angle d $ addPoints location $ rotatePoint angle wheelPos
        (newLeftLoc, newRightLoc) = (moveWheel dl lWheelPosition, moveWheel dr rWheelPosition)
        newLocation = subtractPoints (midpoint newLeftLoc newRightLoc) $
            rotatePoint newAngle (wheelPositionsX, 0)
        newAngle = addAngles angle $ atan' $ (dr - dl) / (wheelPositionsY * 2)
    in  VehicleState { location = newLocation, angle = newAngle }