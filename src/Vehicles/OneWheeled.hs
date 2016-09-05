{-# LANGUAGE NamedFieldPuns #-}

module Vehicles.OneWheeled where

import Vehicles.Vehicle
import Vehicles.Parts
import Vehicles.Math

import Graphics.Gloss

vehiclePicture :: Picture
vehiclePicture = pictures
    [ rectangleWire width 20
    , translate (-30) 0 wheel
    , translate (width / 2) sensorPositionY sensor ]

width, sensorPositionY :: Float
width = 60
sensorPositionY = 0

sensorPosition :: SensorPosition
sensorPosition = (width / 2 + sensorOffset, sensorPositionY)

sense :: SourceLocation -> VehicleState -> SensorValue
sense sourceLoc vSt = applySensorRule sourceLoc vSt sensorPosition

act :: TimePassed -> VehicleState -> MotorVelocity -> VehicleState
act t (VehicleState { location, angle }) v =
    VehicleState { location = moveAtAngle angle (v * t) location, angle }
