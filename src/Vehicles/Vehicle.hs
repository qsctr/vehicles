{-# LANGUAGE NamedFieldPuns #-}

module Vehicles.Vehicle where

import Vehicles.Math

import Graphics.Gloss

type SourceLocation = Point
type SensorPosition = Point
type TimePassed = Float
type VehicleFunction = SourceLocation -> TimePassed -> VehicleState -> VehicleState

data Vehicle = Vehicle { function :: VehicleFunction, picture :: Picture, state :: VehicleState }
data VehicleState = VehicleState { location :: Point, angle :: Float }

makeVehicle
    :: (SourceLocation -> VehicleState -> sensorValues)               -- sense
    -> (sensorValues -> motorVelocities)                                -- decide
    -> (TimePassed -> VehicleState -> motorVelocities -> VehicleState)  -- act
    -> Picture
    -> Vehicle
makeVehicle sense decide act pic =
    let vFunc sourceLoc t vSt = limitAct sourceLoc t vSt $ decide $ sense sourceLoc vSt
        limitAct sourceLoc t vSt v =
            let newVSt = act t vSt v
            in  if distance (location newVSt) (location vSt) < 0.03
                    && distance sourceLoc (location vSt) < 58
                then vSt else newVSt
        defaultState = VehicleState { location = (0, 0), angle = 0 }
    in  Vehicle vFunc pic defaultState

applySensorRule :: SourceLocation -> VehicleState -> SensorPosition -> SensorValue
applySensorRule sourceLoc (VehicleState { location, angle }) =
    sensorRule . distance sourceLoc . addPoints location . rotatePoint angle