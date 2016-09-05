module Vehicles.Vehicles where

import Vehicles.Vehicle

import Vehicles.One
import Vehicles.Two
import Vehicles.Three

type VehicleName = String
type ErrorMessage = String

allVehicles :: [(VehicleName, Vehicle)]
allVehicles =
    [ ("1", vehicle1)
    , ("2a", vehicle2a)
    , ("2b", vehicle2b)
    , ("2c", vehicle2c)
    , ("3a", vehicle3a)
    , ("3b", vehicle3b) ]

lookupVehicle :: String -> Either ErrorMessage Vehicle
lookupVehicle name = case lookup name allVehicles of
    Just vehicle -> Right vehicle
    Nothing -> Left $ name ++ " is not a valid vehicle."