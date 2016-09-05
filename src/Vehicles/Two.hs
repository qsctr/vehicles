module Vehicles.Two where

import Vehicles.Vehicle
import Vehicles.TwoWheeled

import Data.Tuple

vehicle2a :: Vehicle
vehicle2a =
    let decide = id
    in  makeVehicle sense decide act vehiclePicture

vehicle2b :: Vehicle
vehicle2b =
    let decide = swap
    in  makeVehicle sense decide act vehiclePicture

vehicle2c :: Vehicle
vehicle2c =
    let decide (l, r) = let avg = (l + r) / 2 in (avg, avg)
    in  makeVehicle sense decide act vehiclePicture