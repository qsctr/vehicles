module Vehicles.Three where

import Vehicles.Vehicle
import Vehicles.TwoWheeled
import Vehicles.Math

import Data.Tuple

vehicle3a :: Vehicle
vehicle3a =
    let decide = mapPair inverse
    in  makeVehicle sense decide act vehiclePicture

vehicle3b :: Vehicle
vehicle3b =
    let decide = swap . mapPair inverse
    in  makeVehicle sense decide act vehiclePicture