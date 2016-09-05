module Vehicles.One where

import Vehicles.Vehicle
import Vehicles.OneWheeled

vehicle1 :: Vehicle
vehicle1 =
    let decide = id
    in  makeVehicle sense decide act vehiclePicture