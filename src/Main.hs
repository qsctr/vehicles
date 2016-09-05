{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Constants
import Vehicles.Vehicle
import Vehicles.Vehicles

import Graphics.Gloss.Interface.Pure.Game
import System.Random

data State = State { vehicles :: [Vehicle], mouse :: Maybe Point }

main :: IO ()
main = do
    mapM_ putStrLn
        [ "Enter a vehicle."
        , "You can also enter multiple vehicles separated by spaces."
        , "The vehicles can be the same types."
        , "Available vehicle types are: " ++ unwords (map fst allVehicles) ]
    input <- words <$> getLine
    case mapM lookupVehicle input of
        Left err -> putStrLn err >> main
        Right vehicles -> do
            let (w, h) = (800, 600)
            initialState <- do
                let randomize vehicle = do
                        let randomAround x = randomRIO (- x / 2, x / 2)
                        randX <- randomAround $ fromIntegral w - 100
                        randY <- randomAround $ fromIntegral h - 100
                        let location = (randX, randY)
                        angle <- randomRIO (0, 359)
                        return $ vehicle { state = VehicleState { location, angle } }
                randomVehicles <- mapM randomize vehicles
                return $ State { vehicles = randomVehicles, mouse = Nothing }
            mapM_ putStrLn 
                [ "Instructions:"
                , "Space bar: toggle mouse control"
                , "Press enter to continue." ]
            _ <- getLine
            play (InWindow "Vehicles" (w, h) (0, 0)) white 60 initialState draw handleInput update

draw :: State -> Picture
draw (State { vehicles, mouse }) =
    let mousePicture = case mouse of
            Just (x, y) -> translate x y $ circle mouseCircleRadius
            Nothing -> blank
        drawVehicle (Vehicle { picture, state = VehicleState { location = (x, y), angle } }) =
            translate x y $ rotate (-angle) $ picture
    in  pictures $ mousePicture : map drawVehicle vehicles

update :: TimePassed -> State -> State
update _ state@(State { mouse = Nothing }) = state
update time state@(State { mouse = Just sourceLoc, vehicles }) =
    let updateVehicle vehicle@(Vehicle { function, state = vState }) =
            vehicle { state = function sourceLoc time vState }
    in  state { vehicles = map updateVehicle vehicles }

handleInput :: Event -> State -> State
handleInput (EventKey (SpecialKey KeySpace) Down _ pos) state@(State { mouse }) =
    case mouse of
        Nothing -> state { mouse = Just pos }
        Just _ -> state { mouse = Nothing }
handleInput (EventMotion pos) state@(State { mouse }) =
    case mouse of
        Nothing -> state
        Just _ -> state { mouse = Just pos }
handleInput _ state = state