module Coordinate
    exposing
        ( Global
        , Local
        , toGlobal
        , toLocal
        , toTuple
        , fromGlobalXY
        , setMagnitude
        , distance
        , toTileCenter
        )

import Euclid.Vector as Vector


type Global
    = Global (Vector.V2 Int)


type Local
    = Local (Vector.V2 Int)


toTuple : Global -> ( Int, Int )
toTuple (Global { x, y }) =
    ( x, y )


fromGlobalXY : Int -> Int -> Global
fromGlobalXY x y =
    Global <| Vector.vec x y


toGlobal : Global -> Local -> Global
toGlobal (Global offset) (Local position) =
    Global <| Vector.add offset position


toLocal : Global -> Global -> Local
toLocal (Global offset) (Global position) =
    offset
        |> Vector.subtract position
        |> Local


setMagnitude : Float -> Local -> Local
setMagnitude mag (Local position) =
    let
        theta =
            position
                |> Vector.map toFloat
                |> Vector.arg
    in
        Vector.fromPolar theta mag
            |> Vector.map round
            |> Local


distance : Global -> Global -> Float
distance (Global position1) (Global position2) =
    position1
        |> Vector.map toFloat
        |> Vector.subtract (Vector.map toFloat position2)
        |> Vector.abs


toTileCenter : Int -> Global -> Global
toTileCenter tileWidth (Global currentPosition) =
    Vector.vec (tileWidth // 2) (tileWidth // 2)
        |> Vector.add currentPosition
        |> Global
