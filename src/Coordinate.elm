module Coordinate
    exposing
        ( Global
        , Local
        , toGlobal
        , toLocal
        , toTuple
        , fromGlobalXY
        , setMagnitude
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


setMagnitude : Int -> Local -> Local
setMagnitude mag (Local position) =
    let
        theta =
            position
                |> Vector.map toFloat
                |> Vector.arg
    in
        Vector.fromPolar theta (toFloat mag)
            |> Vector.map round
            |> Local
