module Pirate exposing (Pirate, toEntity, position, move)

import Element exposing (Element)
import Entity exposing (Entity)
import Map exposing (Map)
import AStar
import Euclid.Vector as Vector


type alias Pirate =
    { x : Int
    , y : Int
    , path : Maybe AStar.Path
    }


position : Pirate -> ( Int, Int )
position { x, y } =
    ( x, y )


toEntity : Pirate -> Entity
toEntity { x, y } =
    { x = x
    , y = y
    , width = width
    , height = height
    , imagePath = imagePath
    }


move : Pirate -> Pirate
move pirate =
    case pirate.path of
        Just (nextPoint :: rest) ->
            let
                distanceToNextPoint =
                    AStar.pythagoreanCost (position pirate) nextPoint
            in
                if (round distanceToNextPoint) > speed then
                    let
                        ( nextX, nextY ) =
                            nextPoint

                        here =
                            Vector.vec (toFloat pirate.x) (toFloat pirate.y)

                        finalDestination =
                            Vector.vec (toFloat nextX) (toFloat nextY)

                        totalMove =
                            Vector.subtract finalDestination here

                        thisMove =
                            Vector.fromPolar (Vector.arg totalMove) (toFloat speed)

                        finalMoveVector =
                            Vector.add here thisMove
                    in
                        { pirate | x = round finalMoveVector.x, y = round finalMoveVector.y }
                else
                    let
                        ( newX, newY ) =
                            nextPoint
                    in
                        { pirate | x = newX, y = newY, path = Just rest }

        _ ->
            pirate


imagePath : String
imagePath =
    "../images/pirate-new.png"


width : Int
width =
    33


height : Int
height =
    56


speed : Int
speed =
    2
