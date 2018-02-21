module Pirate exposing (Pirate, toEntity, position, move)

import Element exposing (Element)
import Entity exposing (Entity)
import Map exposing (Map)
import AStar
import Euclid.Vector as Vector
import Coordinate


type alias Pirate =
    { position : Coordinate.Global
    , path : Maybe AStar.Path
    }


position : Pirate -> ( Int, Int )
position pirate =
    Coordinate.toTuple pirate.position


toEntity : Pirate -> Entity
toEntity { position } =
    { position = position
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

                        thisTickG =
                            Coordinate.fromGlobalXY nextX nextY
                                |> Coordinate.toLocal pirate.position
                                |> Coordinate.setMagnitude speed
                                |> Coordinate.toGlobal pirate.position
                    in
                        { pirate | position = thisTickG }
                else
                    let
                        ( newX, newY ) =
                            nextPoint

                        newPosition =
                            Coordinate.fromGlobalXY newX newY
                    in
                        { pirate | position = newPosition, path = Just rest }

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
