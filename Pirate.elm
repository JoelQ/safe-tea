module Pirate exposing (Pirate, toEntity, move, takeAHit, isDead)

import Element exposing (Element)
import Entity exposing (Entity)
import Map exposing (Map)
import AStar
import Euclid.Vector as Vector
import Coordinate
import Time exposing (Time)


type alias Pirate =
    { position : Coordinate.Global
    , path : Maybe AStar.Path
    , health : Int
    }


toEntity : Pirate -> Entity
toEntity { position } =
    { position = position
    , width = width
    , height = height
    , imagePath = imagePath
    }


move : Time -> Pirate -> Pirate
move diff pirate =
    case pirate.path of
        Just (nextPoint :: rest) ->
            let
                distanceToNextPoint =
                    AStar.pythagoreanCost (Coordinate.toTuple pirate.position)
                        nextPoint

                distanceInThisInterval =
                    (Time.inSeconds diff) * speedPerSecond
            in
                if distanceToNextPoint > distanceInThisInterval then
                    let
                        ( nextX, nextY ) =
                            nextPoint

                        thisTickG =
                            Coordinate.fromGlobalXY nextX nextY
                                |> Coordinate.toLocal pirate.position
                                |> Coordinate.setMagnitude distanceInThisInterval
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


takeAHit : Pirate -> Pirate
takeAHit pirate =
    { pirate | health = pirate.health - 1 }


isDead : Pirate -> Bool
isDead { health } =
    health <= 0


imagePath : String
imagePath =
    "images/pirate-new.png"


width : Int
width =
    33


height : Int
height =
    56


speedPerSecond : number
speedPerSecond =
    60
