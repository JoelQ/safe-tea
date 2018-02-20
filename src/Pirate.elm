module Pirate exposing (Pirate, toEntity, position, move)

import Element exposing (Element)
import Entity exposing (Entity)
import Map exposing (Map)
import AStar


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
        Just (( newX, newY ) :: rest) ->
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
