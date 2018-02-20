module Pirate exposing (Pirate, toEntity, position)

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


imagePath : String
imagePath =
    "../images/pirate-new.png"


width : Int
width =
    33


height : Int
height =
    56
