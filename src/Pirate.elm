module Pirate exposing (Pirate, toEntity)

import Element exposing (Element)
import Entity exposing (Entity)


type alias Pirate =
    { x : Int
    , y : Int
    }


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
