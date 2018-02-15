module Pirate exposing (Pirate, renderList)

import Element exposing (Element)


type alias Pirate =
    { x : Int
    , y : Int
    }


sprite : Element
sprite =
    Element.fittedImage width height "../images/pirate-new.png"


width : Int
width =
    33


height : Int
height =
    56


render : Pirate -> Element
render { x, y } =
    let
        position =
            Element.topLeftAt (Element.absolute <| x - (width // 2))
                (Element.absolute <| y - (height // 2))
    in
        Element.container 960 960 position sprite


renderList : List Pirate -> Element
renderList pirates =
    pirates
        |> List.map render
        |> Element.layers
