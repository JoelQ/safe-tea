module Entity exposing (Entity, render, renderList)

import Element exposing (Element)
import Coordinate


type alias Entity =
    { position : Coordinate.Global
    , width : Int
    , height : Int
    , imagePath : String
    }


render : Entity -> Element
render { position, imagePath, width, height } =
    let
        ( x, y ) =
            Coordinate.toTuple position

        sprite =
            Element.fittedImage width height imagePath

        screenPosition =
            Element.topLeftAt (Element.absolute <| x - (width // 2))
                (Element.absolute <| y - (height // 2))
    in
        Element.container 960 960 screenPosition sprite


renderList : List Entity -> Element
renderList entities =
    entities
        |> List.map render
        |> Element.layers
