module Entity exposing (Entity, render, renderList)

import Element exposing (Element)


type alias Entity =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , imagePath : String
    }


render : Entity -> Element
render { x, y, imagePath, width, height } =
    let
        sprite =
            Element.fittedImage width height imagePath

        position =
            Element.topLeftAt (Element.absolute <| x - (width // 2))
                (Element.absolute <| y - (height // 2))
    in
        Element.container 960 960 position sprite


renderList : List Entity -> Element
renderList entities =
    entities
        |> List.map render
        |> Element.layers
