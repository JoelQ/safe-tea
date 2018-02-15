module Entity exposing (Entity, render, renderList)

import Element exposing (Element)


type alias Entity a =
    { a
        | x : Int
        , y : Int
        , width : Int
        , height : Int
        , imagePath : String
    }


render : Entity a -> Element
render { x, y, imagePath, width, height } =
    let
        sprite =
            Element.fittedImage width height imagePath

        position =
            Element.topLeftAt (Element.absolute <| x - (width // 2))
                (Element.absolute <| y - (height // 2))
    in
        Element.container 960 960 position sprite


renderList : List (Entity a) -> Element
renderList entities =
    entities
        |> List.map render
        |> Element.layers
