module Map exposing (Map, render, level1)

import Layer exposing (Layer)
import Element exposing (Element)


type alias Map =
    { layers : List Layer
    }


level1 : Map
level1 =
    { layers = [ Layer.sea, Layer.land ]
    }


render : Map -> Element
render map =
    map.layers
        |> List.map Layer.render
        |> Element.layers
