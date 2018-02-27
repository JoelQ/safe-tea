module Path exposing (fromTo, renderList)

import Map exposing (Map)
import AStar
import Element exposing (Element)
import Collage exposing (defaultLine)
import Color
import Coordinate


fromTo : Map -> ( Int, Int ) -> ( Int, Int ) -> Maybe AStar.Path
fromTo map fromCoords ( toX, toY ) =
    let
        destination =
            Map.tileNumberFromCoords toX toY map
                |> Map.centerOfTile map
                |> Coordinate.toTuple
    in
        AStar.findPath AStar.straightLineCost
            (Map.validMovesFrom map)
            fromCoords
            destination


renderList : List (Maybe AStar.Path) -> Element
renderList paths =
    paths
        |> List.map render
        |> Element.layers


render : Maybe AStar.Path -> Element
render path =
    case path of
        Nothing ->
            Element.empty

        Just p ->
            -- Collage.path [ ( 0, 0 ), ( 480, -480 ) ]
            --     |> Collage.traced { defaultLine | width = 10 }
            --     |> Collage.move ( -480, 480 )
            --     |> List.singleton
            --     |> Collage.collage 960 960
            -- Collage.rect 480 480
            --     |> Collage.filled Color.black
            --     |> List.singleton
            --     |> Collage.collage 960 960
            Element.layers [ line p, points p ]


line : AStar.Path -> Element
line path =
    path
        |> List.map (\( x, y ) -> ( toFloat x, negate <| toFloat y ))
        |> Collage.path
        |> Collage.traced defaultLine
        |> Collage.move ( -480, 480 )
        |> List.singleton
        |> Collage.collage 960 960


points : AStar.Path -> Element
points path =
    path
        |> List.map
            (\( x, y ) ->
                Collage.circle 5
                    |> Collage.filled Color.black
                    |> Collage.move ( toFloat (x - 480), toFloat (480 - y) )
            )
        |> Collage.collage 960 960
