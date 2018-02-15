module Main exposing (main)

import Html exposing (Html, text)
import Map exposing (Map)
import Element exposing (Element)
import Pirate exposing (Pirate)
import Entity exposing (Entity)


type alias Game =
    { map : Map
    , pirates : List Pirate
    , playerShip : Entity
    }


player : Entity
player =
    { x = 416
    , y = 576
    , width = 33
    , height = 56
    , imagePath = "../images/player-ship.png"
    }


pirate1 : Pirate
pirate1 =
    { x = 896
    , y = 544
    }


pirate2 : Pirate
pirate2 =
    { x = 640
    , y = 896
    }


initialGame : Game
initialGame =
    { map = Map.level1
    , pirates = [ pirate1, pirate2 ]
    , playerShip = player
    }


type Msg
    = Start
    | End


update : Msg -> Game -> Game
update msg game =
    game


viewMapAndEntities : Game -> Html a
viewMapAndEntities game =
    [ Map.render Map.level1
    , Entity.renderList <| List.map Pirate.toEntity game.pirates
    , Entity.render game.playerShip
    ]
        |> Element.layers
        |> Element.toHtml


showXYEntity : Map -> String -> { a | x : Int, y : Int } -> Html b
showXYEntity map name { x, y } =
    Html.div []
        [ Html.h4 [] [ text name ]
        , Html.ul []
            [ Html.li [] [ text <| "X: " ++ toString x ]
            , Html.li [] [ text <| "Y: " ++ toString y ]
            , Html.li [] [ text <| "Tile number: " ++ (toString <| Map.tileNumberFromCoords x y map) ]
            ]
        ]


debugInfo : Game -> Html Msg
debugInfo game =
    let
        player =
            showXYEntity game.map "Player Ship" game.playerShip

        pirates =
            List.map (showXYEntity game.map "Pirate") game.pirates

        header =
            Html.h2 [] [ text "Debug Entities" ]
    in
        Html.section [] (header :: player :: pirates)


view : Game -> Html Msg
view game =
    Html.div []
        [ viewMapAndEntities game
        , debugInfo game
        ]


main : Program Never Game Msg
main =
    Html.beginnerProgram
        { model = initialGame
        , update = update
        , view = view
        }
