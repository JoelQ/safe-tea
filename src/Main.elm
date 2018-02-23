module Main exposing (main)

import Html exposing (Html, text)
import Map exposing (Map)
import Element exposing (Element)
import Pirate exposing (Pirate)
import Entity exposing (Entity)
import Path
import Time
import Coordinate


type alias Game =
    { map : Map
    , pirates : List Pirate
    , playerShip : Entity
    }


player : Entity
player =
    { position = Coordinate.fromGlobalXY 416 608
    , width = 33
    , height = 56
    , imagePath = "../images/player-ship.png"
    }


pirate1 : Pirate
pirate1 =
    { position = Coordinate.fromGlobalXY 928 544
    , path = Nothing
    }


pirate2 : Pirate
pirate2 =
    { position = Coordinate.fromGlobalXY 672 928
    , path = Nothing
    }


initialGame : Game
initialGame =
    { map = Map.level1
    , pirates = [ pirate1, pirate2 ]
    , playerShip = player
    }


calculatesPaths : Game -> Game
calculatesPaths game =
    { game | pirates = List.map (calculatePiratePath game) game.pirates }


calculatePiratePath : Game -> Pirate -> Pirate
calculatePiratePath game pirate =
    { pirate
        | path =
            Path.fromTo game.map
                (Coordinate.toTuple pirate.position)
                (Coordinate.toTuple game.playerShip.position)
    }


type Msg
    = Tick


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        Tick ->
            ( { game | pirates = List.map Pirate.move game.pirates }, Cmd.none )


viewMapAndEntities : Game -> Html a
viewMapAndEntities game =
    [ Map.render Map.level1
    , Entity.renderList <| List.map Pirate.toEntity game.pirates
    , Entity.render game.playerShip
    , Path.renderList <| List.map .path game.pirates
    ]
        |> Element.layers
        |> Element.toHtml


showXYEntity : Map -> String -> { a | position : Coordinate.Global } -> Html b
showXYEntity map name { position } =
    let
        ( x, y ) =
            Coordinate.toTuple position
    in
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


subscriptions : Game -> Sub Msg
subscriptions model =
    Time.every (33 * Time.millisecond) (always Tick)


main : Program Never Game Msg
main =
    Html.program
        { init = ( calculatesPaths initialGame, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
