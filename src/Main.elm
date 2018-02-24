module Main exposing (main)

import Html exposing (Html, text)
import Map exposing (Map, TileNumber(..), Pixels(..))
import Mouse
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
    , towerPlacement : Maybe TileNumber
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


pirate3 : Pirate
pirate3 =
    { position = Coordinate.fromGlobalXY 325 32
    , path = Nothing
    }


initialGame : Game
initialGame =
    { map = Map.level1
    , pirates = [ pirate1, pirate2, pirate3 ]
    , playerShip = player
    , towerPlacement = Just (TileNumber 55)
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
    | MouseMove Mouse.Position


towerPlacement : Mouse.Position -> Map -> Maybe TileNumber
towerPlacement { x, y } map =
    let
        (Pixels mapWidth) =
            Map.pixelWidth map

        (Pixels mapHeight) =
            Map.pixelHeight map
    in
        if x < mapWidth && y < mapHeight then
            Just (Map.tileNumberFromCoords x y map)
        else
            Nothing


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        Tick ->
            ( { game | pirates = List.map Pirate.move game.pirates }, Cmd.none )

        MouseMove mousePosition ->
            ( { game | towerPlacement = towerPlacement mousePosition game.map }
            , Cmd.none
            )


renderTowerPlacement : Game -> Element
renderTowerPlacement { map, towerPlacement } =
    case towerPlacement of
        Just tileNumber ->
            Map.renderInvalidOverlayAt map tileNumber

        Nothing ->
            Element.empty


viewMapAndEntities : Game -> Html a
viewMapAndEntities game =
    [ Map.render Map.level1
    , Entity.renderList <| List.map Pirate.toEntity game.pirates
    , Entity.render game.playerShip
    , renderTowerPlacement game
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
    Sub.batch
        [ Time.every (33 * Time.millisecond) (always Tick)
        , Mouse.moves MouseMove
        ]


main : Program Never Game Msg
main =
    Html.program
        { init = ( calculatesPaths initialGame, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
