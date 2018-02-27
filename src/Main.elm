module Main exposing (main)

import Bullet exposing (Bullet)
import Html exposing (Html, text)
import Map exposing (Map)
import Mouse
import Element exposing (Element)
import Pirate exposing (Pirate)
import Entity exposing (Entity)
import Path
import Time
import Coordinate
import Tower exposing (Tower)


type alias Game =
    { map : Map
    , pirates : List Pirate
    , playerShip : Entity
    , towerPlacement : Tower.Placement
    , towers : List Tower
    , bullets : List Bullet
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
    , towerPlacement = Tower.noPlacement
    , towers = []
    , bullets = []
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
    | PlaceTower


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        Tick ->
            ( game |> applyMovement |> shoot, Cmd.none )

        MouseMove mousePosition ->
            ( { game | towerPlacement = Tower.placement mousePosition game.map }
            , Cmd.none
            )

        PlaceTower ->
            ( { game | towers = Tower.placeTower game.map game.towers game.towerPlacement }
            , Cmd.none
            )


applyMovement : Game -> Game
applyMovement game =
    { game
        | pirates = List.map Pirate.move game.pirates
        , bullets = List.map Bullet.move game.bullets
    }


shootPirate : Tower -> Maybe Pirate -> ( Tower, Maybe Bullet )
shootPirate tower pirateInRange =
    case pirateInRange of
        Just pirate ->
            let
                bullet =
                    Bullet.fireFrom tower.position
                        |> Bullet.fireTowards pirate.position
            in
                ( { tower | hasShot = True }, Just bullet )

        Nothing ->
            ( tower, Nothing )


shootNearbyPirates : List Pirate -> Tower -> ( Tower, Maybe Bullet )
shootNearbyPirates pirates tower =
    pirates
        |> List.filter (\pirate -> Coordinate.distance tower.position pirate.position < Tower.maxRange)
        |> List.head
        |> shootPirate tower


attemptToShootNearbyPirates : List Pirate -> Tower -> ( Tower, Maybe Bullet )
attemptToShootNearbyPirates pirates tower =
    if tower.hasShot then
        ( tower, Nothing )
    else
        shootNearbyPirates pirates tower


shoot : Game -> Game
shoot ({ pirates, towers } as game) =
    let
        ( shotTowers, bullets ) =
            List.map (attemptToShootNearbyPirates pirates) towers
                |> List.unzip
    in
        { game
            | towers = shotTowers
            , bullets = game.bullets ++ List.filterMap identity bullets
        }


viewMapAndEntities : Game -> Html a
viewMapAndEntities game =
    [ Map.render Map.level1
    , Entity.renderList <| List.map Pirate.toEntity game.pirates
    , Entity.render game.playerShip
    , Tower.renderPlacement game.map game.towerPlacement
    , Tower.renderTowers game.map game.towers
    , Entity.renderList <| List.map Bullet.toEntity game.bullets
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
        , Mouse.clicks (always PlaceTower)
        ]


main : Program Never Game Msg
main =
    Html.program
        { init = ( calculatesPaths initialGame, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
