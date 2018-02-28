module Main exposing (main)

import Game exposing (Game(..))
import Bullet exposing (Bullet)
import Collage
import Html exposing (Html, text)
import Map exposing (Map)
import Mouse
import Element exposing (Element)
import Text
import Pirate exposing (Pirate)
import Entity exposing (Entity)
import Path
import Time
import Coordinate
import Tower exposing (Tower)


type Msg
    = Tick
    | MouseMove Mouse.Position
    | PlaceTower
    | StartPlacement


update : Msg -> Game -> ( Game, Cmd Msg )
update msg gamePhase =
    case gamePhase of
        IntroPhase map ->
            case msg of
                StartPlacement ->
                    Game.startPlacement map
                        |> (\g -> ( g, Cmd.none ))

                _ ->
                    gamePhase
                        |> (\g -> ( g, Cmd.none ))

        TowerPlacementPhase placementState ->
            case msg of
                MouseMove mousePosition ->
                    placementState
                        |> Game.calculateTowerPlacement mousePosition
                        |> TowerPlacementPhase
                        |> (\g -> ( g, Cmd.none ))

                PlaceTower ->
                    placementState
                        |> Game.placeTower
                        |> Game.transitionToAttackIfMaxTowers
                        |> (\g -> ( g, Cmd.none ))

                _ ->
                    gamePhase
                        |> (\g -> ( g, Cmd.none ))

        AttackPhase gameState ->
            case msg of
                Tick ->
                    gameState
                        |> Game.applyMovement
                        |> Game.shoot
                        |> Game.detectCollisions
                        |> Game.eliminateDead
                        |> Game.checkForGameEnd
                        |> (\g -> ( g, Cmd.none ))

                _ ->
                    gameState
                        |> AttackPhase
                        |> (\g -> ( g, Cmd.none ))

        Victory _ ->
            gamePhase
                |> (\g -> ( g, Cmd.none ))

        Defeat _ ->
            gamePhase
                |> (\g -> ( g, Cmd.none ))


view : Game -> Html Msg
view game =
    case game of
        IntroPhase map ->
            Html.div []
                [ Game.viewIntroPhase map
                ]

        TowerPlacementPhase placementState ->
            Html.div []
                [ Game.viewTowerPlacementPhase placementState
                ]

        AttackPhase gameState ->
            Html.div []
                [ Game.viewAttackPhase gameState
                ]

        Victory gameState ->
            Html.div []
                [ Game.viewVictoryScreen gameState
                ]

        Defeat gameState ->
            Html.div []
                [ Game.viewDefeatScreen gameState
                ]


subscriptions : Game -> Sub Msg
subscriptions game =
    case game of
        IntroPhase _ ->
            Mouse.clicks (always StartPlacement)

        TowerPlacementPhase _ ->
            Sub.batch
                [ Mouse.moves MouseMove
                , Mouse.clicks (always PlaceTower)
                ]

        AttackPhase _ ->
            Time.every (33 * Time.millisecond) (always Tick)

        Victory _ ->
            Sub.none

        Defeat _ ->
            Sub.none


main : Program Never Game Msg
main =
    Html.program
        { init = ( Game.initialGame, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
