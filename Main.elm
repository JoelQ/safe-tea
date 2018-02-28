module Main exposing (main)

import AnimationFrame
import Game exposing (Game(..))
import Html exposing (Html, text)
import Mouse
import Time exposing (Time)


type Msg
    = Tick Time
    | MouseMove Mouse.Position
    | PlaceTower
    | StartPlacement


update : Msg -> Game -> ( Game, Cmd Msg )
update msg gamePhase =
    case gamePhase of
        IntroPhase introState ->
            case msg of
                StartPlacement ->
                    Game.startPlacement introState
                        |> withNoCmd

                _ ->
                    gamePhase
                        |> withNoCmd

        TowerPlacementPhase placementState ->
            case msg of
                MouseMove mousePosition ->
                    placementState
                        |> Game.calculateTowerPlacement mousePosition
                        |> TowerPlacementPhase
                        |> withNoCmd

                PlaceTower ->
                    placementState
                        |> Game.placeTower
                        |> Game.transitionToAttackIfMaxTowers
                        |> withNoCmd

                _ ->
                    gamePhase
                        |> withNoCmd

        AttackPhase gameState ->
            case msg of
                Tick diff ->
                    gameState
                        |> Game.applyMovement diff
                        |> Game.shoot diff
                        |> Game.detectCollisions
                        |> Game.eliminateDead
                        |> Game.checkForGameEnd
                        |> withNoCmd

                _ ->
                    gameState
                        |> AttackPhase
                        |> withNoCmd

        Victory _ ->
            gamePhase
                |> withNoCmd

        Defeat _ ->
            gamePhase
                |> withNoCmd


withNoCmd : Game -> ( Game, Cmd Msg )
withNoCmd game =
    ( game, Cmd.none )


view : Game -> Html Msg
view game =
    case game of
        IntroPhase map ->
            Game.viewIntroPhase map

        TowerPlacementPhase placementState ->
            Game.viewTowerPlacementPhase placementState

        AttackPhase gameState ->
            Game.viewAttackPhase gameState

        Victory gameState ->
            Game.viewVictoryScreen gameState

        Defeat gameState ->
            Game.viewDefeatScreen gameState


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
            AnimationFrame.diffs Tick

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
