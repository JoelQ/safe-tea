module Main exposing (main)

import Game exposing (Game(..))
import Html exposing (Html, text)
import Mouse
import Time


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
                Tick ->
                    gameState
                        |> Game.applyMovement
                        |> Game.shoot
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
