module Main exposing (main)

import Bullet exposing (Bullet)
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


type Game
    = IntroPhase Map
    | AttackPhase GameState


type alias GameState =
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



-- initialGame : Game
-- initialGame =
--     { map = Map.level1
--     , pirates = [ pirate1, pirate2, pirate3 ]
--     , playerShip = player
--     , towerPlacement = Tower.noPlacement
--     , towers = []
--     , bullets = []
--     }


initialGame : Game
initialGame =
    IntroPhase Map.level1


calculatesPaths : GameState -> GameState
calculatesPaths game =
    { game | pirates = List.map (calculatePiratePath game) game.pirates }


calculatePiratePath : GameState -> Pirate -> Pirate
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
update msg gamePhase =
    case gamePhase of
        IntroPhase map ->
            gamePhase
                |> (\g -> ( g, Cmd.none ))

        AttackPhase gameState ->
            case msg of
                Tick ->
                    gameState
                        |> applyMovement
                        |> shoot
                        |> detectCollisions
                        |> eliminateDead
                        |> AttackPhase
                        |> (\g -> ( g, Cmd.none ))

                MouseMove mousePosition ->
                    gameState
                        |> calculateTowerPlacement mousePosition
                        |> AttackPhase
                        |> (\g -> ( g, Cmd.none ))

                PlaceTower ->
                    gameState
                        |> placeTower
                        |> AttackPhase
                        |> (\g -> ( g, Cmd.none ))


calculateTowerPlacement : Mouse.Position -> GameState -> GameState
calculateTowerPlacement mousePosition gameState =
    { gameState | towerPlacement = Tower.placement mousePosition gameState.map }


placeTower : GameState -> GameState
placeTower gameState =
    let
        towers =
            Tower.placeTower gameState.map gameState.towers gameState.towerPlacement
    in
        { gameState | towers = towers }


applyMovement : GameState -> GameState
applyMovement game =
    { game
        | pirates = List.map Pirate.move game.pirates
        , bullets = List.map Bullet.move game.bullets
    }


collided : List Bullet -> Pirate -> Maybe ( Pirate, Bullet )
collided bullets pirate =
    bullets
        |> List.filter
            (\bullet ->
                Coordinate.distance pirate.position bullet.position < 10
            )
        |> List.head
        |> Maybe.map (\bullet -> ( pirate, bullet ))


detectCollisions : GameState -> GameState
detectCollisions game =
    let
        ( collidedPirates, collidedBullets ) =
            game.pirates
                |> List.filterMap (collided game.bullets)
                |> List.unzip

        remainingPirates =
            List.filter (\p -> not (List.member p collidedPirates)) game.pirates

        remainingBullets =
            List.filter (\b -> not (List.member b collidedBullets)) game.bullets
    in
        { game
            | pirates = remainingPirates
            , bullets = remainingBullets
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


shoot : GameState -> GameState
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


eliminateDead : GameState -> GameState
eliminateDead game =
    { game | bullets = List.filter (\b -> b.position /= b.target) game.bullets }


viewAttackPhase : GameState -> Html a
viewAttackPhase gameState =
    [ Map.render gameState.map
    , Entity.renderList <| List.map Pirate.toEntity gameState.pirates
    , Entity.render gameState.playerShip
    , Tower.renderPlacement gameState.map gameState.towerPlacement
    , Tower.renderTowers gameState.map gameState.towers
    , Entity.renderList <| List.map Bullet.toEntity gameState.bullets
    , Path.renderList <| List.map .path gameState.pirates
    ]
        |> Element.layers
        |> Element.toHtml


introText : String
introText =
    """
  You are a tea merchant and your ship has just run aground on a
  sandbank.  To make things worse, it seems like a group of pirates
  have noticed your predicament.

  Your crew have time to build 3 towers before the pirates attack.
  Make the most of them.

  Defeat the pirates! Save the tea!

  Click anywhere to continue...
  """


viewIntroPhase : Map -> Html a
viewIntroPhase map =
    [ Map.render map |> Element.opacity 0.3
    , Text.fromString introText
        |> Text.height 30
        |> Text.typeface [ "helvetica", "arial", "sans-serif" ]
        |> Element.centered
    ]
        |> Element.layers
        |> Element.toHtml


view : Game -> Html Msg
view game =
    case game of
        IntroPhase map ->
            Html.div []
                [ viewIntroPhase map
                ]

        AttackPhase gameState ->
            Html.div []
                [ viewAttackPhase gameState
                ]


subscriptions : Game -> Sub Msg
subscriptions game =
    case game of
        IntroPhase _ ->
            Sub.none

        AttackPhase _ ->
            Sub.batch
                [ Time.every (33 * Time.millisecond) (always Tick)
                , Mouse.moves MouseMove
                , Mouse.clicks (always PlaceTower)
                ]


main : Program Never Game Msg
main =
    Html.program
        { init = ( initialGame, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
