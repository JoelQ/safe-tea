module Game
    exposing
        ( Game(..)
        , initialGame
        , startPlacement
        , transitionToAttackIfMaxTowers
        , checkForGameEnd
        , calculateTowerPlacement
        , placeTower
        , applyMovement
        , shoot
        , detectCollisions
        , eliminateDead
        , viewIntroPhase
        , viewTowerPlacementPhase
        , viewAttackPhase
        , viewVictoryScreen
        , viewDefeatScreen
        )

import Bullet exposing (Bullet)
import Coordinate
import Collage
import Element exposing (Element)
import Entity exposing (Entity)
import Html exposing (Html)
import Map exposing (Map)
import Mouse
import Path
import Pirate exposing (Pirate)
import Text
import Tower exposing (Tower)


type Game
    = IntroPhase Map
    | TowerPlacementPhase PlacementState
    | AttackPhase GameState
    | Victory GameState
    | Defeat GameState


type alias PlacementState =
    { map : Map
    , towerPlacement : Tower.Placement
    , playerShip : Entity
    , towers : List Tower
    }


type alias GameState =
    { map : Map
    , pirates : List Pirate
    , playerShip : Entity
    , towers : List Tower
    , bullets : List Bullet
    }



-- CONSTANTS


initialGame : Game
initialGame =
    IntroPhase Map.level1


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



-- TRANSITIONS


startPlacement : Map -> Game
startPlacement map =
    TowerPlacementPhase
        { map = map
        , playerShip = player
        , towerPlacement = Tower.noPlacement
        , towers = []
        }


transitionToAttackIfMaxTowers : PlacementState -> Game
transitionToAttackIfMaxTowers placementState =
    if List.length placementState.towers >= 3 then
        placementState
            |> toAttackState
            |> calculatesPaths
            |> AttackPhase
    else
        TowerPlacementPhase placementState


toAttackState : PlacementState -> GameState
toAttackState placementState =
    { map = placementState.map
    , playerShip = placementState.playerShip
    , towers = placementState.towers
    , pirates = [ pirate1, pirate2, pirate3 ]
    , bullets = []
    }


checkForGameEnd : GameState -> Game
checkForGameEnd ({ pirates, playerShip } as gameState) =
    if List.isEmpty pirates then
        Victory gameState
    else if List.any (canPlunderPlayer playerShip) pirates then
        Defeat gameState
    else
        AttackPhase gameState


canPlunderPlayer : Entity -> Pirate -> Bool
canPlunderPlayer player pirate =
    (Coordinate.distance player.position pirate.position) < 10


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



-- PLACEMENT STATE TRANSFORMS


calculateTowerPlacement : Mouse.Position -> PlacementState -> PlacementState
calculateTowerPlacement mousePosition ({ map } as placementState) =
    { placementState | towerPlacement = Tower.placement mousePosition map }


placeTower : PlacementState -> PlacementState
placeTower ({ map, towers, towerPlacement } as placementState) =
    let
        placedTowers =
            Tower.placeTower map towers towerPlacement
    in
        { placementState | towers = placedTowers }



-- TICK GAME STATE


applyMovement : GameState -> GameState
applyMovement game =
    { game
        | pirates = List.map Pirate.move game.pirates
        , bullets = List.map Bullet.move game.bullets
    }


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



-- COLLISION DETECTION


collided : List Bullet -> Pirate -> Maybe ( Pirate, Bullet )
collided bullets pirate =
    bullets
        |> List.filter
            (\bullet ->
                Coordinate.distance pirate.position bullet.position < 10
            )
        |> List.head
        |> Maybe.map (\bullet -> ( pirate, bullet ))



-- SHOOTING RANGE


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



-- VIEW


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


viewTowerPlacementPhase : PlacementState -> Html a
viewTowerPlacementPhase { map, playerShip, towerPlacement, towers } =
    [ Map.render map
    , Entity.render playerShip
    , Tower.renderPlacement map towerPlacement
    , Tower.renderTowers map towers
    ]
        |> Element.layers
        |> Element.toHtml


viewAttackPhase : GameState -> Html a
viewAttackPhase gameState =
    gameState
        |> renderGameState
        |> Element.toHtml


viewVictoryScreen : GameState -> Html a
viewVictoryScreen gameState =
    [ gameState
        |> renderGameState
        |> Element.opacity 0.3
    , "Safe Tea!"
        |> Text.fromString
        |> Text.height 150
        |> Text.typeface [ "helvetica", "arial", "sans-serif" ]
        |> Element.centered
        |> Collage.toForm
        |> Collage.moveY 150
        |> List.singleton
        |> Collage.collage 960 960
    ]
        |> Element.layers
        |> Element.toHtml


viewDefeatScreen : GameState -> Html a
viewDefeatScreen gameState =
    [ gameState
        |> renderGameState
        |> Element.opacity 0.3
    , "Defeat!"
        |> Text.fromString
        |> Text.height 150
        |> Text.typeface [ "helvetica", "arial", "sans-serif" ]
        |> Element.centered
        |> Collage.toForm
        |> Collage.moveY 150
        |> List.singleton
        |> Collage.collage 960 960
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


renderGameState : GameState -> Element
renderGameState gameState =
    [ Map.render gameState.map
    , Entity.renderList <| List.map Pirate.toEntity gameState.pirates
    , Entity.render gameState.playerShip
    , Tower.renderTowers gameState.map gameState.towers
    , Entity.renderList <| List.map Bullet.toEntity gameState.bullets
    ]
        |> Element.layers