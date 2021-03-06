module Tower
    exposing
        ( Placement
        , Tower
        , isReadyToShoot
        , shoot
        , timeElapsed
        , placement
        , renderPlacement
        , noPlacement
        , renderTowers
        , placeTower
        , maxRange
        )

import Element exposing (Element)
import Map exposing (Map, Pixels(..))
import Mouse
import Tile
import TileSheet exposing (TileSheet)
import Time exposing (Time)
import Color
import Coordinate


type alias Tower =
    { tileNumber : Map.TileNumber
    , position : Coordinate.Global
    , timeSinceLastShot : Time
    }


isReadyToShoot : Tower -> Bool
isReadyToShoot { timeSinceLastShot } =
    timeSinceLastShot > reloadTime


shoot : Time -> Tower -> Tower
shoot diff tower =
    { tower | timeSinceLastShot = tower.timeSinceLastShot + diff - reloadTime }


timeElapsed : Time -> Tower -> Tower
timeElapsed diff tower =
    { tower | timeSinceLastShot = tower.timeSinceLastShot + diff }


reloadTime : Time
reloadTime =
    Time.second / 1


fromTileNumber : Map -> Map.TileNumber -> Tower
fromTileNumber map tileNumber =
    { tileNumber = tileNumber
    , position = Map.centerOfTile map tileNumber
    , timeSinceLastShot = 0
    }


maxRange : number
maxRange =
    100


type Placement
    = NoPlacement
    | InvalidPlacement Map.TileNumber
    | ValidPlacement Map.TileNumber


noPlacement : Placement
noPlacement =
    NoPlacement


placement : Mouse.Position -> Map -> Placement
placement { x, y } map =
    let
        (Pixels mapWidth) =
            Map.pixelWidth map

        (Pixels mapHeight) =
            Map.pixelHeight map

        tileNumber =
            Map.tileNumberFromCoords x y map
    in
        if x < mapWidth && y < mapHeight then
            if Map.isBuildableTile map tileNumber then
                ValidPlacement tileNumber
            else
                InvalidPlacement tileNumber
        else
            NoPlacement


placeTower : Map -> List Tower -> Placement -> List Tower
placeTower map towers placement =
    case placement of
        ValidPlacement tileNumber ->
            (fromTileNumber map tileNumber) :: towers

        _ ->
            towers


renderPlacement : Map -> Placement -> Element
renderPlacement map towerPlacement =
    case towerPlacement of
        ValidPlacement tileNumber ->
            validPlacementOverlay
                |> Map.renderAtTile map tileNumber

        InvalidPlacement tileNumber ->
            invalidPlacementOverlay map.sheet
                |> Map.renderAtTile map tileNumber

        NoPlacement ->
            Element.empty


renderTowerSprite : Element
renderTowerSprite =
    Tile.render TileSheet.kennyPirates Tile.kennyPirateTower


validPlacementOverlay : Element
validPlacementOverlay =
    renderTowerSprite
        |> Element.opacity 0.66


invalidPlacementOverlay : TileSheet -> Element
invalidPlacementOverlay { tileSide } =
    Element.spacer tileSide tileSide
        |> Element.color Color.red
        |> Element.opacity 0.3


renderTowers : Map -> List Tower -> Element
renderTowers map towers =
    towers
        |> List.map (\{ tileNumber } -> Map.renderAtTile map tileNumber renderTowerSprite)
        |> Element.layers
