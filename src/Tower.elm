module Tower
    exposing
        ( Placement
        , placement
        , renderPlacement
        , noPlacement
        , renderTowers
        , placeTower
        )

import Element exposing (Element)
import Map exposing (Map, TileNumber, Pixels(..))
import Mouse
import Tile
import TileSheet exposing (TileSheet)
import Color


type Placement
    = NoPlacement
    | InvalidPlacement TileNumber
    | ValidPlacement TileNumber


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


placeTower : List TileNumber -> Placement -> List TileNumber
placeTower towers placement =
    case placement of
        ValidPlacement tileNumber ->
            tileNumber :: towers

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


renderTowers : Map -> List TileNumber -> Element
renderTowers map tileNumbers =
    tileNumbers
        |> List.map (\tn -> Map.renderAtTile map tn renderTowerSprite)
        |> Element.layers
