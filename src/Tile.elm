module Tile exposing (Tile(..), render)

import TileSheet exposing (TileSheet)
import Element exposing (Element)


type Tile
    = NoTile
    | TileId Int


render : TileSheet -> Tile -> Element
render sheet tile =
    case tile of
        TileId id ->
            renderTileId sheet id

        NoTile ->
            renderEmptyTile sheet


renderTileId : TileSheet -> Int -> Element
renderTileId { sheetWidth, tileSide, source } tileId =
    let
        x =
            tileSide * ((tileId - 1) % sheetWidth)

        y =
            tileSide * ((tileId - 1) // sheetWidth)
    in
        Element.croppedImage ( x, y ) tileSide tileSide source


renderEmptyTile : TileSheet -> Element
renderEmptyTile { tileSide } =
    Element.spacer tileSide tileSide
