module Map.LevelOne exposing (seaTiles, landTiles)

import Tile exposing (Tile(..))


seaTiles : List Tile
seaTiles =
    List.map (always <| TileId 73) landTiles


landTiles : List Tile
landTiles =
    [ TileId 23
    , TileId 23
    , TileId 24
    , TileId 25
    , NoTile
    , NoTile
    , TileId 54
    , TileId 55
    , TileId 55
    , TileId 56
    , TileId 55
    , TileId 37
    , TileId 23
    , TileId 39
    , TileId 23
    , TileId 23
    , TileId 36
    , TileId 55
    , TileId 57
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , TileId 54
    , TileId 37
    , TileId 24
    , TileId 23
    , TileId 36
    , TileId 57
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , TileId 6
    , TileId 7
    , TileId 9
    , NoTile
    , NoTile
    , TileId 54
    , TileId 37
    , TileId 24
    , TileId 41
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , TileId 6
    , TileId 8
    , TileId 53
    , TileId 40
    , TileId 25
    , NoTile
    , NoTile
    , NoTile
    , TileId 54
    , TileId 37
    , TileId 25
    , NoTile
    , NoTile
    , NoTile
    , TileId 6
    , TileId 53
    , TileId 23
    , TileId 40
    , TileId 24
    , TileId 41
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , TileId 22
    , TileId 25
    , NoTile
    , NoTile
    , NoTile
    , TileId 38
    , TileId 40
    , TileId 39
    , TileId 39
    , TileId 23
    , TileId 25
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , TileId 22
    , TileId 25
    , NoTile
    , NoTile
    , NoTile
    , TileId 54
    , TileId 37
    , TileId 23
    , TileId 24
    , TileId 40
    , TileId 25
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , TileId 22
    , TileId 41
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , TileId 22
    , TileId 24
    , TileId 36
    , TileId 56
    , TileId 57
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , TileId 54
    , TileId 57
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , TileId 54
    , TileId 55
    , TileId 57
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , TileId 1
    , TileId 2
    , TileId 3
    , NoTile
    , NoTile
    , TileId 6
    , TileId 7
    , TileId 9
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , TileId 1
    , TileId 2
    , TileId 21
    , TileId 4
    , TileId 35
    , NoTile
    , NoTile
    , TileId 22
    , TileId 24
    , TileId 25
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , TileId 1
    , TileId 21
    , TileId 4
    , TileId 34
    , TileId 35
    , NoTile
    , NoTile
    , NoTile
    , TileId 54
    , TileId 55
    , TileId 57
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , TileId 33
    , TileId 34
    , TileId 35
    , NoTile
    , NoTile
    , NoTile
    , TileId 6
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , TileId 6
    , TileId 9
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , NoTile
    , TileId 6
    , TileId 53
    , NoTile
    , TileId 6
    , TileId 7
    , TileId 7
    , TileId 7
    , TileId 53
    , TileId 52
    , TileId 8
    , TileId 9
    , NoTile
    , NoTile
    , TileId 6
    , TileId 7
    , TileId 53
    , TileId 40
    ]
