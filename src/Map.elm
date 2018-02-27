module Map
    exposing
        ( Map
        , render
        , renderAtTile
        , level1
        , tileNumberFromCoords
        , topLeftCornerOfTile
        , centerOfTile
        , validMovesFrom
        , pixelWidth
        , pixelHeight
        , isBuildableTile
        , TileNumber(..)
        , Pixels(..)
        )

import Element exposing (Element)
import List.Extra
import Set exposing (Set)
import Tile exposing (Tile(..))
import TileSheet exposing (TileSheet)
import Coordinate


type Pixels
    = Pixels Int


type TileNumber
    = TileNumber Int


type Region
    = NorthWestCorner
    | NorthEdge
    | NorthEastCorner
    | WestEdge
    | Middle
    | EastEdge
    | SouthWestCorner
    | SouthEdge
    | SouthEastCorner


region : Map -> TileNumber -> Region
region { land, width } (TileNumber tileNumber) =
    let
        height =
            (List.length land) // width

        x =
            tileNumber % width

        y =
            tileNumber // height
    in
        if ( x, y ) == ( 0, 0 ) then
            NorthWestCorner
        else if ( x, y ) == ( width - 1, 0 ) then
            NorthEastCorner
        else if ( x, y ) == ( 0, height - 1 ) then
            SouthWestCorner
        else if ( x, y ) == ( width - 1, height - 1 ) then
            SouthEastCorner
        else if y == 0 then
            NorthEdge
        else if x == 0 then
            WestEdge
        else if x == (width - 1) then
            EastEdge
        else if y == (height - 1) then
            SouthEdge
        else
            Middle


type alias Map =
    { land : List Tile
    , sea : List Tile
    , width : Int
    , sheet : TileSheet
    }


level1 : Map
level1 =
    { land = landTiles
    , sea = seaTiles
    , sheet = TileSheet.kennyPirates
    , width = 15
    }


pixelWidth : Map -> Pixels
pixelWidth { width, sheet } =
    Pixels <| width * sheet.tileSide


pixelHeight : Map -> Pixels
pixelHeight { width, land, sheet } =
    Pixels <| (List.length land // width) * sheet.tileSide


render : Map -> Element
render map =
    [ renderSea map, renderLand map ]
        |> Element.layers


renderSea : Map -> Element
renderSea { sheet, sea, width } =
    renderTiles sheet sea width


renderLand : Map -> Element
renderLand { sheet, land, width } =
    renderTiles sheet land width


renderTiles : TileSheet -> List Tile -> Int -> Element
renderTiles sheet tiles width =
    tiles
        |> List.map (Tile.render sheet)
        |> List.Extra.groupsOf width
        |> List.map (Element.flow Element.right)
        |> Element.flow Element.down


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


renderAtTile : Map -> TileNumber -> Element -> Element
renderAtTile ({ sheet } as map) tileNumber element =
    let
        ( x, y ) =
            Coordinate.toTuple <| topLeftCornerOfTile map tileNumber

        screenPosition =
            Element.topLeftAt (Element.absolute x) (Element.absolute y)
    in
        Element.container 960 960 screenPosition element


tileNumberFromCoords : Int -> Int -> Map -> TileNumber
tileNumberFromCoords x y { width, sheet } =
    let
        tilesFromRight =
            (x // sheet.tileSide)

        tilesFromTop =
            y // sheet.tileSide
    in
        TileNumber ((tilesFromTop * width) + tilesFromRight)


passableNeighboringTiles : Map -> ( Int, Int ) -> List TileNumber
passableNeighboringTiles map coords =
    let
        neighbors =
            neighboringTiles map coords
                |> List.map (\(TileNumber tn) -> tn)
                |> Set.fromList

        passable =
            passableTiles map
                |> List.map (\(TileNumber tn) -> tn)
                |> Set.fromList
    in
        Set.intersect neighbors passable
            |> Set.toList
            |> List.map TileNumber


neighboringTiles : Map -> ( Int, Int ) -> List TileNumber
neighboringTiles ({ width, sheet } as map) ( x, y ) =
    let
        (TileNumber tileNumber) =
            tileNumberFromCoords x y map

        north =
            tileNumber - width

        south =
            tileNumber + width

        east =
            tileNumber + 1

        west =
            tileNumber - 1

        northEast =
            north + 1

        northWest =
            north - 1

        southEast =
            south + 1

        southWest =
            south - 1
    in
        case region map (TileNumber tileNumber) of
            NorthWestCorner ->
                [ TileNumber south
                , TileNumber east
                , TileNumber southEast
                ]

            NorthEastCorner ->
                [ TileNumber south
                , TileNumber west
                , TileNumber southWest
                ]

            SouthWestCorner ->
                [ TileNumber north
                , TileNumber east
                , TileNumber northEast
                ]

            SouthEastCorner ->
                [ TileNumber north
                , TileNumber west
                , TileNumber northWest
                ]

            NorthEdge ->
                [ TileNumber south
                , TileNumber east
                , TileNumber west
                , TileNumber southEast
                , TileNumber southWest
                ]

            WestEdge ->
                [ TileNumber north
                , TileNumber south
                , TileNumber east
                , TileNumber northEast
                , TileNumber southEast
                ]

            EastEdge ->
                [ TileNumber north
                , TileNumber south
                , TileNumber west
                , TileNumber northWest
                , TileNumber southWest
                ]

            SouthEdge ->
                [ TileNumber north
                , TileNumber east
                , TileNumber west
                , TileNumber northEast
                , TileNumber northWest
                ]

            Middle ->
                [ TileNumber north
                , TileNumber south
                , TileNumber east
                , TileNumber west
                , TileNumber northEast
                , TileNumber northWest
                , TileNumber southEast
                , TileNumber southWest
                ]


validMovesFrom : Map -> ( Int, Int ) -> Set ( Int, Int )
validMovesFrom map position =
    passableNeighboringTiles map position
        |> List.map (Coordinate.toTuple << centerOfTile map)
        |> Set.fromList


topLeftCornerOfTile : Map -> TileNumber -> Coordinate.Global
topLeftCornerOfTile { sheet, width } (TileNumber tileNumber) =
    let
        xEdge =
            sheet.tileSide * (tileNumber % width)

        yEdge =
            sheet.tileSide * (tileNumber // width)
    in
        Coordinate.fromGlobalXY xEdge yEdge


centerOfTile : Map -> TileNumber -> Coordinate.Global
centerOfTile map tileNumber =
    topLeftCornerOfTile map tileNumber
        |> Coordinate.toTileCenter map.sheet.tileSide


passableTile : Int -> Tile -> Maybe TileNumber
passableTile rawTileNumber tile =
    case tile of
        NoTile ->
            Just <| TileNumber <| rawTileNumber

        _ ->
            Nothing


passableTiles : Map -> List TileNumber
passableTiles { land } =
    land
        |> List.indexedMap passableTile
        |> List.filterMap identity


buildableTile : Int -> Tile -> Maybe TileNumber
buildableTile rawTileNumber tile =
    case tile of
        NoTile ->
            Nothing

        TileId _ ->
            Just <| TileNumber rawTileNumber


buildableTiles : Map -> List TileNumber
buildableTiles { land } =
    land
        |> List.indexedMap buildableTile
        |> List.filterMap identity


isBuildableTile : Map -> TileNumber -> Bool
isBuildableTile map tileNumber =
    List.member tileNumber (buildableTiles map)
