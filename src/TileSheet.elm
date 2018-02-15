module TileSheet exposing (TileSheet, kennyPirates)


type alias TileSheet =
    { sheetWidth : Int
    , tileSide : Int
    , source : String
    }


kennyPirates : TileSheet
kennyPirates =
    { sheetWidth = 16
    , tileSide = 64
    , source = "../images/tiles_sheet.png"
    }
