module Player exposing (Player, toEntity)

import Coordinate
import Entity exposing (Entity)


type alias Player =
    { position : Coordinate.Global
    }


toEntity : Player -> Entity
toEntity { position } =
    { position = position
    , width = 33
    , height = 56
    , imagePath = "../images/player-ship.png"
    }
