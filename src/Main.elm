module Main exposing (main)

import Html exposing (Html, text)
import Map exposing (Map)
import Element exposing (Element)
import Pirate exposing (Pirate)
import Entity exposing (Entity)


type alias Game =
    { map : Map
    , pirates : List Pirate
    , playerShip : Entity
    }


player : Entity
player =
    { x = 416
    , y = 576
    , width = 33
    , height = 56
    , imagePath = "../images/player-ship.png"
    }


pirate1 : Pirate
pirate1 =
    { x = 896
    , y = 544
    }


pirate2 : Pirate
pirate2 =
    { x = 640
    , y = 896
    }


initialGame : Game
initialGame =
    { map = Map.level1
    , pirates = [ pirate1, pirate2 ]
    , playerShip = player
    }


type Msg
    = Start
    | End


update : Msg -> Game -> Game
update msg game =
    game


view : Game -> Html Msg
view game =
    [ Map.render Map.level1
    , Entity.renderList <| List.map Pirate.toEntity game.pirates
    , Entity.render game.playerShip
    ]
        |> Element.layers
        |> Element.toHtml


main : Program Never Game Msg
main =
    Html.beginnerProgram
        { model = initialGame
        , update = update
        , view = view
        }
