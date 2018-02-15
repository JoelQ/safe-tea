module Main exposing (main)

import Html exposing (Html, text)
import Map
import Element


type Game
    = Game


type Msg
    = Start
    | End


update : Msg -> Game -> Game
update msg game =
    game


view : Game -> Html Msg
view game =
    Map.level1
        |> Map.render
        |> Element.toHtml


main : Program Never Game Msg
main =
    Html.beginnerProgram
        { model = Game
        , update = update
        , view = view
        }
