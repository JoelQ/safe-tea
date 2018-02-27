module Bullet exposing (Bullet, toEntity, fireTowards, fireFrom, move)

import Coordinate
import Entity exposing (Entity)


type alias Bullet =
    { position : Coordinate.Global
    , target : Coordinate.Global
    }


fireFrom : Coordinate.Global -> Bullet
fireFrom position =
    { position = position
    , target = position
    }


fireTowards : Coordinate.Global -> Bullet -> Bullet
fireTowards targetPosition bullet =
    { bullet | target = targetPosition }


move : Bullet -> Bullet
move bullet =
    let
        distanceToNextPoint =
            Coordinate.distance bullet.position bullet.target
    in
        if distanceToNextPoint > speed then
            let
                thisTickG =
                    bullet.target
                        |> Coordinate.toLocal bullet.position
                        |> Coordinate.setMagnitude speed
                        |> Coordinate.toGlobal bullet.position
            in
                { bullet | position = thisTickG }
        else
            { bullet | position = bullet.target }


speed : Int
speed =
    10


toEntity : Bullet -> Entity
toEntity { position } =
    { position = position
    , width = 10
    , height = 10
    , imagePath = "../images/cannon-ball.png"
    }
