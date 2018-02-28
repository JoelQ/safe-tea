module Bullet
    exposing
        ( Bullet
        , toEntity
        , fireTowards
        , fireFrom
        , move
        , impact
        , isDead
        )

import Coordinate
import Entity exposing (Entity)


type Status
    = Flying
    | Impacted


type alias Bullet =
    { position : Coordinate.Global
    , target : Coordinate.Global
    , status : Status
    }


fireFrom : Coordinate.Global -> Bullet
fireFrom position =
    { position = position
    , target = position
    , status = Flying
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


impact : Bullet -> Bullet
impact bullet =
    { bullet | status = Impacted }


isDead : Bullet -> Bool
isDead { position, target, status } =
    status == Impacted || position == target


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
