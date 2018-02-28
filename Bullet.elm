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
import Time exposing (Time)


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


move : Time -> Bullet -> Bullet
move diff bullet =
    let
        distanceToNextPoint =
            Coordinate.distance bullet.position bullet.target

        distanceInThisInterval =
            (Time.inSeconds diff) * speedPerSecond
    in
        if distanceToNextPoint > distanceInThisInterval then
            let
                thisTickG =
                    bullet.target
                        |> Coordinate.toLocal bullet.position
                        |> Coordinate.setMagnitude distanceInThisInterval
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


speedPerSecond : number
speedPerSecond =
    300


toEntity : Bullet -> Entity
toEntity { position } =
    { position = position
    , width = 10
    , height = 10
    , imagePath = "images/cannon-ball.png"
    }
