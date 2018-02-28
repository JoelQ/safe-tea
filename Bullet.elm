module Bullet
    exposing
        ( Bullet
        , toEntity
        , fireTowards
        , fireFrom
        , move
        , impact
        , keepExploding
        , isDead
        )

import Coordinate
import Entity exposing (Entity)
import Time exposing (Time)


type Status
    = Flying
    | Exploding Time
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
    case bullet.status of
        Flying ->
            moveTheBullet diff bullet

        _ ->
            bullet


moveTheBullet : Time -> Bullet -> Bullet
moveTheBullet diff bullet =
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
    { bullet | status = Exploding 0 }


keepExploding : Time -> Bullet -> Bullet
keepExploding diff bullet =
    case bullet.status of
        Exploding timeSoFar ->
            if ceiling (timeSoFar / explosionSpeed) > 3 then
                { bullet | status = Impacted }
            else
                { bullet | status = Exploding (timeSoFar + diff) }

        _ ->
            bullet


isDead : Bullet -> Bool
isDead { position, target, status } =
    status == Impacted || position == target


speedPerSecond : number
speedPerSecond =
    300


toEntity : Bullet -> Entity
toEntity { position, status } =
    case status of
        Exploding diff ->
            if ceiling (diff / explosionSpeed) == 1 then
                explosionPhase1 position diff
            else if ceiling (diff / explosionSpeed) == 2 then
                explosionPhase2 position diff
            else
                explosionPhase3 position diff

        _ ->
            regularBulletEntity position


regularBulletEntity : Coordinate.Global -> Entity
regularBulletEntity position =
    { position = position
    , width = 10
    , height = 10
    , imagePath = "images/cannon-ball.png"
    }


explosionPhase1 : Coordinate.Global -> Time -> Entity
explosionPhase1 position timeSoFar =
    { position = position
    , width = 50
    , height = 50
    , imagePath = "images/explosion1.png"
    }


explosionPhase2 : Coordinate.Global -> Time -> Entity
explosionPhase2 position timeSoFar =
    { position = position
    , width = 75
    , height = 75
    , imagePath = "images/explosion2.png"
    }


explosionPhase3 : Coordinate.Global -> Time -> Entity
explosionPhase3 position timeSoFar =
    { position = position
    , width = 50
    , height = 50
    , imagePath = "images/explosion3.png"
    }


explosionSpeed : Time
explosionSpeed =
    Time.second / 20
