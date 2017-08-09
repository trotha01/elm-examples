module Collision exposing (Dimensions, Position, Shape(Circle, Rect), Velocity, collideList, collideWith, height, width)

import Math.Vector2 as Vec2 exposing (Vec2, getX, getY, vec2)
import Time exposing (Time)


type alias Object a =
    { a
        | position : Position
        , velocity : Velocity
        , inverseMass : Float
        , restitution : Float
        , shape : Shape
    }


type Shape
    = Circle Radius
    | Rect Dimensions


type alias Position =
    Vec2


type alias Velocity =
    Vec2


type alias Radius =
    Float


{-| Dimensions is stored as a width/height vector
-}
type alias Dimensions =
    Vec2


{-| helper function to get the width from Dimension
-}
width : Dimensions -> Float
width =
    Vec2.getX


{-| helper function to get the height from Dimension
-}
height : Dimensions -> Float
height =
    Vec2.getY


{-| run collision detection and resolition

  - between all the given list of objects

-}
collideList : List (Object a) -> List (Object a)
collideList objects =
    collideList_ [] objects


collideList_ : List (Object a) -> List (Object a) -> List (Object a)
collideList_ acc bodies =
    case bodies of
        [] ->
            acc

        h :: t ->
            case collideWith t h of
                ( h1, [] ) ->
                    -- (++) instead of (::) to keep the order of the list
                    acc ++ [ h1 ]

                ( h1, t1 ) ->
                    collideList_ (acc ++ [ h1 ]) t1


{-| Collide a single object with a list of objects,
modifying the single object and the list of objects along the way.
return (updated object, [updated objects])
-}
collideWith : List (Object a) -> Object a -> ( Object a, List (Object a) )
collideWith bodies a0 =
    collideWith_ bodies [] a0


collideWith_ : List (Object a) -> List (Object a) -> Object a -> ( Object a, List (Object a) )
collideWith_ bodies acc a0 =
    case bodies of
        [] ->
            ( a0, acc )

        b0 :: bs ->
            let
                collisionResult =
                    collide a0 b0

                ( a1, b1 ) =
                    resolveCollision collisionResult a0 b0
            in
            collideWith_ bs (acc ++ [ b1 ]) a1


type alias CollisionResult =
    { normal : Vec2, penetration : Float }


{-| collide two objects, returning a CollisionResult
-}
collide : Object a -> Object a -> CollisionResult
collide body0 body1 =
    case ( body0.shape, body1.shape ) of
        ( Circle b0, Circle b1 ) ->
            collideCircles ( body0.position, b0 ) ( body1.position, b1 )

        ( Rect b0Dimensions, Rect b1Dimensions ) ->
            collideBoxes ( body0.position, vec2Half b0Dimensions ) ( body1.position, vec2Half b1Dimensions )

        ( Rect dimensions, Circle radius ) ->
            collideRectWithCircle ( body0.position, vec2Half dimensions ) ( body1.position, radius )

        ( Circle radius, Rect dimensions ) ->
            -- negate the normal because the bodies were put in switched relative to their position in the list
            collideRectWithCircle ( body1.position, vec2Half dimensions ) ( body0.position, radius )
                |> negateNormal


{-| a helper function to half the dimensions
-}
vec2Half : Dimensions -> Dimensions
vec2Half =
    Vec2.scale 0.5


{-| a helper function to negate the normal vector in a collision
-}
negateNormal : CollisionResult -> CollisionResult
negateNormal collision =
    { collision | normal = Vec2.negate collision.normal }


{-| Calculate CollisionResult for two circles
-- takes position vector and radius for each circle
-}
collideCircles : ( Position, Radius ) -> ( Position, Radius ) -> CollisionResult
collideCircles ( pos0, radius0 ) ( pos1, radius1 ) =
    let
        b0b1 =
            Vec2.sub pos1 pos0

        radiusb0b1 =
            radius0 + radius1

        distanceSq =
            -- simple optimization: doesn't compute sqrt unless necessary
            Vec2.lengthSquared b0b1
    in
    if distanceSq == 0 then
        -- same position, arbitrary normal
        CollisionResult (vec2 1 0) radius0
    else if distanceSq >= radiusb0b1 * radiusb0b1 then
        -- no intersection, arbitrary normal
        CollisionResult (vec2 1 0) 0
    else
        let
            d =
                sqrt distanceSq
        in
        CollisionResult (Vec2.scale (1 / d) b0b1) (radiusb0b1 - d)


{-| -- collide two boxes
-- takes positions vector and half-lengths vectors of boxes
-}
collideBoxes : ( Vec2, Vec2 ) -> ( Vec2, Vec2 ) -> CollisionResult
collideBoxes ( pos0, extents0 ) ( pos1, extents1 ) =
    let
        dist =
            -- vector between box centerpoints
            Vec2.sub pos1 pos0

        ( nx, ny ) =
            Vec2.toTuple dist

        ( ox, oy ) =
            -- overlaps
            Vec2.add extents0 extents1
                |> Vec2.sub (abs2 dist)
                |> Vec2.toTuple
    in
    if ox > 0 && oy > 0 then
        if ox < oy then
            if nx < 0 then
                CollisionResult (vec2 -1 0) ox
            else
                CollisionResult (vec2 1 0) ox
        else if ny < 0 then
            CollisionResult (vec2 0 -1) oy
        else
            CollisionResult (vec2 0 1) oy
    else
        CollisionResult (vec2 1 0) 0


abs2 : Vec2 -> Vec2
abs2 v =
    vec2 (abs <| getX v) (abs <| getY v)


{-| collide a rectangle with a circle

  - takes position and half-length of rectangle, position and radius of circle

-}
collideRectWithCircle : ( Vec2, Vec2 ) -> ( Vec2, Float ) -> CollisionResult
collideRectWithCircle ( posBox, boxExtents ) ( posCircle, circleRadius ) =
    let
        dist =
            Vec2.sub posCircle posBox

        ( dx, dy ) =
            ( Vec2.getX dist, Vec2.getY dist )

        ( boxX, boxY ) =
            ( Vec2.getX boxExtents, Vec2.getY boxExtents )

        c =
            vec2 (clamp -boxX boxX dx) (clamp -boxY boxY dy)

        -- closest point on box to center of circle
        ( cx, cy ) =
            ( Vec2.getX c, Vec2.getY c )

        ( closest, inside ) =
            if
                --circle is outside
                dist /= c
            then
                ( c, False )
            else if
                -- circle is inside
                abs dx > abs dy
            then
                -- clamp center to closest edge
                if cx > 0 then
                    ( vec2 boxX cy, True )
                else
                    ( vec2 -boxX cy, True )
            else if cy > 0 then
                ( vec2 cx boxY, True )
            else
                ( vec2 cx -boxY, True )

        normal =
            Vec2.sub dist closest

        normalLenSq =
            Vec2.lengthSquared normal
    in
    if normalLenSq > circleRadius * circleRadius && not inside then
        CollisionResult (vec2 1 0) 0
    else
        let
            penetration =
                circleRadius + sqrt normalLenSq
        in
        if inside then
            CollisionResult (Vec2.scale -1 (Vec2.normalize normal)) penetration
        else
            CollisionResult (Vec2.normalize normal) penetration


{-| modify bodies' trajectories based off the colision result
-}
resolveCollision : CollisionResult -> Object a -> Object a -> ( Object a, Object a )
resolveCollision { normal, penetration } b0 b1 =
    let
        relativeVelocity =
            Vec2.sub b1.velocity b0.velocity

        velocityAlongNormal =
            Vec2.dot relativeVelocity normal
    in
    if penetration == 0 || velocityAlongNormal > 0 then
        ( b0, b1 )
        -- no collision or velocities separating
    else
        let
            restitution =
                -- collision restitution
                min b0.restitution b1.restitution

            invMassSum =
                b0.inverseMass + b1.inverseMass

            j =
                -- impulse scalar
                (-(1 + restitution) * velocityAlongNormal) / invMassSum

            impulse =
                -- impulse vector
                Vec2.scale j normal
        in
        ( { b0 | velocity = Vec2.sub b0.velocity (Vec2.scale b0.inverseMass impulse) }
        , { b1 | velocity = Vec2.add b1.velocity (Vec2.scale b1.inverseMass impulse) }
        )
