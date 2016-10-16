module Math.Float2 exposing (..)

{-|
-}


type alias Float2 =
    Vec2 Float


type alias Vec2 a =
    ( a, a )



-- set, get, map


getX ( x, _ ) =
    x


getY ( _, y ) =
    y


setX a ( x, y ) =
    ( a, y )


setY a ( x, y ) =
    ( x, a )


map : (a -> b) -> Vec2 a -> Vec2 b
map f ( x, y ) =
    ( f x, f y )


map2 : (a -> b -> c) -> Vec2 a -> Vec2 b -> Vec2 c
map2 op ( x1, y1 ) ( x2, y2 ) =
    ( op x1 x2, op y1 y2 )



-- math


add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


sub ( x1, y1 ) ( x2, y2 ) =
    ( x1 - x2, y1 - y2 )


negate ( x, y ) =
    ( -x, -y )


scale a ( x, y ) =
    ( a * x, a * y )


divideBy a ( x, y ) =
    ( x / a, y / a )


dot ( x1, y1 ) ( x2, y2 ) =
    x1 * x2 + y1 * y2


length v =
    sqrt (dot v v)


lengthSquared v =
    dot v v


normalize v =
    divideBy (length v) v


directionFromTo a b =
    normalize (sub b a)


distance a b =
    length (sub a b)


distanceSquared a b =
    lengthSquared (sub a b)


angle a b =
    let
        r =
            dot a b / (length a * length b)
    in
        if r >= 1 then
            0
        else
            acos r
