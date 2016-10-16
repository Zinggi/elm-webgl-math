module Math.Float4 exposing (..)

{-|
-}


type alias Float4 =
    Vec4 Float


type alias Vec4 a =
    ( a, a, a, a )



-- set, get, map


fromV3 ( x, y, z ) w =
    ( x, y, z, w )


getX ( x, _, _, _ ) =
    x


getY ( _, y, _, _ ) =
    y


getZ ( _, _, z, _ ) =
    z


getW ( _, _, _, w ) =
    w


setX a ( x, y, z, w ) =
    ( a, y, z, w )


setY a ( x, y, z, w ) =
    ( x, a, z, w )


setZ a ( x, y, z, w ) =
    ( x, y, a, w )


setW a ( x, y, z, w ) =
    ( x, y, z, a )


map f ( x, y, z, w ) =
    ( f x, f y, f z, f w )


map2 : (a -> b -> c) -> Vec4 a -> Vec4 b -> Vec4 c
map2 f ( x1, y1, z1, w1 ) ( x2, y2, z2, w2 ) =
    ( f x1 x2, f y1 y2, f z1 z2, f w1 w2 )



-- math


add ( x1, y1, z1, w1 ) ( x2, y2, z2, w2 ) =
    ( x1 + x2, y1 + y2, z1 + z2, w1 + w2 )


sub ( x1, y1, z1, w1 ) ( x2, y2, z2, w2 ) =
    ( x1 - x2, y1 - y2, z1 - z2, w1 - w2 )


negate ( x, y, z, w ) =
    ( -x, -y, -z, -w )


scale a ( x, y, z, w ) =
    ( a * x, a * y, a * z, a * w )


divideBy a ( x, y, z, w ) =
    ( x / a, y / a, z / a, w / a )


dot ( x1, y1, z1, w1 ) ( x2, y2, z2, w2 ) =
    x1 * x2 + y1 * y2 + z1 * z2 + w1 * w2


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
