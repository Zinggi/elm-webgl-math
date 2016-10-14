module Math.Float3 exposing (..)

{-|
-}


type alias Float3 =
    Vec3 Float


type alias Vec3 a =
    ( a, a, a )



-- set, get, map


fromV2 ( x, y ) z =
    ( x, y, z )


getX ( x, _, _ ) =
    x


getY ( _, y, _ ) =
    y


getZ ( _, _, z ) =
    z


setX a ( x, y, z ) =
    ( a, y, z )


setY a ( x, y, z ) =
    ( x, a, z )


setZ a ( x, y, z ) =
    ( x, y, a )


map f ( x, y, z ) =
    ( f x, f y, f z )


map2 : (a -> b -> c) -> Vec3 a -> Vec3 b -> Vec3 c
map2 f ( x1, y1, z1 ) ( x2, y2, z2 ) =
    ( f x1 x2, f y1 y2, f z1 z2 )



-- math


add ( x1, y1, z1 ) ( x2, y2, z2 ) =
    ( x1 + x2, y1 + y2, z1 + z2 )


sub ( x1, y1, z1 ) ( x2, y2, z2 ) =
    ( x1 - x2, y1 - y2, z1 - z2 )


negate ( x, y, z ) =
    ( -x, -y, -z )


scale a ( x, y, z ) =
    ( a * x, a * y, a * z )


divideBy a ( x, y, z ) =
    ( x / a, y / a, z / a )


dot ( x1, y1, z1 ) ( x2, y2, z2 ) =
    x1 * x2 + y1 * y2 + z1 * z2


cross ( x1, y1, z1 ) ( x2, y2, z2 ) =
    ( y1 * z2 - z1 * y2, z1 * x2 - x1 * z2, x1 * y2 - y1 * x2 )


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
