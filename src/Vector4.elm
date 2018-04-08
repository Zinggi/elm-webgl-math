module Vector4 exposing (..)

{-|


## Vector4

@docs Float4, Vec4

@docs fromV3, setX, setY, setZ, setW, getX, getY, getZ, getW, map, map2, foldl, foldr

@docs add, sub, negate, scale, divideBy

@docs dot, length, lengthSquared, normalize, directionFromTo, distance, distanceSquared, angle

-}

import Vector3 exposing (Vec3)


{-| -}
type alias Vec4 a =
    ( a, a, a, a )


{-| -}
type alias Float4 =
    Vec4 Float



-- set, get, map


{-|

    fromV3 (1,2,3) 1 == (1,2,3,1)
-}
fromV3 : Vec3 a -> a -> Vec4 a
fromV3 ( x, y, z ) w =
    ( x, y, z, w )


{-| -}
getX : Vec4 a -> a
getX ( x, _, _, _ ) =
    x


{-| -}
getY : Vec4 a -> a
getY ( _, y, _, _ ) =
    y


{-| -}
getZ : Vec4 a -> a
getZ ( _, _, z, _ ) =
    z


{-| -}
getW : Vec4 a -> a
getW ( _, _, _, w ) =
    w


{-| -}
setX : a -> Vec4 a -> Vec4 a
setX a ( x, y, z, w ) =
    ( a, y, z, w )


{-| -}
setY : a -> Vec4 a -> Vec4 a
setY a ( x, y, z, w ) =
    ( x, a, z, w )


{-| -}
setZ : a -> Vec4 a -> Vec4 a
setZ a ( x, y, z, w ) =
    ( x, y, a, w )


{-| -}
setW : a -> Vec4 a -> Vec4 a
setW a ( x, y, z, w ) =
    ( x, y, z, a )


{-|

    map (\x -> x^2) (1,2,3,4) == (1,4,9,16)
-}
map : (a -> b) -> Vec4 a -> Vec4 b
map f ( x, y, z, w ) =
    ( f x, f y, f z, f w )


{-|

    map2 (<) (2,1,4,2) (3,2,1,6) == (True, True, False, True)
-}
map2 : (a -> b -> c) -> Vec4 a -> Vec4 b -> Vec4 c
map2 f ( x1, y1, z1, w1 ) ( x2, y2, z2, w2 ) =
    ( f x1 x2, f y1 y2, f z1 z2, f w1 w2 )


{-|

    foldl (\elem acc -> acc + elem^2) 0 (2,4,1,2) == 25
-}
foldl : (elem -> acc -> acc) -> acc -> Vec4 elem -> acc
foldl f start ( x, y, z, w ) =
    f w (f z (f y (f x start)))


{-|

    foldr (::) [] (1,2,3,5) == [1,2,3,5]
-}
foldr : (elem -> acc -> acc) -> acc -> Vec4 elem -> acc
foldr f start ( x, y, z, w ) =
    f x (f y (f z (f w start)))



-- math


{-| `v + w`

    add (2,4,1,-2) (3,-6,2,1) == (5,-2,3,-1)

-}
add : Float4 -> Float4 -> Float4
add ( x1, y1, z1, w1 ) ( x2, y2, z2, w2 ) =
    ( x1 + x2, y1 + y2, z1 + z2, w1 + w2 )


{-| `v - w`

    sub (4,6,1,2) (3,-1,-4,4) == (1,7,5,-2)

-}
sub : Float4 -> Float4 -> Float4
sub ( x1, y1, z1, w1 ) ( x2, y2, z2, w2 ) =
    ( x1 - x2, y1 - y2, z1 - z2, w1 - w2 )


{-| `-v`

    negate (2,-1,5,1) == (-2,1,-5,-1)

-}
negate : Float4 -> Float4
negate ( x, y, z, w ) =
    ( -x, -y, -z, -w )


{-| `a*v`

    scale (3/2) (4,2,6,10) == (6,3,9,15)

-}
scale : Float -> Float4 -> Float4
scale a ( x, y, z, w ) =
    ( a * x, a * y, a * z, a * w )


{-| `v/a`

    divideBy (3/2) (3,12,6,9) == (2,8,4,6)

NaN/infinity warning: if a = 0

-}
divideBy : Float -> Float4 -> Float4
divideBy a ( x, y, z, w ) =
    ( x / a, y / a, z / a, w / a )


{-| `v dot w`

The **dot product** of two vectors. Also called **scalar product** or **inner product**.

It links the length and angle of two vectors.

`v dot w = |v|*|w|*cos(phi)`

    dot (1,2,2,3) (3,3,2,2) == 1*3 + 2*3 + 2*2 + 3*2 == 19

-}
dot : Float4 -> Float4 -> Float
dot ( x1, y1, z1, w1 ) ( x2, y2, z2, w2 ) =
    x1 * x2 + y1 * y2 + z1 * z2 + w1 * w2


{-| The length of a vector. Also known as magnitude or norm.

`|v| = sqrt(v dot v)`

    length (2,4,1,2) == sqrt (2^2+4^2+1^2+2^2) == 5

-}
length : Float4 -> Float
length v =
    sqrt (dot v v)


{-| The squared length. This is cheaper to calculate,
so if you only need to compare lengths you can use this instead of the length.

`|v|^2 = v dot w`

    lengthSquared (3,4,1,2) == 3^2+4^2+1^2+2^2 == 30

-}
lengthSquared : Float4 -> Float
lengthSquared v =
    dot v v


{-| Normalizes a vector. This will give you a unit vector (e.g. with length 1) in the same direction as `v`.

`v/|v|`

    normalize (2,4,1,2) == (2/5,4/5,1/5,2/5)

NaN warning: if v = 0

-}
normalize : Float4 -> Float4
normalize v =
    divideBy (length v) v


{-| A unit vector pointing from `v` to `w`

`(w - v)/|w - v|`

    directionFromTo (5,1,2,4) (7,5,3,6) == (2/5,4/5,1/5,2/5)

NaN warning: if v = w

-}
directionFromTo : Float4 -> Float4 -> Float4
directionFromTo a b =
    normalize (sub b a)


{-| Calculates the distance from `v` to `w`.

`|v - w| = |w - v|`

    distance (7,5,3,6) (5,1,2,4) == 5

-}
distance : Float4 -> Float4 -> Float
distance a b =
    length (sub a b)


{-| The squared distance. This is slightly faster.

`|v - w|^2`

    distanceSquared (3,0,2,1) (0,2,4,1) == 17

-}
distanceSquared : Float4 -> Float4 -> Float
distanceSquared a b =
    lengthSquared (sub a b)


{-| The angle between two vectors. The angle is in radians.

`acos((v dot w)/(|v|*|w|))`

    angle (-1,-1,2,0) (2,2,2,0) == pi/2    -- or 90°

NaN warning: if v = 0 or w = 0

-}
angle : Float4 -> Float4 -> Float
angle a b =
    let
        r =
            dot a b / (length a * length b)
    in
        if r >= 1 then
            0
        else
            acos r
