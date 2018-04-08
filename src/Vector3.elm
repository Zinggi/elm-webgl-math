module Vector3 exposing (..)

{-|


## Vector3

@docs Float3, Vec3

@docs fromV2, setX, setY, setZ, getX, getY, getZ, map, map2, foldl, foldr

@docs add, sub, negate, scale, divideBy

@docs dot, cross, length, lengthSquared, normalize, directionFromTo, distance, distanceSquared, angle, project, reject

-}

import Vector2 exposing (Vec2)


{-| -}
type alias Vec3 a =
    ( a, a, a )


{-| -}
type alias Float3 =
    Vec3 Float



-- set, get, map


{-|

    fromV2 (1,2) 3 == (1,2,3)
-}
fromV2 : Vec2 a -> a -> Vec3 a
fromV2 ( x, y ) z =
    ( x, y, z )


{-| -}
getX : Vec3 a -> a
getX ( x, _, _ ) =
    x


{-| -}
getY : Vec3 a -> a
getY ( _, y, _ ) =
    y


{-| -}
getZ : Vec3 a -> a
getZ ( _, _, z ) =
    z


{-| -}
setX : a -> Vec3 a -> Vec3 a
setX a ( x, y, z ) =
    ( a, y, z )


{-| -}
setY : a -> Vec3 a -> Vec3 a
setY a ( x, y, z ) =
    ( x, a, z )


{-| -}
setZ : a -> Vec3 a -> Vec3 a
setZ a ( x, y, z ) =
    ( x, y, a )


{-|

    map sqrt (1,4,9) == (1,2,3)
-}
map : (a -> b) -> Vec3 a -> Vec3 b
map f ( x, y, z ) =
    ( f x, f y, f z )


{-|

    map2 (/) (4,9,12) (2,3,4) == (2,3,3)
-}
map2 : (a -> b -> c) -> Vec3 a -> Vec3 b -> Vec3 c
map2 f ( x1, y1, z1 ) ( x2, y2, z2 ) =
    ( f x1 x2, f y1 y2, f z1 z2 )


{-|

    foldl (*) 1 (2,4,1) == 8
-}
foldl : (elem -> acc -> acc) -> acc -> Vec3 elem -> acc
foldl f start ( x, y, z ) =
    f z (f y (f x start))


{-|

    foldr max 0 (1,12,-5) == 12
-}
foldr : (elem -> acc -> acc) -> acc -> Vec3 elem -> acc
foldr f start ( x, y, z ) =
    f x (f y (f z start))



-- math


{-| `v + w`

    add (2,4,1) (3,-6,2) == (5,-2,3)

-}
add : Float3 -> Float3 -> Float3
add ( x1, y1, z1 ) ( x2, y2, z2 ) =
    ( x1 + x2, y1 + y2, z1 + z2 )


{-| `v - w`

    sub (4,6,1) (3,-1,-4) == (1,7,5)

-}
sub : Float3 -> Float3 -> Float3
sub ( x1, y1, z1 ) ( x2, y2, z2 ) =
    ( x1 - x2, y1 - y2, z1 - z2 )


{-| `-v`

    negate (2,-1,5) == (-2,1,-5)

-}
negate : Float3 -> Float3
negate ( x, y, z ) =
    ( -x, -y, -z )


{-| `a*v`

    scale (1/2) (4,2,6) == (2,1,3)

-}
scale : Float -> Float3 -> Float3
scale a ( x, y, z ) =
    ( a * x, a * y, a * z )


{-| `v/a`

    divideBy (1/2) (2,1,3) == (4,2,6)

NaN/infinity warning: if a = 0

-}
divideBy : Float -> Float3 -> Float3
divideBy a ( x, y, z ) =
    ( x / a, y / a, z / a )


{-| `v dot w`

The **dot product** of two vectors. Also called **scalar product** or **inner product**.

It links the length and angle of two vectors.

`v dot w = |v|*|w|*cos(phi)`

    dot (1,2,2) (3,3,2) == 1*3 + 2*3 + 2*2 == 13

-}
dot : Float3 -> Float3 -> Float
dot ( x1, y1, z1 ) ( x2, y2, z2 ) =
    x1 * x2 + y1 * y2 + z1 * z2


{-| `v cross w`

The **cross product** of two vectors. Also called **vector product**.

`v cross w` is a vector that is perpendicular to both `v` and `w`
and therefore normal to the plane containing them.

The length of `v cross w` is equal to the area of the parallelogram spanned by `v` and `w`.

`|v cross w| = |v|*|w|*sin(phi)`

    cross (2,1,3) (4,5,-3) == (1*(-3) - 3*5, 3*4 - 2*(-3), 2*5 - 1*4) == (-18, 18, 6)

-}
cross : Float3 -> Float3 -> Float3
cross ( x1, y1, z1 ) ( x2, y2, z2 ) =
    ( y1 * z2 - z1 * y2, z1 * x2 - x1 * z2, x1 * y2 - y1 * x2 )


{-| The length of a vector. Also known as magnitude or norm.

`|v| = sqrt(v dot v)`

    length (4,2,4) == sqrt (4^2+2^2+4^2) == 6

-}
length : Float3 -> Float
length v =
    sqrt (dot v v)


{-| The squared length. This is cheaper to calculate,
so if you only need to compare lengths you can use this instead of the length.

`|v|^2 = v dot w`

    lengthSquared (3,4,1) == 3^2+4^2+1^2 == 26

-}
lengthSquared : Float3 -> Float
lengthSquared v =
    dot v v


{-| Normalizes a vector. This will give you a unit vector (e.g. with length 1) in the same direction as `v`.

`v/|v|`

    normalize (4,2,4) == (2/3,1/3,2/3)

NaN warning: if v = 0

-}
normalize : Float3 -> Float3
normalize v =
    divideBy (length v) v


{-| The projection of `v` onto `w`.

`(v dot w)/|w| * w/|w|`

    project (2,1,0) (4,0,0) == (2,0,0)

NaN warning: if w = 0

-}
project : Float3 -> Float3 -> Float3
project v w =
    let
        l_w =
            lengthSquared w
    in
        scale ((dot v w) / l_w) w


{-| The rejection of `v` onto `w`. This is always perpendicular to the projection.

`v - (project v w)`

    reject (2,1,0) (4,0,0) == (0,1,0)

NaN warning: if w = 0

-}
reject : Float3 -> Float3 -> Float3
reject v w =
    sub v (project v w)


{-| A unit vector pointing from `v` to `w`

`(w - v)/|w - v|`

    directionFromTo (5,1,2) (9,3,6) == (2/3,1/3,2/3)

NaN warning: if v = w

-}
directionFromTo : Float3 -> Float3 -> Float3
directionFromTo a b =
    normalize (sub b a)


{-| Calculates the distance from `v` to `w`.

`|v - w| = |w - v|`

    distance (2,0,4) (0,4,0) == 6

-}
distance : Float3 -> Float3 -> Float
distance a b =
    length (sub a b)


{-| The squared distance. This is slightly faster.

`|v - w|^2`

    distanceSquared (3,0,2) (0,4,1) == 26

-}
distanceSquared : Float3 -> Float3 -> Float
distanceSquared a b =
    lengthSquared (sub a b)


{-| The angle between two vectors. The angle is in radians.

`acos((v dot w)/(|v|*|w|))`

    angle (-1,-1,2) (2,2,2) == pi/2    -- or 90°

NaN warning: if v = 0 or w = 0

-}
angle : Float3 -> Float3 -> Float
angle a b =
    let
        r =
            dot a b / (length a * length b)
    in
        if r >= 1 then
            0
        else
            acos r
