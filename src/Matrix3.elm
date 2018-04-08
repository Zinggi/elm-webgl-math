module Matrix3 exposing (..)

{-|


## Matrix3

This will have some more functions soon.
Currently it's missing 2D transformation functions.

@docs Mat3, Float3x3

@docs map, map2

@docs identity, add, sub, mul, mulVector, transpose, elementWiseMul

@docs transform

-}

import Vector3 as V3 exposing (Float3, Vec3)
import Vector2 as V2 exposing (Float2, Vec2)


{-| -}
type alias Mat3 a =
    ( Vec3 a, Vec3 a, Vec3 a )


{-| -}
type alias Float3x3 =
    Mat3 Float


{-| The identity matrix

    I = |1 0 0|
        |0 1 0|
        |0 0 1|

-}
identity : Float3x3
identity =
    ( ( 1, 0, 0 )
    , ( 0, 1, 0 )
    , ( 0, 0, 1 )
    )


{-| -}
map : (a -> b) -> Mat3 a -> Mat3 b
map f =
    V3.map (V3.map f)


{-| -}
map2 : (a -> b -> c) -> Mat3 a -> Mat3 b -> Mat3 c
map2 f =
    V3.map2 (V3.map2 f)


{-| `A + B`
-}
add : Float3x3 -> Float3x3 -> Float3x3
add =
    map2 (+)


{-| `A - B`
-}
sub : Float3x3 -> Float3x3 -> Float3x3
sub =
    map2 (-)


{-| `A .* B`
-}
elementWiseMul : Float3x3 -> Float3x3 -> Float3x3
elementWiseMul =
    map2 (*)


{-| `A*B`
-}
mul : Float3x3 -> Float3x3 -> Float3x3
mul ( ( a11, a12, a13 ), ( a21, a22, a23 ), ( a31, a32, a33 ) ) ( ( b11, b12, b13 ), ( b21, b22, b23 ), ( b31, b32, b33 ) ) =
    ( ( a11 * b11 + a12 * b21 + a13 * b31, a11 * b12 + a12 * b22 + a13 * b32, a11 * b13 + a12 * b23 + a13 * b33 )
    , ( a21 * b11 + a22 * b21 + a23 * b31, a21 * b12 + a22 * b22 + a23 * b32, a21 * b13 + a22 * b23 + a23 * b33 )
    , ( a31 * b11 + a32 * b21 + a33 * b31, a31 * b12 + a32 * b22 + a33 * b32, a31 * b13 + a32 * b23 + a33 * b33 )
    )


{-| `A^T`
-}
transpose : Float3x3 -> Float3x3
transpose ( ( a11, a12, a13 ), ( a21, a22, a23 ), ( a31, a32, a33 ) ) =
    ( ( a11, a21, a31 )
    , ( a12, a22, a32 )
    , ( a13, a23, a33 )
    )



-- Not in this library
--
--det : Float3x3 -> Float
--det ( ( a11, a12, a13 ), ( a21, a22, a23 ), ( a31, a32, a33 ) ) =
--    a11 * (a22 * a33 - a23 * a32) - a12 * (a21 * a33 - a23 * a31) + a13 * (a21 * a32 - a22 * a31)


{-| `A*v`
-}
mulVector : Float3x3 -> Float3 -> Float3
mulVector ( v1, v2, v3 ) v =
    ( V3.dot v1 v, V3.dot v2 v, V3.dot v3 v )


{-|

    |v'| = A*|v|  v'/w
    |w |     |1|,

NaN/infinity warning: if w = 0

-}
transform : Float3x3 -> Float2 -> Float2
transform m ( x, y ) =
    let
        ( u, v, w ) =
            mulVector m ( x, y, 1 )
    in
        ( u / w, v / w )
