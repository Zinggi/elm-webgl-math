module Math.Float3x3 exposing (..)

import Math.Float3 as V3 exposing (Float3, Vec3)


type alias Float3x3 =
    Mat3x3 Float


type alias Mat3x3 a =
    ( Vec3 a, Vec3 a, Vec3 a )


identity =
    ( ( 1, 0, 0 )
    , ( 0, 1, 0 )
    , ( 0, 0, 1 )
    )


map f =
    V3.map (V3.map f)


map2 f =
    V3.map2 (V3.map2 f)


add =
    map2 (+)


sub =
    map2 (-)


mul ( ( a11, a12, a13 ), ( a21, a22, a23 ), ( a31, a32, a33 ) ) ( ( b11, b12, b13 ), ( b21, b22, b23 ), ( b31, b32, b33 ) ) =
    ( ( a11 * b11 + a12 * b21 + a13 * b31, a11 * b12 + a12 * b22 + a13 * b32, a11 * b13 + a12 * b23 + a13 * b33 )
    , ( a21 * b11 + a22 * b21 + a23 * b31, a21 * b12 + a22 * b22 + a23 * b32, a21 * b13 + a22 * b23 + a23 * b33 )
    , ( a31 * b11 + a32 * b21 + a33 * b31, a31 * b12 + a32 * b22 + a33 * b32, a31 * b13 + a32 * b23 + a33 * b33 )
    )


transpose ( ( a11, a12, a13 ), ( a21, a22, a23 ), ( a31, a32, a33 ) ) =
    ( ( a11, a21, a31 )
    , ( a12, a22, a32 )
    , ( a13, a23, a33 )
    )


transform ( v1, v2, v3 ) v =
    ( V3.dot v1 v, V3.dot v2 v, V3.dot v3 v )
