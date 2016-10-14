module Math.Float2x2 exposing (..)

import Math.Float2 as V2 exposing (Float2, Vec2)


type alias Float2x2 =
    Mat2x2 Float


type alias Mat2x2 a =
    ( Vec2 a, Vec2 a )


identity =
    ( ( 1, 0 )
    , ( 0, 1 )
    )


mul ( ( a11, a12 ), ( a21, a22 ) ) ( ( b11, b12 ), ( b21, b22 ) ) =
    ( ( a11 * b11 + a12 * b21, a11 * b12 + a12 * b22 )
    , ( a21 * b11 + a22 * b21, a21 * b12 + a22 * b22 )
    )


map : (a -> b) -> Mat2x2 a -> Mat2x2 b
map f =
    V2.map (V2.map f)


map2 : (a -> b -> c) -> Mat2x2 a -> Mat2x2 b -> Mat2x2 c
map2 f =
    V2.map2 (V2.map2 f)


add =
    map2 (+)


sub =
    map2 (-)


transpose ( ( a11, a12 ), ( a21, a22 ) ) =
    ( ( a11, a21 )
    , ( a12, a22 )
    )


transform ( v1, v2 ) v =
    ( V2.dot v1 v, V2.dot v2 v )
