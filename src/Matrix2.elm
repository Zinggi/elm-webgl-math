module Matrix2 exposing (..)

{-|


## Matrix2

@docs Float2x2, Mat2


## General operations

@docs map, map2, foldl, foldr


## Math

@docs identity, fromRows, fromColumns

@docs add, sub, mul, elementWiseMul, mulByConst, transpose, mulVector


## Other

@docs almostEqual, maxNorm

-}

import Vector2 as V2 exposing (Float2, Vec2)


{-| -}
type alias Mat2 a =
    ( Vec2 a, Vec2 a )


{-| -}
type alias Float2x2 =
    Mat2 Float


{-|

    elementsSquared =
        map (\x -> x ^ 2)
-}
map : (a -> b) -> Mat2 a -> Mat2 b
map f =
    V2.map (V2.map f)


{-|

    elementWiseDivision =
        map2 (/)
-}
map2 : (a -> b -> c) -> Mat2 a -> Mat2 b -> Mat2 c
map2 f =
    V2.map2 (V2.map2 f)


{-| -}
foldl : (elem -> acc -> acc) -> acc -> Mat2 elem -> acc
foldl f init ( r1, r2 ) =
    V2.foldl f (V2.foldl f init r1) r2


{-| -}
foldr : (elem -> acc -> acc) -> acc -> Mat2 elem -> acc
foldr f init ( r1, r2 ) =
    V2.foldr f (V2.foldr f init r2) r1



-- Math


{-| The identity matrix.

    I = |1 0|
        |0 1|

    I*A = A = A*I

-}
identity : Float2x2
identity =
    ( ( 1, 0 )
    , ( 0, 1 )
    )


{-| Construct a matrix from rows.

    fromRows (1,2) (3,4) == ((1,2),(3,4))

-}
fromRows : Float2 -> Float2 -> Float2x2
fromRows a b =
    ( a, b )


{-| Construct a matrix from columns.

    fromColumns (1,2) (3,4) == ((1,3),(2,4))

-}
fromColumns : Float2 -> Float2 -> Float2x2
fromColumns ( a11, a21 ) ( a12, a22 ) =
    ( ( a11, a12 ), ( a21, a22 ) )


{-| Matrix addition.

    |a b|   |e f|   |a+e b+f|
    |c d| + |g h| = |c+g d+h|

-}
add : Float2x2 -> Float2x2 -> Float2x2
add =
    map2 (+)


{-| Matrix subtraction.

`A - B`

-}
sub : Float2x2 -> Float2x2 -> Float2x2
sub =
    map2 (-)


{-| Matrix multiplication.

`A*B`

-}
mul : Float2x2 -> Float2x2 -> Float2x2
mul ( ( a11, a12 ), ( a21, a22 ) ) ( ( b11, b12 ), ( b21, b22 ) ) =
    ( ( a11 * b11 + a12 * b21, a11 * b12 + a12 * b22 )
    , ( a21 * b11 + a22 * b21, a21 * b12 + a22 * b22 )
    )


{-| Element wise multiplication. Also called Hadamard product, Schur product or entrywise product.

    |a b|    |e f|   |ae bf|
    |c d| .* |g h| = |cg dh|

-}
elementWiseMul : Float2x2 -> Float2x2 -> Float2x2
elementWiseMul =
    map2 (*)


{-| `a*A`
Multiply a matrix by a constant
-}
mulByConst : Float -> Float2x2 -> Float2x2
mulByConst a ( ( a11, a12 ), ( a21, a22 ) ) =
    ( ( a * a11, a * a12 ), ( a * a21, a * a22 ) )



-- determinants are not in the scope of this library
--{-| The determinant.
--
--       |a b|
--    det|c d| = ad - bc
---}
--det : Float2x2 -> Float
--det ( ( a11, a12 ), ( a21, a22 ) ) =
--    a11 * a22 - a12 * a21


{-| The transpose.
Flips a matrix along it's diagonal.

    |a b|T  |a c|
    |c d| = |b d|

-}
transpose : Float2x2 -> Float2x2
transpose ( ( a11, a12 ), ( a21, a22 ) ) =
    ( ( a11, a21 )
    , ( a12, a22 )
    )


{-| Matrix-vector multiplication.

          |a b| |x|   |ax+by|
    A*v = |c d|*|y| = |cx+dy|

-}
mulVector : Float2x2 -> Float2 -> Float2
mulVector ( v1, v2 ) v =
    ( V2.dot v1 v, V2.dot v2 v )



-- Inverses are not in the scope of this library
--| The inverse.
--It's almost always a better idea to use `solve`.
--http://www.johndcook.com/blog/2010/01/19/dont-invert-that-matrix/
--    A^-1*A = I = A*A^-1
--inverse : Float2x2 -> Float2x2
--inverse (((a11,a12),(a21,a22)) as m) =
--    scale (1/(det m)) ((a22, -a12),(-a21,a11))
--
--solve ((a11, a12), (a21, a22)) (bx, by) =


{-| This checks whether `|A - B| < eps`.

    almostEqual eps a b

This is useful for testing, see the tests of this library for how this makes testing easy.

Since any definition of a norm can be used for this, it uses the simple `maxNorm`

-}
almostEqual : Float -> Float2x2 -> Float2x2 -> Bool
almostEqual eps a b =
    maxNorm (sub a b) <= eps


{-| The max norm. This is the biggest element of a matrix.
Useful for fuzz testing.
-}
maxNorm : Float2x2 -> Float
maxNorm =
    foldl (\elem acc -> max (abs elem) acc) 0
