module Matrix4 exposing (..)

{-|


## Matrix4

This module deals mostly with 3D transformation matrices.

We can represent a transformation of a 3D vector as a multiplication with a 4x4 matrix
by writing the vector in homogeneous coordinates.

Here are a few useful links if you want to understand the math behind this library.

<http://learnopengl.com/#!Getting-started/Transformations>
<http://www.codinglabs.net/article_world_view_projection_matrix.aspx>
<http://www.euclideanspace.com/maths/geometry/affine/index.htm>

@docs Float4x4, Mat4


## General operations

@docs map, map2, foldl, foldr


## General math

@docs identity, fromRows, fromColumns

@docs add, sub, mul, elementWiseMul, mulByConst, transpose, mulVector


## Transformation matrices

A transformation matrix represents an arbitrary transform on a 3d vector.
To transform a 3d vector v, we multiply it with a 4x4 transformation matrix M.
To do that we need to write v in homogeneous coordinates,
then transform the result back to normal coordinates.

        |v|   |v'|
    M * |1| = |w |,
    res = v'/w

These transformations can be chained, e.g. you can apply several transformations in series.
Note that to understand how a vector will be transformed, it helps to read it backwards:

    T*R*S*v

Means that `v` will be scaled by the matrix `S`, then rotated by `R` and finally translated by `T`.

@docs transform


### Affine transformations

These represent a transform that preserves shapes, e.g. translations, rotations and scaling.
These are composed of a 3x3 rotation and scale matrix M and a translation vector t:

    |M t|
    |0 1|

@docs makeRotate, makeScale, makeTranslate, makeTransform
@docs rotate, scale, translate, transformBy

TODO: does it make sense for these to be post multiplied?
This means that currently these operations will be done before doing the transform,
meaning they will act as local transformations.
However this makes creating a composed transformation weird,
as the order of operations might be counter-intuitive!
E.g. `identity |> translate |> rotate` actually corresponds to `I*T*R`
which means rotate first, then translate.

@docs makeLookAt, makeBasis


#### Operations on affine transforms

These can speed up some calculations, but are only correct if actually used with affine transforms.

@docs transformAffine, mulAffine, inverseRigidBodyTransform


### Cameras

Cameras can also be represented as a transformation matrix.
**NOTE**: These transforms are generally **not** affine transforms!

Camera projection matrices map their view of the scene into a 2x2x2 cube.
Math heavy reference: <http://www.songho.ca/opengl/gl_projectionmatrix.html>

@docs makeFrustum, makePerspective, makeOrtho, makeOrtho2d


## Other

@docs maxNorm, almostEqual

-}

import Vector4 as V4 exposing (Float4, Vec4)
import Vector3 as V3 exposing (Float3, Vec3)


-- Useful references:
-- https://github.com/mrdoob/three.js/blob/master/src/math/Matrix4.js
--
-- NOTE: This library uses row-major order,
-- the elm-community/elm-linear-algebra one uses column-major order!


{-| -}
type alias Mat4 a =
    ( Vec4 a, Vec4 a, Vec4 a, Vec4 a )


{-| -}
type alias Float4x4 =
    Mat4 Float



-- general purpose


{-| -}
map : (a -> b) -> Mat4 a -> Mat4 b
map f =
    V4.map (V4.map f)


{-| -}
map2 : (a -> b -> c) -> Mat4 a -> Mat4 b -> Mat4 c
map2 f =
    V4.map2 (V4.map2 f)


{-| -}
foldl : (elem -> acc -> acc) -> acc -> Mat4 elem -> acc
foldl f init ( m1, m2, m3, m4 ) =
    (V4.foldl f (V4.foldl f (V4.foldl f (V4.foldl f init m1) m2) m3) m4)


{-| -}
foldr : (elem -> acc -> acc) -> acc -> Mat4 elem -> acc
foldr f init ( m1, m2, m3, m4 ) =
    V4.foldr f (V4.foldr f (V4.foldr f (V4.foldr f init m4) m3) m2) m1



-- Math


{-| The identity matrix. This is also a valid transformation matrix which doesn't do anything.

    I = |1 0 0 0|
        |0 1 0 0|
        |0 0 1 0|
        |0 0 0 1|

-}
identity : Float4x4
identity =
    ( ( 1, 0, 0, 0 )
    , ( 0, 1, 0, 0 )
    , ( 0, 0, 1, 0 )
    , ( 0, 0, 0, 1 )
    )


{-|

    fromRows a b c d == (a,b,c,d)
-}
fromRows : Float4 -> Float4 -> Float4 -> Float4 -> Float4x4
fromRows a b c d =
    ( a, b, c, d )


{-|

    fromColumns a b c d == transpose (a,b,c,d)
-}
fromColumns : Float4 -> Float4 -> Float4 -> Float4 -> Float4x4
fromColumns a b c d =
    transpose ( a, b, c, d )


{-| `A + B`
-}
add : Float4x4 -> Float4x4 -> Float4x4
add =
    map2 (+)


{-| `A - B`
-}
sub : Float4x4 -> Float4x4 -> Float4x4
sub =
    map2 (-)


{-| Element wise multiplication. Also called Hadamard product, Schur product or entrywise product.

    A .* B

-}
elementWiseMul : Float4x4 -> Float4x4 -> Float4x4
elementWiseMul =
    map2 (*)


{-| `a*A`
-}
mulByConst : Float -> Float4x4 -> Float4x4
mulByConst a ( r1, r2, r3, r4 ) =
    ( V4.scale a r1, V4.scale a r2, V4.scale a r3, V4.scale a r4 )


{-| `A*B`
-}
mul : Float4x4 -> Float4x4 -> Float4x4
mul ( ( a11, a12, a13, a14 ), ( a21, a22, a23, a24 ), ( a31, a32, a33, a34 ), ( a41, a42, a43, a44 ) ) ( ( b11, b12, b13, b14 ), ( b21, b22, b23, b24 ), ( b31, b32, b33, b34 ), ( b41, b42, b43, b44 ) ) =
    ( ( a11 * b11 + a12 * b21 + a13 * b31 + a14 * b41
      , a11 * b12 + a12 * b22 + a13 * b32 + a14 * b42
      , a11 * b13 + a12 * b23 + a13 * b33 + a14 * b43
      , a11 * b14 + a12 * b24 + a13 * b34 + a14 * b44
      )
    , ( a21 * b11 + a22 * b21 + a23 * b31 + a24 * b41
      , a21 * b12 + a22 * b22 + a23 * b32 + a24 * b42
      , a21 * b13 + a22 * b23 + a23 * b33 + a24 * b43
      , a21 * b14 + a22 * b24 + a23 * b34 + a24 * b44
      )
    , ( a31 * b11 + a32 * b21 + a33 * b31 + a34 * b41
      , a31 * b12 + a32 * b22 + a33 * b32 + a34 * b42
      , a31 * b13 + a32 * b23 + a33 * b33 + a34 * b43
      , a31 * b14 + a32 * b24 + a33 * b34 + a34 * b44
      )
    , ( a41 * b11 + a42 * b21 + a43 * b31 + a44 * b41
      , a41 * b12 + a42 * b22 + a43 * b32 + a44 * b42
      , a41 * b13 + a42 * b23 + a43 * b33 + a44 * b43
      , a41 * b14 + a42 * b24 + a43 * b34 + a44 * b44
      )
    )


{-| The transpose.
Flips a matrix along it's diagonal.

`A^T`

-}
transpose : Float4x4 -> Float4x4
transpose ( ( a11, a12, a13, a14 ), ( a21, a22, a23, a24 ), ( a31, a32, a33, a34 ), ( a41, a42, a43, a44 ) ) =
    ( ( a11, a21, a31, a41 )
    , ( a12, a22, a32, a42 )
    , ( a13, a23, a33, a43 )
    , ( a14, a24, a34, a44 )
    )


{-| `A*v`
-}
mulVector : Float4x4 -> Float4 -> Float4
mulVector ( r1, r2, r3, r4 ) v =
    ( V4.dot r1 v, V4.dot r2 v, V4.dot r3 v, V4.dot r4 v )



-- I decided not to include the determinant, as the focus of this library is graphics programming
-- and calculating determinants shouldn't be needed.
--det ( ( a11, a12, a13, a14 ), ( a21, a22, a23, a24 ), ( a31, a32, a33, a34 ), ( a41, a42, a43, a44 ) ) =
--    (a11 * M3.det ( ( a22, a23, a24 ), ( a32, a33, a34 ), ( a42, a43, a44 ) ))
--        - (a12 * M3.det ( ( a21, a23, a24 ), ( a31, a33, a34 ), ( a41, a43, a44 ) ))
--        + (a13 * M3.det ( ( a21, a22, a24 ), ( a31, a32, a34 ), ( a41, a42, a44 ) ))
--        - (a14 * M3.det ( ( a22, a23, a24 ), ( a31, a32, a33 ), ( a41, a42, a43 ) ))
--
--
--
-- Transformations


{-| Transform a vector by an arbitrary transformation matrix.
In math terms, we do:

    |v'| = A*|v|  v'/w
    |w |     |1|,

NaN/infinity warning: if w = 0

-}
transform : Float4x4 -> Float3 -> Float3
transform ( ( a11, a12, a13, a14 ), ( a21, a22, a23, a24 ), ( a31, a32, a33, a34 ), ( a41, a42, a43, a44 ) ) ( x, y, z ) =
    let
        ( r0, r1, r2, w ) =
            ( a11 * x + a12 * y + a13 * z + a14
            , a21 * x + a22 * y + a23 * z + a24
            , a31 * x + a32 * y + a33 * z + a34
            , a41 * x + a42 * y + a43 * z + a44
            )
    in
        if w /= 1.0 then
            ( r0 / w, r1 / w, r2 / w )
        else
            ( r0, r1, r2 )



-- affine transforms


{-| Creates a rotation matrix `R`.
This represents a rotation of `angle` degrees (in radians)
around the vector specified by `axis`.
The rotation is specified according to the right hand rule.

NaN warning: if axis = 0

-}
makeRotate : Float -> Float3 -> Float4x4
makeRotate angle axis =
    let
        ( x, y, z ) =
            V3.normalize axis

        ( c, s ) =
            ( cos angle, sin angle )

        c1 =
            1 - c

        ( zs, ys, xs, xc1, yc1 ) =
            ( z * s, y * s, x * s, x * c1, y * c1 )

        ( xyc1, xzc1, yzc1 ) =
            ( y * xc1, z * xc1, z * yc1 )
    in
        ( ( x * xc1 + c, xyc1 - zs, xzc1 + ys, 0 )
        , ( xyc1 + zs, y * yc1 + c, yzc1 - xs, 0 )
        , ( xzc1 - ys, yzc1 + xs, z * z * c1 + c, 0 )
        , ( 0, 0, 0, 1 )
        )


{-| Rotate an affine transform by an angle along the given axis.

NaN warning: if axis = 0

-}
rotate : Float -> Float3 -> Float4x4 -> Float4x4
rotate angle (( a0, a1, a2 ) as axis) ( ( m11, m12, m13, m14 ), ( m21, m22, m23, m24 ), ( m31, m32, m33, m34 ), m4 ) =
    let
        l_2 =
            a0 * a0 + a1 * a1 + a2 * a2

        ( x, y, z ) =
            if l_2 /= 1.0 then
                let
                    l_1 =
                        1 / (sqrt l_2)
                in
                    ( a0 * l_1, a1 * l_1, a2 * l_1 )
            else
                ( a0, a1, a2 )

        ( c, s ) =
            ( cos angle, sin angle )

        c1 =
            1 - c

        ( zs, ys, xs, xc1, yc1 ) =
            ( z * s, y * s, x * s, x * c1, y * c1 )

        ( xyc1, xzc1, yzc1 ) =
            ( y * xc1, z * xc1, z * yc1 )

        ( ( t11, t12, t13 ), ( t21, t22, t23 ), ( t31, t32, t33 ) ) =
            ( ( x * xc1 + c, xyc1 - zs, xzc1 + ys )
            , ( xyc1 + zs, y * yc1 + c, yzc1 - xs )
            , ( xzc1 - ys, yzc1 + xs, z * z * c1 + c )
            )
    in
        ( ( t11 * m11 + t21 * m12 + t31 * m13
          , t12 * m11 + t22 * m12 + t32 * m13
          , t13 * m11 + t23 * m12 + t33 * m13
          , m14
          )
        , ( t11 * m21 + t21 * m22 + t31 * m23
          , t12 * m21 + t22 * m22 + t32 * m23
          , t13 * m21 + t23 * m22 + t33 * m23
          , m24
          )
        , ( t11 * m31 + t21 * m32 + t31 * m33
          , t12 * m31 + t22 * m32 + t32 * m33
          , t13 * m31 + t23 * m32 + t33 * m33
          , m34
          )
        , m4
        )


{-| Create a scale matrix `S`.
-}
makeScale : Float3 -> Float4x4
makeScale ( x, y, z ) =
    ( ( x, 0, 0, 0 )
    , ( 0, y, 0, 0 )
    , ( 0, 0, z, 0 )
    , ( 0, 0, 0, 1 )
    )


{-| Scale an affine transform with the given vector.
Same as `M*makeScale(s)`
-}
scale : Float3 -> Float4x4 -> Float4x4
scale ( x, y, z ) ( ( a11, a12, a13, a14 ), ( a21, a22, a23, a24 ), ( a31, a32, a33, a34 ), a4 ) =
    ( ( a11 * x, a12 * y, a13 * z, a14 )
    , ( a21 * x, a22 * y, a23 * z, a24 )
    , ( a31 * x, a32 * y, a33 * z, a34 )
    , a4
    )


{-| Create a translation matrix `T`
-}
makeTranslate : Float3 -> Float4x4
makeTranslate ( x, y, z ) =
    ( ( 1, 0, 0, x )
    , ( 0, 1, 0, y )
    , ( 0, 0, 1, z )
    , ( 0, 0, 0, 1 )
    )


{-| Translate an affine transform by the given vector.
Same as `M*makeTranslate(t)`
-}
translate : Float3 -> Float4x4 -> Float4x4
translate ( x, y, z ) ( ( a11, a12, a13, a14 ), ( a21, a22, a23, a24 ), ( a31, a32, a33, a34 ), a4 ) =
    ( ( a11, a12, a13, a11 * x + a12 * y + a13 * z + a14 )
    , ( a21, a22, a23, a21 * x + a22 * y + a23 * z + a24 )
    , ( a31, a32, a33, a31 * x + a32 * y + a33 * z + a34 )
    , a4
    )


{-| Create an affine transform from 3 orthogonal (perpendicular) vectors.
Make sure xAxis yAxis zAxis are really orthonormal,
otherwise you won't get an affine transform!
Only use this if you know what you are doing.

    makeBasis (1, 0, 0) (0, 1, 0) (0, 0, 1) == identity

-}
makeBasis : Float3 -> Float3 -> Float3 -> Float4x4
makeBasis ( ax, ay, az ) ( bx, by, bz ) ( cx, cy, cz ) =
    ( ( ax, bx, cx, 0 )
    , ( ay, by, cy, 0 )
    , ( az, bz, cz, 0 )
    , ( 0, 0, 0, 1 )
    )


{-| Create a transform that makes a 3D object look at the target.
Very often used with cameras to make them look at a target.
The up vector is usually (0, 1, 0), e.g. the y-axis. (some people also use the z-axis)

    makeLookAt cameraPosition target (0, 1, 0)

NaN warning: if cameraPosition = target

-}
makeLookAt : Float3 -> Float3 -> Float3 -> Float4x4
makeLookAt eye target up =
    -- the math goes like this:
    -- z (forward) = norm(eye - target)
    -- x (right) = norm(up cross z)
    -- y (up) = norm(z cross x)
    --
    -- create a new basis for this transformation
    -- bc =|      0|
    --     |x y z 0|
    --     |      0|
    --     |0 0 0 1|
    -- create a translation matrix for the camera
    -- tc = |I3 eye|
    --      |0  1  |
    -- invert the transform
    -- (tc*bc)^-1
    --      (( because bc is orthogonal and translate(eye)^-1 = translate(-eye) ))
    -- = (bc^T*translate(-eye))
    -- =|     x^T     -(x dot eye)|
    --  |     y^T     -(y dot eye)|
    --  |     z^T     -(z dot eye)|
    --  | 0   0   0   1           |
    --
    let
        (( z0, z1, z2 ) as z) =
            V3.normalize (V3.sub eye target)

        (( x0, x1, x2 ) as x) =
            V3.normalize (V3.cross up z)

        (( y0, y1, y2 ) as y) =
            V3.normalize (V3.cross z x)
    in
        ( ( x0, x1, x2, -(V3.dot x eye) )
        , ( y0, y1, y2, -(V3.dot y eye) )
        , ( z0, z1, z2, -(V3.dot z eye) )
        , ( 0, 0, 0, 1 )
        )


{-| Creates an affine transform given a translation, a scale, a rotation and a pivot vector.
The pivot is the 'center' of the rotation and scaling operation.

    makeTransform translation scale angle rotationAxis pivot

NaN warning: if rotationAxis = 0

-}
makeTransform : Float3 -> Float3 -> Float -> Float3 -> Float3 -> Float4x4
makeTransform ( tx, ty, tz ) ( sx, sy, sz ) angle axis ( px, py, pz ) =
    -- I used pythons sympy library to arrive at this result
    -- It's T*R*S*(Tp^-1)
    --  Where
    --      Tp = makeTranslate (px,py,pz)
    --      T = makeTranslate (tx,ty,tz)
    --      S = makeScale (sx,sy,sz)
    --      R = makeRotate angle (ax,ay,az)
    let
        ( c, s ) =
            ( cos angle, sin angle )

        c1 =
            1 - c

        ( rx, ry, rz ) =
            V3.normalize axis

        ( rxs, rys, rzs ) =
            ( rx * s, ry * s, rz * s )

        ( rxc1, ryc1 ) =
            ( rx * c1, ry * c1 )

        ( ryxc1, rzxc1, rzyc1 ) =
            ( ry * rxc1, rz * rxc1, rz * ryc1 )

        ( new_x1, new_x2, new_x3 ) =
            ( sx * (rx * rxc1 + c), sx * (ryxc1 + rzs), sx * (rzxc1 - rys) )

        ( new_y1, new_y2, new_y3 ) =
            ( sy * (ryxc1 - rzs), sy * (ry * ryc1 + c), sy * (rzyc1 + rxs) )

        ( new_z1, new_z2, new_z3 ) =
            ( sz * (rzxc1 + rys), sz * (rzyc1 - rxs), sz * (rz * rz * c1 + c) )
    in
        ( ( new_x1, new_y1, new_z1, -px * new_x1 - py * new_y1 - pz * new_z1 + tx )
        , ( new_x2, new_y2, new_z2, -px * new_x2 - py * new_y2 - pz * new_z2 + ty )
        , ( new_x3, new_y3, new_z3, -px * new_x3 - py * new_y3 - pz * new_z3 + tz )
        , ( 0, 0, 0, 1 )
        )



-- operations


{-| Same as `transform`, but a bit faster. Only correct if used with an affine transform!
-}
transformAffine : Float4x4 -> Float3 -> Float3
transformAffine ( ( a11, a12, a13, a14 ), ( a21, a22, a23, a24 ), ( a31, a32, a33, a34 ), ( a41, a42, a43, a44 ) ) ( x, y, z ) =
    ( a11 * x + a12 * y + a13 * z + a14
    , a21 * x + a22 * y + a23 * z + a24
    , a31 * x + a32 * y + a33 * z + a34
    )


{-| Transform a vector by the transformation specified in the parameters

    transformBy translation scale angle rotationAxis pivot v

-}
transformBy : Float3 -> Float3 -> Float -> Float3 -> Float3 -> Float3 -> Float3
transformBy translation scale angle axis pivot =
    transformAffine (makeTransform translation scale angle axis pivot)



--|
--Calculate the inverse of an affine transform.
--This represents doing all the transformations in reverse.
--
--        should be (inv(R), -inv(R)*t)
--                  (      0,        1)
--inverseAffine ( ( a11, a12, a13, t1 ), ( a21, a22, a23, t2 ), ( a31, a32, a33, t3 ), a4 ) =
--    TODO
--
--
--|
--
--| R t |^-1 * |t| = | R^-1 * (t - b) |
--| 0 1 |      |1| = | 1              |
--transformBackAffine m b =
--    TODO


{-| Multiply two affine transforms. Same as `mul` but a bit faster.
Only correct if used with an affine transform!
-}
mulAffine : Float4x4 -> Float4x4 -> Float4x4
mulAffine ( ( a11, a12, a13, a14 ), ( a21, a22, a23, a24 ), ( a31, a32, a33, a34 ), ( a41, a42, a43, a44 ) ) ( ( b11, b12, b13, b14 ), ( b21, b22, b23, b24 ), ( b31, b32, b33, b34 ), ( b41, b42, b43, b44 ) ) =
    ( ( a11 * b11 + a12 * b21 + a13 * b31
      , a11 * b12 + a12 * b22 + a13 * b32
      , a11 * b13 + a12 * b23 + a13 * b33
      , a11 * b14 + a12 * b24 + a13 * b34 + a14
      )
    , ( a21 * b11 + a22 * b21 + a23 * b31
      , a21 * b12 + a22 * b22 + a23 * b32
      , a21 * b13 + a22 * b23 + a23 * b33
      , a21 * b14 + a22 * b24 + a23 * b34 + a24
      )
    , ( a31 * b11 + a32 * b21 + a33 * b31
      , a31 * b12 + a32 * b22 + a33 * b32
      , a31 * b13 + a32 * b23 + a33 * b33
      , a31 * b14 + a32 * b24 + a33 * b34 + a34
      )
    , ( 0, 0, 0, 1 )
    )


{-| Calculate the inverse of a rigid body transform.
This a special form of affine transform, that is only composed of
rotations and translations.

    | R t |^-1 = | R^T -R^T*t |
    | 0 1 |      | 0    1     |

It's usually easier and faster to just do the construction of the matrix you want to invert in reverse, e.g.

    inverseRigidBodyTransform (makeRotate 1.2 (0,1,0)) == makeRotate -1.2 (0,1,0)
    inverseRigidBodyTransform (makeScale (2,3,1)) == makeScale (1/2,1/3,1)

-}
inverseRigidBodyTransform : Float4x4 -> Float4x4
inverseRigidBodyTransform ( ( a11, a12, a13, t1 ), ( a21, a22, a23, t2 ), ( a31, a32, a33, t3 ), a4 ) =
    -- I renamed inverseOrthonormal to inverseRidgidBodyTransform, as the other name didn't make sense,
    -- as the inverse of an orthonormal matrix is just the transpose of that matrix.
    -- I don't know what the original inverseOrthonormal actually did, but it might have done this.
    ( ( a11, a21, a31, -a11 * t1 - a21 * t2 - a31 * t3 )
    , ( a12, a22, a32, -a12 * t1 - a22 * t2 - a32 * t3 )
    , ( a13, a23, a33, -a13 * t1 - a23 * t2 - a33 * t3 )
    , a4
    )



-- Cameras


{-| Creates a matrix for a projection frustum.

    makeFrustum left right bottom top znear zfar

<http://www.songho.ca/opengl/gl_projectionmatrix.html>

NaN warning: if left = right or bottom = top or znear = zfar

-}
makeFrustum : Float -> Float -> Float -> Float -> Float -> Float -> Float4x4
makeFrustum left right bottom top znear zfar =
    let
        ( r_l, t_b, zf_zn, zn_2 ) =
            ( right - left, top - bottom, zfar - znear, 2 * znear )
    in
        ( ( zn_2 / r_l, 0, (right + left) / r_l, 0 )
        , ( 0, zn_2 / t_b, (top + bottom) / t_b, 0 )
        , ( 0, 0, -(zfar + znear) / zf_zn, -zn_2 * zfar / zf_zn )
        , ( 0, 0, -1, 0 )
        )


{-| Creates a matrix for a perspective projection.

    makePerspective fovy aspect znear zfar

fovy - field of view in the y axis, in degrees
aspect - aspect ratio
znear - the near z distance of the projection
zfar - the far z distance of the projection

NaN warning: if znear = zfar or fovy = 720+n*1440

-}
makePerspective : Float -> Float -> Float -> Float -> Float4x4
makePerspective fovy aspect znear zfar =
    let
        ymax =
            znear * tan (fovy * pi / 360.0)

        ymin =
            -ymax

        ( xmin, xmax ) =
            ( ymin * aspect, ymax * aspect )
    in
        makeFrustum xmin xmax ymin ymax znear zfar


{-| This creates an orthographic projection.

    makeOrtho left right bottom top znear zfar

NaN warning: if left = right or bottom = top or znear = zfar

-}
makeOrtho : Float -> Float -> Float -> Float -> Float -> Float -> Float4x4
makeOrtho left right bottom top znear zfar =
    let
        ( r_l, t_b, zf_zn ) =
            ( right - left, top - bottom, zfar - znear )
    in
        ( ( 2 / r_l, 0, 0, -(right + left) / r_l )
        , ( 0, 2 / t_b, 0, -(top + bottom) / t_b )
        , ( 0, 0, -2 / zf_zn, -(zfar + znear) / zf_zn )
        , ( 0, 0, 0, 1 )
        )


{-| Same as `makeOrtho`, but with `znear = -1` and `zfar = 1` set.

    makeOrtho2d left right bottom top

NaN warning: if left = right or bottom = top

-}
makeOrtho2d : Float -> Float -> Float -> Float -> Float4x4
makeOrtho2d left right bottom top =
    makeOrtho left right bottom top -1 1


{-| This checks whether `|A - B| <= eps`.

    almostEqual eps a b

This is useful for testing, see the tests of this library for how this makes testing easy.

Since any definition of a norm can be used for this, it uses the simple `maxNorm`

-}
almostEqual : Float -> Float4x4 -> Float4x4 -> Bool
almostEqual eps a b =
    maxNorm (sub a b) <= eps


{-| The max norm. This is the biggest element of a matrix.
Useful for fuzz testing.
-}
maxNorm : Float4x4 -> Float
maxNorm =
    foldl (\elem acc -> max (abs elem) acc) 0
