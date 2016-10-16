module Math.Float4x4 exposing (..)

import Math.Float4 as V4 exposing (Float4, Vec4)
import Math.Float3 as V3 exposing (Float3, Vec3)


-- Useful websites/references:
-- http://www.euclideanspace.com/maths/geometry/affine/matrix4x4/index.htm
-- http://www.codinglabs.net/article_world_view_projection_matrix.aspx
-- https://github.com/mrdoob/three.js/blob/master/src/math/Matrix4.js
-- http://learnopengl.com/#!Getting-started/Transformations
-- http://www.songho.ca/opengl/gl_projectionmatrix.html
--
-- NOTE: This library uses row-major order,
-- the elm-community/elm-linear-algebra one uses column-major order!


type alias Float4x4 =
    Mat4x4 Float


type alias Mat4x4 a =
    ( Vec4 a, Vec4 a, Vec4 a, Vec4 a )



-- general purpose math


identity =
    ( ( 1, 0, 0, 0 )
    , ( 0, 1, 0, 0 )
    , ( 0, 0, 1, 0 )
    , ( 0, 0, 0, 1 )
    )


map f =
    V4.map (V4.map f)


map2 f =
    V4.map2 (V4.map2 f)


add =
    map2 (+)


sub =
    map2 (-)


elementWiseMul =
    map2 (*)


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


transpose ( ( a11, a12, a13, a14 ), ( a21, a22, a23, a24 ), ( a31, a32, a33, a34 ), ( a41, a42, a43, a44 ) ) =
    ( ( a11, a21, a31, a41 )
    , ( a12, a22, a32, a42 )
    , ( a13, a23, a33, a43 )
    , ( a14, a24, a34, a44 )
    )



-- # Transformation matrices.
-- A transformation matrix represents an arbitrary transform on a 3d vector.
-- To transform a 3d vector v, we multiply it with a 4x4 transformation matrix T.
-- To do that we need to write v in homogeneous coordinates,
-- then transform the result back to normal coordinates.
--      |v|   |v'|
--  M * |1| = |w |, res = v'/w


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



-- ## Affine transformations
-- These represent a transform that preserves shapes, e.g. translations, rotations and scaling.
-- These are composed of a 3x3 rotation and scale matrix R and a translation vector t:
--   |R t|
--   |0 1|
--
-- **NOTE**: a camera projection is **not** an affine transformation.


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


{-|
Rotate an affine transform by an angle along the given axis.
-}
rotate angle (( a0, a1, a2 ) as axis) ( ( m11, m12, m13, m14 ), ( m21, m22, m23, m24 ), ( m31, m32, m33, m34 ), m4 ) =
    let
        l =
            sqrt (a0 * a0 + a1 * a1 + a2 * a2)

        ( x, y, z ) =
            if l /= 1.0 then
                ( a0 / l, a1 / l, a2 / l )
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


makeScale ( x, y, z ) =
    ( ( x, 0, 0, 0 )
    , ( 0, y, 0, 0 )
    , ( 0, 0, z, 0 )
    , ( 0, 0, 0, 1 )
    )


{-|
Scale an affine transform with the given vector.
Same as M*makeScale(s)
-}
scale ( x, y, z ) ( ( a11, a12, a13, a14 ), ( a21, a22, a23, a24 ), ( a31, a32, a33, a34 ), a4 ) =
    ( ( a11 * x, a12 * y, a13 * z, a14 )
    , ( a21 * x, a22 * y, a23 * z, a24 )
    , ( a31 * x, a32 * y, a33 * z, a34 )
    , a4
    )


makeTranslate ( x, y, z ) =
    ( ( 1, 0, 0, x )
    , ( 0, 1, 0, y )
    , ( 0, 0, 1, z )
    , ( 0, 0, 0, 1 )
    )


{-|
Translate an affine transform by the given vector.
Same as M*makeTranslate(t)
-}
translate ( x, y, z ) ( ( a11, a12, a13, a14 ), ( a21, a22, a23, a24 ), ( a31, a32, a33, a34 ), a4 ) =
    ( ( a11, a12, a13, a11 * x + a12 * y + a13 * z + a14 )
    , ( a21, a22, a23, a21 * x + a22 * y + a23 * z + a24 )
    , ( a31, a32, a33, a31 * x + a32 * y + a33 * z + a34 )
    , a4
    )


{-| same as transform, but only gives correct results if used with an affine transform
-}
transformAffine ( ( a11, a12, a13, a14 ), ( a21, a22, a23, a24 ), ( a31, a32, a33, a34 ), ( a41, a42, a43, a44 ) ) ( x, y, z ) =
    ( a11 * x + a12 * y + a13 * z + a14
    , a21 * x + a22 * y + a23 * z + a24
    , a31 * x + a32 * y + a33 * z + a34
    )


{-|
Calculate the inverse of an affine transform.
This represents doing all the transformations in reverse.
-}
inverseAffine ( ( a11, a12, a13, t1 ), ( a21, a22, a23, t2 ), ( a31, a32, a33, t3 ), a4 ) =
    -- TODO: this is wrong!
    -- e.g. the inverse of a scale matrix isn't S.T
    -- This is only true for rotation and translation
    -- Since R is an orthogonal matrix, the inverse of an affine transform looks like this:
    --   |R^T -R^T*t|
    --   |0    1    |
    --
    -- I renamed inverseOrthonormal to inverseAffine, as the other name didn't make sense,
    -- as the inverse of an orthonormal matrix is just the transpose of that matrix.
    ( ( a11, a21, a31, -a11 * t1 - a21 * t2 - a31 * t3 )
    , ( a12, a22, a32, -a12 * t1 - a22 * t2 - a32 * t3 )
    , ( a13, a23, a33, -a13 * t1 - a23 * t2 - a33 * t3 )
    , a4
    )


{-| Multiply two affine transforms.

    transform (mul A B) v == transform A (transform B v)

-}
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


{-|
make sure xAxis yAxis zAxis are orthonormal,
otherwise you won't get an affine transform!

    makeBasis (1, 0, 0) (0, 1, 0) (0, 0, 1) == identity

-}
makeBasis ( ax, ay, az ) ( bx, by, bz ) ( cx, cy, cz ) =
    ( ( ax, bx, cx, 0 )
    , ( ay, by, cy, 0 )
    , ( az, bz, cz, 0 )
    , ( 0, 0, 0, 1 )
    )


{-|
Create a transform that makes a 3d object look at the target.
Very often used with cameras to make them look at a target.
The up vector is usually (0, 1, 0), e.g. the y-axis.

    makeLookAt cameraPosition target (0, 1, 0)

-}
makeLookAt eye target up =
    -- the math goes like this:
    -- z (forward) = norm(target - eye)
    -- x (right) = norm(z cross up)
    -- y (up) = norm(x cross z)
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
    --      - because bc is orthogonal and translate(eye)^-1 = translate(-eye) -
    -- = (bc^T*translate(-eye))
    -- =|     x^T     -(x dot eye)|
    --  |     y^T     -(y dot eye)|
    --  |     z^T     -(z dot eye)|
    --  | 0   0   0   1           |
    --
    let
        (( z0, z1, z2 ) as z) =
            V3.normalize (V3.sub target eye)

        (( x0, x1, x2 ) as x) =
            V3.normalize (V3.cross z up)

        ( y0, y1, y2 ) =
            V3.normalize (V3.cross x z)
    in
        ( ( x0, x1, x2, -(V3.dot x eye) )
        , ( y0, y1, y2, -(V3.dot y eye) )
        , ( z0, z1, z2, -(V3.dot z eye) )
        , ( 0, 0, 0, 1 )
        )


makeTransform ( tx, ty, tz ) ( sx, sy, sz ) ( ax, ay, az ) angle ( px, py, pz ) =
    -- TODO: this is wrong!
    -- I only need to do T*R*S*(-Tp)
    -- and I used wrong row order!
    --
    -- I used pythons sympy library to arrive at this result
    -- It's Tp*T*S*R*(-Tp)
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

        ( axs, ays, azs ) =
            ( ax * s, ay * s, az * s )

        ( azc1, ayc1 ) =
            ( az * c1, ay * c1 )

        ( ax_2c1, ay_2c1, az_2c1 ) =
            ( ax * ax * c1, ay * ayc1, az * azc1 )

        ( axyc1, axzc1, ayzc1 ) =
            ( ax * ayc1, ax * azc1, ay * azc1 )

        ( pstx, psty, pstz ) =
            ( px + sx + tx, py + sy + ty, pz + sz + tz )
    in
        ( ( -ax_2c1 - c, -axyc1 - azs, -axzc1 + ays, 0 )
        , ( -axyc1 + azs, -ay_2c1 - c, -axs - ayzc1, 0 )
        , ( -axzc1 - ays, axs - ayzc1, -az_2c1 - c, 0 )
        , ( -px - (ax_2c1 + c) * (pstx) - (axyc1 - azs) * (psty) - (axzc1 + ays) * (pstz), -py - (-axs + ayzc1) * (pstz) - (ay_2c1 + c) * (psty) - (axyc1 + azs) * (pstx), -pz - (axs + ayzc1) * (psty) - (az_2c1 + c) * (pstz) - (axzc1 - ays) * (pstx), -1 )
        )


transformBy ( tx, ty, tz ) ( sx, sy, sz ) ( ax, ay, az ) angle ( px, py, pz ) ( x, y, z ) =
    -- TODO: Wrong! see above
    let
        ( c, s ) =
            ( cos angle, sin angle )

        c1 =
            1 - c

        ( azc1, ayc1 ) =
            ( az * c1, ay * c1 )

        ( ax_2c1, ay_2c1, az_2c1 ) =
            ( ax * ax * c1, ay * ayc1, az * azc1 )

        ( axyc1, axzc1, ayzc1 ) =
            ( ax * ayc1, ax * azc1, ay * azc1 )

        ( ays, azs, axs ) =
            ( ay * s, az * s, ax * s )

        ( pstx, psty, pstz ) =
            ( px + sx + tx, py + sy + ty, pz + sz + tz )
    in
        ( x * (-ax_2c1 - c) + y * (-axyc1 - azs) + z * (-axzc1 + ays)
        , x * (-axyc1 + azs) + y * (-ay_2c1 - c) + z * (-axs - ayzc1)
        , x * (-axzc1 - ays) + y * (axs - ayzc1) + z * (-az_2c1 - c)
        , x * (-px - (ax_2c1 + c) * (pstx) - (axyc1 - azs) * (psty) - (axzc1 + ays) * (pstz)) + y * (-py - (-axs + ayzc1) * (pstz) - (ay_2c1 + c) * (psty) - (axyc1 + azs) * (pstx)) + z * (-pz - (axs + ayzc1) * (psty) - (az_2c1 + c) * (pstz) - (axzc1 - ays) * (pstx)) - 1
        )



-- Camera helper functions.
-- **NOTE**: Unless noted otherwise, these transforms are **not** affine transforms!


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


{-|
This creates an orthographic projection.
**Note**: this is an affine transform, you can use the `affine*` functions with this.
-}
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


{-|
Same as makeOrtho, but with znear = -1 and zfar = 1 set.
**Note**: this is an affine transform, you can use the `affine*` functions with this.
-}
makeOrtho2d left right bottom top =
    makeOrtho left right bottom top -1 1
