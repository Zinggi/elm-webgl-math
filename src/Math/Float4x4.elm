module Math.Float4x4 exposing (..)

import Math.Float4 as V4 exposing (Float4, Vec4)
import Math.Float3 as V3 exposing (Float3, Vec3)


type alias Float4x4 =
    Mat4x4 Float


type alias Mat4x4 a =
    ( Vec4 a, Vec4 a, Vec4 a, Vec4 a )


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


transform : Float4x4 -> Float3 -> Float3
transform ( ( a11, a12, a13, a14 ), ( a21, a22, a23, a24 ), ( a31, a32, a33, a34 ), ( a41, a42, a43, a44 ) ) ( v0, v1, v2 ) =
    let
        ( r0, r1, r2, w ) =
            ( a11 * v0 + a21 * v1 + a31 * v2 + a41
            , a12 * v0 + a22 * v1 + a32 * v2 + a42
            , a13 * v0 + a23 * v1 + a33 * v2 + a43
            , a14 * v0 + a24 * v1 + a34 * v2 + a44
            )
    in
        if w /= 1.0 then
            ( r0 / w, r1 / w, r2 / w )
        else
            ( r0, r1, r2 )



-- inverseOrthonormal:
--  I left out inverseOrthonormal because I don't understand
--  what it is doing in the elm-linear-algebra library.
--  The inverse of an orthonormal matrix should just be the
--  transpose of that matrix, but the library is doing something else.


mulAffine ( ( a11, a12, a13, a14 ), ( a21, a22, a23, a24 ), ( a31, a32, a33, a34 ), ( a41, a42, a43, a44 ) ) ( ( b11, b12, b13, b14 ), ( b21, b22, b23, b24 ), ( b31, b32, b33, b34 ), ( b41, b42, b43, b44 ) ) =
    ( ( a11 * b11 + a12 * b21 + a13 * b31
      , a21 * b11 + a22 * b21 + a23 * b31
      , a31 * b11 + a32 * b21 + a33 * b31
      , 0
      )
    , ( a11 * b12 + a12 * b22 + a13 * b32
      , a21 * b12 + a22 * b22 + a23 * b32
      , a31 * b12 + a32 * b22 + a33 * b32
      , 0
      )
    , ( a11 * b13 + a12 * b23 + a13 * b33
      , a21 * b13 + a22 * b23 + a23 * b33
      , a31 * b13 + a32 * b23 + a33 * b33
      , 0
      )
    , ( a11 * b14 + a12 * b24 + a13 * b34 + a14
      , a21 * b14 + a22 * b24 + a23 * b34 + a24
      , a31 * b14 + a32 * b24 + a33 * b34 + a34
      , 1
      )
    )


makeBasis vx vy vz =
    ( V4.fromV3 vx 0
    , V4.fromV3 vy 0
    , V4.fromV3 vz 0
    , ( 0, 0, 0, 1 )
    )


makeFrustum left right bottom top znear zfar =
    ( ( 2 * znear / (right - left), 0, 0, 0 )
    , ( 0, 2 * znear / (top - bottom), 0, 0 )
    , ( (right + left) / (right - left), (top + bottom) / (top - bottom), -(zfar + znear) / (zfar - znear), -1 )
    , ( 0, 0, -2 * zfar * znear / (zfar - znear), 0 )
    )


makePerspective fovy aspect znear zfar =
    let
        ymax =
            znear * tan (fovy * pi / 360.0)

        ymin =
            -ymax

        xmin =
            ymin * aspect

        xmax =
            ymax * aspect
    in
        makeFrustum xmin xmax ymin ymax znear zfar


makeOrtho left right bottom top znear zfar =
    ( ( 2 / (right - left), 0, 0, 0 )
    , ( 0, 2 / (top - bottom), 0, 0 )
    , ( 0, 0, -2 / (zfar - znear), 0 )
    , ( -(right + left) / (right - left), -(top + bottom) / (top - bottom), -(zfar + znear) / (zfar - znear), 1 )
    )


makeOrtho2d left right bottom top =
    makeOrtho left right bottom top -1 1


makeLookAt eye center up =
    let
        ( e0, e1, e2 ) =
            eye

        z =
            V3.directionFromTo center eye

        ( z0, z1, z2 ) =
            z

        x =
            V3.normalize (V3.cross up z)

        ( x0, x1, x2 ) =
            x

        y =
            V3.normalize (V3.cross z x)

        ( y0, y1, y2 ) =
            y

        tm1 =
            ( ( x0, y0, z0, 0 )
            , ( x1, y1, z1, 0 )
            , ( x2, y2, z2, 0 )
            , ( 0, 0, 0, 1 )
            )

        tm2 =
            ( ( 1, 0, 0, 0 )
            , ( 0, 1, 0, 0 )
            , ( 0, 0, 1, 0 )
            , ( -e0, -e1, -e2, 1 )
            )
    in
        mul tm1 tm2


rotate angle (( a0, a1, a2 ) as axis) ( ( m11, m21, m31, m41 ), ( m12, m22, m32, m42 ), ( m13, m23, m33, m43 ), m_bot ) =
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

        ( xs, ys, zs, xyc1, xzc1, yzc1 ) =
            ( x * s, y * s, z * s, x * y * c1, x * z * c1, y * z * c1 )

        ( ( t11, t21, t31 ), ( t12, t22, t32 ), ( t13, t23, t33 ) ) =
            ( ( x * x * c1 + c, xyc1 + zs, xzc1 - ys )
            , ( xyc1 - zs, y * y * c1 + c, yzc1 + xs )
            , ( xzc1 + ys, yzc1 - xs, z * z * c1 + c )
            )
    in
        ( ( m11 * t11 + m12 * t21 + m13 * t31
          , m21 * t11 + m22 * t21 + m23 * t31
          , m31 * t11 + m32 * t21 + m33 * t31
          , m41 * t11 + m42 * t21 + m43 * t31
          )
        , ( m11 * t12 + m12 * t22 + m13 * t32
          , m21 * t12 + m22 * t22 + m23 * t32
          , m31 * t12 + m32 * t22 + m33 * t32
          , m41 * t12 + m42 * t22 + m43 * t32
          )
        , ( m11 * t13 + m12 * t23 + m13 * t33
          , m21 * t13 + m22 * t23 + m23 * t33
          , m31 * t13 + m32 * t23 + m33 * t33
          , m41 * t13 + m42 * t23 + m43 * t33
          )
        , m_bot
        )


scale ( x, y, z ) ( m1, m2, m3, m4 ) =
    ( V4.scale x m1, V4.scale y m2, V4.scale z m3, m4 )


translate ( x, y, z ) ( m1, m2, m3, m4 ) =
    ( m1, m2, m3, V4.add (V4.add (V4.scale x m1) (V4.scale y m2)) (V4.add (V4.scale z m3) m4) )


makeRotate angle axis =
    let
        ( x, y, z ) =
            V3.normalize axis

        ( c, s ) =
            ( cos angle, sin angle )

        c1 =
            1 - c
    in
        ( ( x * x * c1 + c, y * x * c1 + z * s, z * x * c1 - y * s, 0 )
        , ( x * y * c1 - z * s, y * y * c1 + c, y * z * c1 + x * s, 0 )
        , ( x * z * c1 + y * s, y * z * c1 - x * s, z * z * c1 + c, 0 )
        , ( 0, 0, 0, 1 )
        )


makeScale ( x, y, z ) =
    ( ( x, 0, 0, 0 )
    , ( 0, y, 0, 0 )
    , ( 0, 0, z, 0 )
    , ( 0, 0, 0, 1 )
    )


makeTranslate ( x, y, z ) =
    ( ( 1, 0, 0, 0 )
    , ( 0, 1, 0, 0 )
    , ( 0, 0, 1, 0 )
    , ( x, y, z, 1 )
    )


makeTransform ( tx, ty, tz ) ( sx, sy, sz ) ( ax, ay, az ) angle ( px, py, pz ) =
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
