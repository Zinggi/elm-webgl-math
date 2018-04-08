module Helper exposing (..)

import Math.Vector2 as Ref2
import Math.Vector3 as Ref3 exposing (vec3)
import Math.Vector4 as Ref4
import Math.Matrix4 as RefM4
import Vector2 as V2
import Vector3 as V3
import Vector4 as V4
import Matrix4 as M4
import Fuzz exposing (Fuzzer)
import Expect


smallFloat =
    Fuzz.floatRange -1000 1000


smallNonZeroFloat =
    Fuzz.map3
        (\x y isX ->
            if isX then
                x
            else
                y
        )
        (Fuzz.floatRange 0.001 1000)
        (Fuzz.floatRange -1000 -0.001)
        Fuzz.bool


v2 : Fuzzer V2.Float2
v2 =
    Fuzz.map2 (\x y -> ( x, y )) smallFloat smallFloat


v3 =
    Fuzz.map3 (\x y z -> ( x, y, z )) smallFloat smallFloat smallFloat


v3NonZero =
    Fuzz.map3 (\x y z -> ( x, y, z )) smallNonZeroFloat smallNonZeroFloat smallNonZeroFloat


v4 =
    Fuzz.map4 (\x y z w -> ( x, y, z, w )) smallFloat smallFloat smallFloat smallFloat


m4 =
    Fuzz.map4 (\a b c d -> ( a, b, c, d )) v4 v4 v4 v4


m4affine =
    Fuzz.map4 (\t s ro r -> M4.mul (M4.makeTranslate t) (M4.mul (M4.makeScale s) (M4.makeRotate r ro)))
        v3
        v3NonZero
        v3NonZero
        smallFloat


m4rigidBody =
    Fuzz.map3 (\t ro r -> M4.mul (M4.makeTranslate t) (M4.makeRotate r ro))
        v3
        v3NonZero
        smallFloat


expectAlmostEqualErr =
    mkAlmostEqFn (identity) (-) (identity)


expectAlmostEqual : Float -> Float -> Expect.Expectation
expectAlmostEqual =
    expectAlmostEqualErr 0.1


expectAlmostEqualV2 =
    expectAlmostEqualV2Err 0.1


expectAlmostEqualV3 =
    expectAlmostEqualV3Err 0.1


expectAlmostEqualV4 =
    expectAlmostEqualV4Err 0.1


expectAlmostEqualV2Err =
    mkAlmostEqFn V2.length V2.sub Ref2.toTuple


expectAlmostEqualV3Err =
    mkAlmostEqFn V3.length V3.sub Ref3.toTuple


expectAlmostEqualV4Err =
    mkAlmostEqFn V4.length V4.sub Ref4.toTuple


mkAlmostEqFn len sub toTup e a b =
    let
        err =
            -- elm-linear-algebra uses 32bit floats, that's why we get poor precision
            abs (len (sub a (toTup b)))
    in
        Expect.true "" (err < e)
            |> Expect.onFail ("expected almost equal, failed with error " ++ toString err)


expectAlmostEqualM4 a b =
    Expect.true "" (M4.almostEqual 0.0001 a b)
        |> Expect.onFail
            ("expected almost equal, failed with error "
                ++ toString (M4.maxNorm (M4.sub a b))
                ++ "\n  a = "
                ++ toString a
                ++ "\n  b = "
                ++ toString b
            )
