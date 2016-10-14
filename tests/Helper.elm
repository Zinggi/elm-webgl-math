module Helper exposing (..)

import Math.Vector2 as Ref2
import Math.Vector3 as Ref3 exposing (vec3)
import Math.Vector4 as Ref4
import Math.Matrix4 as RefM4
import Math.Float2 as V2
import Math.Float3 as V3
import Math.Float4 as V4
import Fuzz exposing (Fuzzer)
import Expect


smallFloat =
    Fuzz.floatRange -1000 1000


v2 : Fuzzer V2.Float2
v2 =
    Fuzz.map2 (\x y -> ( x, y )) smallFloat smallFloat


v3 =
    Fuzz.map3 (\x y z -> ( x, y, z )) smallFloat smallFloat smallFloat


v4 =
    Fuzz.map4 (\x y z w -> ( x, y, z, w )) smallFloat smallFloat smallFloat smallFloat


m4 =
    Fuzz.map4 (\a b c d -> ( a, b, c, d )) v4 v4 v4 v4


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
