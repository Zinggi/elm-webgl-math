module Main exposing (main)

import Benchmark
import Benchmark.Program
import Html.App
import Math.Vector4 as RefV4
import Math.Vector3 as RefV3 exposing (vec3)
import Math.Matrix4 as RefM4
import Math.Float4 as V4
import Math.Float3 as V3
import Math.Float4x4 as M4


main : Program Never
main =
    Benchmark.Program.program
        [ pow 1000
        , pow 10000
        , pow 100000
        , pow 1000000
        , makeTransform 1000
        , makeTransform 10000
        , makeTransform 100000
        ]


{-| Set shorter benchmark time to make it complete more quickly.
-}
options =
    let
        defaults =
            Benchmark.defaultOptions
    in
        { defaults | maxTime = 2 }


powM4 a =
    powM4_ a M4.identity


powM4_ a0 a n =
    if n <= 0 then
        a
    else
        powM4_ a0 (M4.mul a0 a) (n - 1)


powRef4 a =
    powRef4_ a RefM4.identity


powRef4_ a0 a n =
    if n <= 0 then
        a
    else
        powRef4_ a0 (RefM4.mul a0 a) (n - 1)


pow n =
    Benchmark.suiteWithOptions options
        ("Matrix exponentiation: A^" ++ toString n)
        [ Benchmark.bench "M4.pow" (\() -> powM4 (M4.makeBasis ( 4, 2, 1 ) ( -4, 1, 8 ) ( -1, 2, 5 )) n)
        , Benchmark.bench "RefM4.pow" (\() -> powRef4 (RefM4.makeBasis (vec3 4 2 1) (vec3 -4 1 8) (vec3 -1 2 5)) n)
        ]


makeTransform times =
    Benchmark.suiteWithOptions options
        ("Real world application: makeTransform " ++ toString times ++ " times.")
        [ Benchmark.bench "M4.makeTransform" (\() -> makeTransformM4 times)
        , Benchmark.bench "M4.makeTransformNaive" (\() -> makeTransformM4Naive times)
        , Benchmark.bench "RefM4.makeTransform" (\() -> makeTransformRef4 times)
        ]


makeTransformM4 =
    makeTransformM4_ (M4.identity)


makeTransformM4_ res n =
    if n <= 0 then
        ()
    else
        makeTransformM4_ (M4.makeTransform ( 1, 2, 3 ) ( 3, 1, 2 ) ( 4, 3, 2 ) 42.3 ( -3, 2, 10 )) (n - 1)


makeTransformM4Naive =
    makeTransformM4Naive_ (M4.identity)


makeTransformM4Naive_ res n =
    if n <= 0 then
        ()
    else
        makeTransformM4Naive_ (makeTransformNaive ( 1, 2, 3 ) ( 3, 1, 2 ) ( 4, 3, 2 ) 42.3 ( -3, 2, 10 )) (n - 1)


makeTransformNaive translation scale rotationAxis rotation pivot =
    M4.identity
        |> M4.translate (V3.negate pivot)
        |> M4.scale scale
        |> M4.rotate rotation rotationAxis
        |> M4.translate (V3.add pivot translation)


makeTransformRef4 =
    makeTransformRef4_ (RefM4.identity)


makeTransformRef4_ res n =
    if n <= 0 then
        ()
    else
        makeTransformRef4_ (ref4makeTransform (vec3 1 2 3) (vec3 3 1 2) (vec3 4 3 2) 42.3 (vec3 -3 2 10)) (n - 1)


ref4makeTransform translation scale rotationAxis rotation pivot =
    RefM4.identity
        |> RefM4.translate (RefV3.negate pivot)
        |> RefM4.scale scale
        |> RefM4.rotate rotation rotationAxis
        |> RefM4.translate (RefV3.add pivot translation)
