module Main exposing (main)

import Benchmark
import Benchmark.Program as Benchmark
import Math.Vector4 as RefV4 exposing (vec4)
import Math.Vector3 as RefV3 exposing (vec3)
import Math.Matrix4 as RefM4
import Vector4 as V4
import Vector3 as V3
import Matrix4 as M4


main : Benchmark.Program
main =
    Benchmark.program <|
        List.concat
            [ [ pow 1000
              , pow 10000
              , pow 100000
              , pow 1000000
              ]
            , [ vectorAddition 1000
              , vectorAddition 10000
              , vectorAddition 100000
              ]
            , [ makeTransform 1000
              , makeTransform 10000
              , makeTransform 100000
              ]
            , [ rotateAround 1000
              , rotateAround 10000
              , rotateAround 100000
              ]
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


vectorAddition n =
    Benchmark.suiteWithOptions options
        ("Useless vector addition/scale: f a b = f (a+b) (-0.999999 * b), " ++ toString n ++ " times.")
        [ Benchmark.bench "V4.add / scale" (\() -> addV4 n ( 1, 2, 3, 5 ) ( -2, 5, 2.3, 0 ))
        , Benchmark.bench "RefV4.add / scale" (\() -> addRefV4 n (vec4 1 2 3 5) (vec4 -2 5 2.3 0))
        ]


addV4 n a b =
    if n <= 0 then
        ( a, b )
    else
        addV4 (n - 1) (V4.add a b) (V4.scale -0.999999 b)


addRefV4 n a b =
    if n <= 0 then
        ( a, b )
    else
        addRefV4 (n - 1) (RefV4.add a b) (RefV4.scale -0.999999 b)


rotateAround n =
    Benchmark.suiteWithOptions options
        ("Rotate vector around " ++ toString n ++ " times")
        [ Benchmark.bench "M4.transform" (\() -> rotateM4 n (M4.makeRotate 0.23 ( 0.5, 0.16, 0.8 )) ( 2, -3.2, 5 ))
        , Benchmark.bench "RefM4.transform" (\() -> rotateRefM4 n (RefM4.makeRotate 0.23 (vec3 0.5 0.16 0.8)) (vec3 2 -3.2 5))
        ]


rotateM4 n m v =
    if n <= 0 then
        v
    else
        rotateM4 (n - 1) m (M4.transform m v)


rotateRefM4 n m v =
    if n <= 0 then
        v
    else
        rotateRefM4 (n - 1) m (RefM4.transform m v)


pow n =
    Benchmark.suiteWithOptions options
        ("Matrix exponentiation: A^" ++ toString n)
        [ Benchmark.bench "M4.pow" (\() -> powM4 (M4.makeRotate 0.23 ( 0.5, 0.16, 0.8 )) n)
        , Benchmark.bench "RefM4.pow" (\() -> powRef4 (RefM4.makeRotate 0.23 (vec3 0.5 0.16 0.8)) n)
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
        makeTransformM4_ (M4.makeTransform ( 1, 2, 3 ) ( 3, 1, 2 ) 42.3 ( 4, 3, 2 ) ( -3, 2, 10 )) (n - 1)


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
