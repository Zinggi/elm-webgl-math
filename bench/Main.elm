module Main exposing (main)

import Benchmark
import Benchmark.Program
import Html.App
import Math.Vector4 as RefV4
import Math.Vector3 as RefV3 exposing (vec3)
import Math.Matrix4 as RefM4
import Math.Float4 as V4
import Math.Float4x4 as M4


main : Program Never
main =
    -- Run over range of sizes that should include some stack-overflow cases.
    Benchmark.Program.program
        [ suiteN 1000
        , suiteN 10000
        , suiteN 100000
        , suiteN 1000000
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


{-| Run the benchmark functions with different n.
-}
suiteN size =
    Benchmark.suiteWithOptions options
        ("size " ++ toString size)
        [ Benchmark.bench "M4.pow" (\() -> powM4 (M4.makeBasis ( 4, 2, 1 ) ( -4, 1, 8 ) ( -1, 2, 5 )) size)
        , Benchmark.bench "RefM4.pow" (\() -> powRef4 (RefM4.makeBasis (vec3 4 2 1) (vec3 -4 1 8) (vec3 -1 2 5)) size)
        ]


{-| Naive version of foldl that grows the call stack.
-}
newFoldl : (a -> b -> b) -> b -> List a -> b
newFoldl fn initial vals =
    case vals of
        [] ->
            initial

        first :: rest ->
            fn first (newFoldl fn initial rest)
