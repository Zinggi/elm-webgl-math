# webgl-math

This library provides functions for working with 2D and 3D vectors.
It includes standard vector and matrix math operations, transformations and camera projections.

Despite being named webgl-math, you can't directly use the types from this library with webgl. If you want to use this library with webgl, you need to convert these types with [this library](https://github.com/Zinggi/elm-webgl-types) (not published yet, as it is a native library).

## Overview

### Modules and Types

The library is organised in `VectorN` and `MatrixN` modules.  

Each module contains a more general type (`VecN a or MatN a`) with a limited set of functions to work with them and a specialized type (`FloatN or FloatNxN`) that only works with `Float`s with many functions to work with them.
All these types are just type aliases for tuples.  

All modules and types:
```elm
import Vector2 as V2 exposing (Vec2, Float2)
import Vector3 as V3 exposing (Vec3, Float3)
import Vector4 as V4 exposing (Vec4, Float4)
import Matrix2 as M2 exposing (Mat2, Float2x2)
import Matrix3 as M3 exposing (Mat3, Float3x3)
import Matrix4 as M4 exposing (Mat4, Float4x4)
```

### Usage
```elm
aVector : Float4
aVector =
    (12, 2, 4, 1)

aMatrix : Float4x4
aMatrix =
    ( (2, 0, 0, 2)
    , (0, 2, 0, 1)
    , (0, 0, 2, 3)
    , (0, 0, 0, 1)
    )

transformedVector : Float4
transformedVector =
    M4.mulVector aMatrix aVector    -- result: (26, 5, 11, 1)

--
-- The same, but much easier:
--

transformedVector2 =
    let
        transform =
            M4.makeTranslate ( 2, 1, 3 )
                |> M4.scale ( 2, 2, 2 )
    in
        ( 12, 2, 4 )
            |> M4.transform transform -- result: (26, 5, 11)
```


### Notes on ascii math
In the documentation, instead of code samples I sometimes use ascii math notation, as it is more compact.

        |a b| |x|
    A = |c d|*|y|

Would be

    ma = mulVector ((a,b),(c,d)) (x,y)

in Elm.

Generally, `a, b, c, ...` and `x, y, z` are usually **scalar** values.  
`v, w` are usually **vectors**.  
`A, B, C, ..` are usually **matrices**, `S, T, R` are **scale**, **translate** and **rotation** matrices.



## Plan
This is meant as a replacement for **elm-community/elm-linear-algebra**.

### Why replace elm-community/elm-linear-algebra, what's wrong with it?

1. It's not written in elm.
2. `Vec`/`Mat` are opaque types:
    - A vector type shouldn't be opaque, as it doesn't hide any complexity. A vector/matrix type should be as fundamental as a list. That's why I made my types to just be aliases for tuples. 
    - It should be possible to create vector/matrix literals. Tuples allow for that.
    - It should be possible to easily extract elements from a vector/matrix. With *elm-community/elm-linear-algebra*s implementation you have to make use of `toTuple`/`fromTuple` very often.
3. Missing functions from elm-community:
    - There is no `Mat2`/`Mat3` type. `Mat3` is very useful for 2d games.
    - There is no way to construct a matrix by specifying each element.
    - Matrix addition, element wise multiplication and matrix-vector multiplication is missing.

### Performance

This needs to be explored more, but first results look very promising.
Matrix multiplication is just as fast as in *elm-community/elm-linear-algebra* and some more complicated examples are much faster because we can directly construct matrices.

Run the benchmark for yourself!  
[Here](https://zinggi.github.io/elm-webgl-math/)/[src](/bench)

My library seems to be slightly faster on Chrome, much faster on Firefox, faster on Edge, and slightly slower on Safari.
If your results are completely different, please post them [here](https://github.com/Zinggi/elm-webgl-math/issues/2).
