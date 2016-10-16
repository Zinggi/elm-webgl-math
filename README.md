# elm-linear-algebra

This is not ready yet, it needs more test to make sure I didn't screw up somewhere and also more benchmarks.

## Plan
This is meant as a replacement for **elm-community/elm-linear-algebra**.

### Why replace elm-community/elm-linear-algebra, what's wrong with it?

1. It's not written in elm.
2. `Vec`/`Mat` are opaque types:
    - A vector type shouldn't be opaque, as it doesn't hide any complexity. A vector/matrix type should be as fundamental as a list. That's why I made my types to just be aliases for tuples. 
    - It should be possible to create vector/matrix literals. Tuples allow for that.
    - It should be possible to easily extract elements from a vector/matrix. With *elm-community/elm-linear-algebra*s implementation you have to make use of `toTuple`/`fromTuple` very often.
3. Missing functions:
    - There is no `Mat2`/`Mat3` type. `Mat3` is very useful for 2d games.
    - There is no way to construct a matrix by specifying each element.
    - Matrix addition, element wise multiplication and matrix-vector multiplication is missing. (Contrary to what the docs say, `transform` does **not** perform matrix-vector multiplication, it does `(x,y,z,1)*M` and then `(x/w,y/w,z/w)`)

### Performance

This needs to be explored more, but first results look very promising.
Matrix multiplication is just as fast as in *elm-community/elm-linear-algebra* and some more complicated examples are much faster because we can directly construct matrices.