module Tests exposing (main)

import Test exposing (..)
import Test.Runner.Html
import Float2
import Float3
import Float4
import Float4x4


all =
    describe "all tests"
        [ Float2.all, Float3.all, Float4.all, Float4x4.all ]


main =
    Test.Runner.Html.run all
