module Float2 exposing (..)

import Helper exposing (..)
import Test exposing (..)
import Math.Vector2 as Ref
import Vector2 as V


f2 =
    Ref.fromTuple



-- Note that the use of "expectAlmostEqualV2" automatically also tests length and sub


scale =
    fuzz2 smallFloat v2 "scale" <|
        \a v ->
            expectAlmostEqualV2 (V.scale a v) (Ref.scale a (f2 v))


add =
    fuzz2 v2 v2 "add" <|
        \a b ->
            expectAlmostEqualV2 (V.add a b) (Ref.add (f2 a) (f2 b))


dot =
    fuzz2 v2 v2 "dot" <|
        \a b ->
            expectAlmostEqual (V.dot a b) (Ref.dot (f2 a) (f2 b))


lengthSquared =
    fuzz v2 "lengthSquared" <|
        \v ->
            expectAlmostEqualErr 0.2 (V.lengthSquared v) (Ref.lengthSquared (f2 v))


normalize =
    fuzz v2 "normalize" <|
        \v ->
            expectAlmostEqualV2 (V.normalize v) (Ref.normalize (f2 v))


directionFromTo =
    fuzz2 v2 v2 "directionFromTo" <|
        \a b ->
            expectAlmostEqualV2 (V.directionFromTo a b) (Ref.direction (f2 b) (f2 a))


distance =
    fuzz2 v2 v2 "distance" <|
        \a b ->
            expectAlmostEqual (V.distance a b) (Ref.distance (f2 b) (f2 a))


distanceSquared =
    fuzz2 v2 v2 "distanceSquared" <|
        \a b ->
            expectAlmostEqualErr 0.3 (V.distanceSquared a b) (Ref.distanceSquared (f2 b) (f2 a))
