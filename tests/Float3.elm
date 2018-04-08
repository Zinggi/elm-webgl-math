module Float3 exposing (..)

import Fuzz exposing (..)
import Helper exposing (..)
import Test exposing (..)
import Expect
import Math.Vector3 as Ref
import Vector3 as V


f2 =
    Ref.fromTuple



-- Note that the use of "expectAlmostEqualV3" automatically also tests length and sub


scale =
    fuzz2 smallFloat v3 "scale" <|
        \a v ->
            expectAlmostEqualV3 (V.scale a v) (Ref.scale a (f2 v))


add =
    fuzz2 v3 v3 "add" <|
        \a b ->
            expectAlmostEqualV3 (V.add a b) (Ref.add (f2 a) (f2 b))


dot =
    fuzz2 v3 v3 "dot" <|
        \a b ->
            expectAlmostEqual (V.dot a b) (Ref.dot (f2 a) (f2 b))


lengthSquared =
    fuzz v3 "lengthSquared" <|
        \v ->
            expectAlmostEqualErr 0.2 (V.lengthSquared v) (Ref.lengthSquared (f2 v))


normalize =
    fuzz v3 "normalize" <|
        \v ->
            expectAlmostEqualV3 (V.normalize v) (Ref.normalize (f2 v))


directionFromTo =
    fuzz2 v3 v3 "directionFromTo" <|
        \a b ->
            expectAlmostEqualV3 (V.directionFromTo a b) (Ref.direction (f2 b) (f2 a))


distance =
    fuzz2 v3 v3 "distance" <|
        \a b ->
            expectAlmostEqual (V.distance a b) (Ref.distance (f2 b) (f2 a))


distanceSquared =
    fuzz2 v3 v3 "distanceSquared" <|
        \a b ->
            expectAlmostEqualErr 0.3 (V.distanceSquared a b) (Ref.distanceSquared (f2 b) (f2 a))
