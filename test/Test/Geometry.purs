module Test.Geometry (spec) where

import Prelude

import Constructible.Field as C
import Data.Maybe (Maybe(..), fromJust)
import Geometry (Meet(..), Point)
import Geometry as G
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (Result, (<?>))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (chooseInt)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.QuickCheck (quickCheck')

pt :: Int -> Int -> Point
pt x y = G.point (C.fromInt x) (C.fromInt y)

line :: Point -> Point -> G.Line
line p q = unsafePartial (fromJust (G.lineThrough p q))

circle :: Point -> Point -> G.Circle
circle o p = unsafePartial (fromJust (G.circleThrough o p))

showMeet :: Meet -> String
showMeet = case _ of
  NoPoint -> "none"
  OnePoint p -> "one " <> showPt p
  TwoPoints p q -> "two " <> showPt p <> " " <> showPt q
  Coincide -> "coincide"
  where
  showPt p = "(" <> show p.x <> ", " <> show p.y <> ")"

-- | A small random lattice point.
newtype P = P Point

instance Arbitrary P where
  arbitrary = (\x y -> P (pt x y)) <$> chooseInt (-4) 4 <*> chooseInt (-4) 4

instance Show P where
  show (P p) = "(" <> show p.x <> ", " <> show p.y <> ")"

spec :: Spec Unit
spec = describe "Geometry" do

  describe "line and circle" do
    it "regression: circle at (1,0) radius 1 meets y = 0 at x = 0 and x = 2" do
      -- The 2019 Geometry.hs gave x = 0 and x = 1 here.
      showMeet (G.meetLineCircle (line (pt 0 0) (pt 1 0)) (circle (pt 1 0) (pt 2 0)))
        `shouldEqual` "two (0, 0) (2, 0)"
    it "tangent line meets in one point" do
      showMeet (G.meetLineCircle (line (pt (-1) 1) (pt 1 1)) (circle (pt 0 0) (pt 1 0)))
        `shouldEqual` "one (0, 1)"
    it "far line misses" do
      showMeet (G.meetLineCircle (line (pt (-1) 2) (pt 1 2)) (circle (pt 0 0) (pt 1 0)))
        `shouldEqual` "none"
    it "irrational intersections" do
      showMeet (G.meetLineCircle (line (pt 0 0) (pt 1 1)) (circle (pt 0 0) (pt 1 0)))
        `shouldEqual` "two (-1/2*sqrt 2, -1/2*sqrt 2) (1/2*sqrt 2, 1/2*sqrt 2)"

  describe "two lines" do
    it "meet in one point" do
      showMeet (G.meetLines (line (pt 0 0) (pt 2 2)) (line (pt 0 2) (pt 2 0))) `shouldEqual` "one (1, 1)"
    it "parallel lines do not meet" do
      showMeet (G.meetLines (line (pt 0 0) (pt 1 1)) (line (pt 0 1) (pt 1 2))) `shouldEqual` "none"
    it "the same line from different points coincides" do
      showMeet (G.meetLines (line (pt 0 0) (pt 1 1)) (line (pt 3 3) (pt (-2) (-2)))) `shouldEqual` "coincide"

  describe "two circles" do
    it "Euclid I.1: the apex of the equilateral triangle" do
      -- Compare values: the printed radicand depends on the formula used
      -- (here sqrt (3/4) rather than 1/2*sqrt 3).
      case G.meetCircles (circle (pt 0 0) (pt 1 0)) (circle (pt 1 0) (pt 0 0)) of
        TwoPoints p q -> do
          let
            h = unsafePartial (fromJust (C.sqrt (C.fromInt 3))) / C.fromInt 2
          [ p.x, p.y, q.x, q.y ] `shouldEqual` [ C.fromInts 1 2, negate h, C.fromInts 1 2, h ]
        m -> fail ("expected two points, got " <> showMeet m)
    it "externally tangent circles meet once" do
      showMeet (G.meetCircles (circle (pt 0 0) (pt 1 0)) (circle (pt 2 0) (pt 1 0))) `shouldEqual` "one (1, 0)"
    it "concentric circles: none or coincide" do
      showMeet (G.meetCircles (circle (pt 0 0) (pt 1 0)) (circle (pt 0 0) (pt 2 0))) `shouldEqual` "none"
      showMeet (G.meetCircles (circle (pt 0 0) (pt 1 0)) (circle (pt 0 0) (pt 0 1))) `shouldEqual` "coincide"

  describe "degenerate objects" do
    it "no line through a single point" do
      (const unit <$> G.lineThrough (pt 1 1) (pt 1 1)) `shouldEqual` Nothing
    it "no circle of radius 0" do
      (const unit <$> G.circleThrough (pt 1 1) (pt 1 1)) `shouldEqual` Nothing

  describe "properties" do
    it "line ∩ circle lies on both" $ quickCheck' 300 \(P a) (P b) (P o) (P p) ->
      case G.lineThrough a b, G.circleThrough o p of
        Just l, Just c -> onBoth (G.meetLineCircle l c) (\q -> G.onLine q l && G.onCircle q c)
        _, _ -> ok
    it "circle ∩ circle lies on both" $ quickCheck' 300 \(P a) (P b) (P o) (P p) ->
      case G.circleThrough a b, G.circleThrough o p of
        Just c, Just d -> onBoth (G.meetCircles c d) (\q -> G.onCircle q c && G.onCircle q d)
        _, _ -> ok
    it "line ∩ line lies on both" $ quickCheck' 300 \(P a) (P b) (P o) (P p) ->
      case G.lineThrough a b, G.lineThrough o p of
        Just l, Just m -> onBoth (G.meetLines l m) (\q -> G.onLine q l && G.onLine q m)
        _, _ -> ok
    it "the defining points lie on their objects" $ quickCheck' 300 \(P a) (P b) ->
      case G.lineThrough a b, G.circleThrough a b of
        Just l, Just c -> (G.onLine a l && G.onLine b l && G.onCircle b c) <?> "not incident"
        _, _ -> ok
    it "two intersection points are distinct and ordered" $ quickCheck' 300 \(P a) (P b) (P o) (P p) ->
      case G.circleThrough a b, G.circleThrough o p of
        Just c, Just d -> case G.meetCircles c d of
          TwoPoints q r -> (G.comparePoints q r == LT) <?> "not ordered"
          _ -> ok
        _, _ -> ok
  where
  ok = true <?> ""

onBoth :: Meet -> (Point -> Boolean) -> Result
onBoth m test = case m of
  OnePoint q -> test q <?> ("point not on both: " <> showMeet m)
  TwoPoints q r -> (test q && test r) <?> ("points not on both: " <> showMeet m)
  _ -> true <?> ""
