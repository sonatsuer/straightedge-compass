module Test.Figure (spec) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Number as Number
import Partial.Unsafe (unsafeCrashWith)
import Proof.Eval (Trace, evaluateSource)
import Render.Figure (Item(..), Role(..), figure, panBy, viewport, zoomAt)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

trace :: String -> Trace
trace src = case evaluateSource src of
  Right t -> t
  Left e -> unsafeCrashWith e.message

close :: Number -> Number -> Boolean
close a b = Number.abs (a - b) < 1.0e-9

spec :: Spec Unit
spec = describe "Render.Figure" do
  let
    v = { minX: 0.0, minY: 0.0, maxX: 10.0, maxY: 10.0 }
  it "zoomAt keeps the point under the cursor fixed" do
    let
      z = zoomAt 0.5 { fx: 0.2, fy: 0.3 } v
    -- the point at 20% from the left, 30% from the top: (2, 7)
    [ close (z.minX + 0.2 * (z.maxX - z.minX)) 2.0, close (z.maxY - 0.3 * (z.maxY - z.minY)) 7.0 ]
      `shouldEqual` [ true, true ]
    close (z.maxX - z.minX) 5.0 `shouldEqual` true
  it "panBy moves the drawing with the pointer" do
    let
      p = panBy { fx: 0.1, fy: 0.2 } v
    [ p.minX, p.maxX, p.minY, p.maxY ] `shouldEqual` [ -1.0, 9.0, 2.0, 12.0 ]
  it "names lines and circles, and colours several new objects differently" do
    let
      t = trace "given A = (0, 0)\ngiven B = (1, 0)\nl = line A B\nc = circle A B\nC, D = meet l c"
      f = figure t (viewport t) { upTo: 4, newFrom: 2, expanded: false }
      labels = Array.mapMaybe
        ( case _ of
            ISegment s -> Just s.label
            ICircle c -> Just c.label
            _ -> Nothing
        )
        f.items
      roles = map
        ( case _ of
            IPoint p -> p.role
            ISegment s -> s.role
            ICircle c -> c.role
        )
        f.items
    Array.sort labels `shouldEqual` [ "c", "l" ]
    Array.length (Array.nubEq (Array.filter isNew roles)) `shouldEqual` 4
  it "draws segments, polygons and circles from show, without new steps" do
    let
      src = "given A = (0, 0)\ngiven B = (4, 0)\ngiven C = (0, 3)\nshow polygon A B C\nshow segment A B\nshow circle A B\nshow circle through A B C\n"
      t = trace src
      f = figure t (viewport t) { upTo: 2, newFrom: 2, expanded: false }
      segments = Array.mapMaybe
        ( case _ of
            ISegment s | s.label == "" -> Just (Tuple s.x1 s.x2)
            _ -> Nothing
        )
        f.items
      circles = Array.mapMaybe
        ( case _ of
            ICircle c -> Just c
            _ -> Nothing
        )
        f.items
    Array.length t.steps `shouldEqual` 3
    Array.length segments `shouldEqual` 4
    -- the circle about A through B, and the circle through A, B, C (centre (2, 3/2), radius 5/2)
    map (\c -> [ c.cx, c.cy, c.r ]) circles `shouldEqual` [ [ 0.0, 0.0, 4.0 ], [ 2.0, 1.5, 2.5 ] ]
    -- before the step they are attached to, nothing is shown
    Array.length (figure t (viewport t) { upTo: 1, newFrom: 1, expanded: false }).items `shouldEqual` 2
  it "rejects show of an unknown point" do
    case evaluateSource "given A = (0, 0)\nshow segment A Z\n" of
      Right t' -> map _.message t'.error `shouldEqual` Just "show segment A Z: Z is not defined"
      Left e -> unsafeCrashWith e.message
  where
  isNew = case _ of
    New _ -> true
    _ -> false
