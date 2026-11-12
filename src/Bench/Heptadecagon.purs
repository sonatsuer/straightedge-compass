-- | Phase 3 spike: the regular 17-gon by Carlyle circles, with exact checks.
-- |
-- | Every point comes from straightedge-and-compass primitives only (lines
-- | through two points, circles with a centre through a point, and their
-- | intersections). Run with `npm run bench`.
-- |
-- | Gauss's periods, with c_k = 2cos(2πk/17):
-- |   η₀, η₁ = c₁+c₂+c₄+c₈, c₃+c₅+c₆+c₇   roots of x² + x − 4
-- |   μ₀, μ₁ = c₁+c₄, c₂+c₈              roots of x² − η₀x − 1
-- |   ν₀, ν₁ = c₃+c₅, c₆+c₇              roots of x² − η₁x − 1
-- |   c₁, c₄                             roots of x² − μ₀x + ν₀
-- | The Carlyle circle of x² − sx + p has diameter (0,1)–(s,p); its
-- | intersections with the x-axis are the roots.
module Bench.Heptadecagon (main) where

import Prelude

import Constructible.Field as C
import Data.Array ((..))
import Data.Array as Array
import Data.Foldable (all, foldl, maximum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromJust)
import Effect (Effect)
import Effect.Console (log)
import Geometry (Circle, Line, Meet(..), Point)
import Geometry as G
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

foreign import nowMs :: Effect Number

-------------------------------------------------------------------------------
-- Primitive steps

line :: Point -> Point -> Line
line p q = unsafePartial (fromJust (G.lineThrough p q))

circle :: Point -> Point -> Circle
circle o p = unsafePartial (fromJust (G.circleThrough o p))

two :: String -> Meet -> { lo :: Point, hi :: Point }
two _ (TwoPoints p q) = { lo: p, hi: q }
two what _ = unsafeCrashWith (what <> ": expected two intersection points")

single :: String -> Meet -> Point
single _ (OnePoint p) = p
single what _ = unsafeCrashWith (what <> ": expected one intersection point")

-- | The intersection point other than `p`.
other :: String -> Point -> Meet -> Point
other what p m =
  let
    r = two what m
  in
    if G.samePoint r.lo p then r.hi else r.lo

upper :: String -> Meet -> Point
upper what m =
  let
    r = two what m
  in
    if r.lo.y > r.hi.y then r.lo else r.hi

-------------------------------------------------------------------------------
-- Derived constructions (these become prelude macros)

perpBisector :: Point -> Point -> Line
perpBisector p q =
  let
    r = two "perpBisector" (G.meetCircles (circle p q) (circle q p))
  in
    line r.lo r.hi

midpoint :: Point -> Point -> Point
midpoint p q = single "midpoint" (G.meetLines (perpBisector p q) (line p q))

-- | The perpendicular to `l` at its point `p`, using another point `q` of `l`.
perpAt :: Line -> Point -> Point -> Line
perpAt l p q =
  let
    r = two "perpAt" (G.meetLineCircle l (circle p q))
  in
    perpBisector r.lo r.hi

-------------------------------------------------------------------------------

type Step = { name :: String, points :: Array Point }

timed :: forall a. String -> (Unit -> a) -> (a -> Array Point) -> Effect a
timed name f pts = do
  t0 <- nowMs
  let
    x = f unit
    ps = pts x
    depthOf = maximum (map (\p -> max (C.depth p.x) (C.depth p.y)) ps)
  -- Force the coordinates (strict language, but keep it explicit).
  t1 <- nowMs
  log (pad 28 name <> show (round1 (t1 - t0)) <> " ms   depth " <> showDepth depthOf)
  pure x
  where
  showDepth (Just d) = show d
  showDepth Nothing = "-"

round1 :: Number -> Number
round1 x = toNumber (floorInt (x * 10.0)) / 10.0

foreign import floorInt :: Number -> Int

pad :: Int -> String -> String
pad k s = s <> foldl (\acc _ -> acc <> " ") "" (1 .. (k - len s))
  where
  len = stringLength

foreign import stringLength :: String -> Int

main :: Effect Unit
main = do
  start <- nowMs
  let
    o = G.point zero zero
    p0 = G.point one zero
  xAxis <- timed "x-axis, unit circle" (\_ -> line o p0) (const [])
  let
    unit = circle o p0
  pm <- timed "P' = (-1, 0)" (\_ -> other "P'" p0 (G.meetLineCircle xAxis unit)) pure'
  yAxis <- timed "y-axis" (\_ -> perpBisector pm p0) (const [])
  a <- timed "A = (0, 1)" (\_ -> upper "A" (G.meetLineCircle yAxis unit)) pure'
  h <- timed "H = (-1/2, 0)" (\_ -> midpoint o pm) pure'
  q <- timed "Q = (-1/4, 0)" (\_ -> midpoint o h) pure'
  -- u² + u/2 − 1 = 0 has roots η/2: Carlyle circle centred at (−1/4, 0).
  wv <- timed "W, V = (eta/2, 0)" (\_ -> two "C1" (G.meetLineCircle xAxis (circle q a))) (\r -> [ r.lo, r.hi ])
  let
    w = wv.hi
    v = wv.lo
  -- x² − ηx − 1: Carlyle circles centred at (η/2, 0) through A.
  mu <- timed "mu0 = (c1 + c4, 0)" (\_ -> (two "C2" (G.meetLineCircle xAxis (circle w a))).hi) pure'
  nu <- timed "nu0 = (c3 + c5, 0)" (\_ -> (two "C3" (G.meetLineCircle xAxis (circle v a))).hi) pure'
  -- B = (μ₀, ν₀), then the Carlyle circle of x² − μ₀x + ν₀.
  nuY <- timed "(0, nu0)" (\_ -> upper "nuY" (G.meetLineCircle yAxis (circle o nu))) pure'
  b <- timed "B = (mu0, nu0)" (\_ -> single "B" (G.meetLines (perpAt xAxis mu o) (perpAt yAxis nuY o))) pure'
  d <- timed "D = midpoint A B" (\_ -> midpoint a b) pure'
  x1 <- timed "X1 = (c1, 0)" (\_ -> (two "C4" (G.meetLineCircle xAxis (circle d a))).hi) pure'
  g <- timed "G = (cos(2pi/17), 0)" (\_ -> midpoint o x1) pure'
  p1 <- timed "P1" (\_ -> upper "P1" (G.meetLineCircle (perpAt xAxis g o) unit)) pure'
  -- Step the chord around: the circle centred P_k through P_{k−1} meets the
  -- unit circle again in P_{k+1}.
  let
    next ps = case Array.unsnoc ps of
      Just { init, last } -> case Array.last init of
        Just prev -> Array.snoc ps (other "step" prev (G.meetCircles (circle last prev) unit))
        Nothing -> ps
      Nothing -> ps
  polygon <- timed "P2 .. P17" (\_ -> foldl (\ps _ -> next ps) [ p0, p1 ] (2 .. 17)) identity
  -- Checks
  let
    vertex k = unsafePartial (fromJust (Array.index polygon k))
    chord = G.dist2 p0 p1
  onUnit <- timed "check: on unit circle" (\_ -> all (\k -> G.onCircle (vertex k) unit) (0 .. 16)) (const [])
  chords <- timed "check: equal chords" (\_ -> all (\k -> G.dist2 (vertex k) (vertex (k + 1)) == chord) (0 .. 16)) (const [])
  closes <- timed "check: P17 = P0" (\_ -> G.samePoint (vertex 17) p0) (const [])
  distinct <- timed "check: distinct" (\_ -> all (\k -> all (\l -> k == l || not (G.samePoint (vertex k) (vertex l))) (0 .. 16)) (0 .. 16)) (const [])
  -- Counterclockwise turns at consecutive vertices hold for the star polygons
  -- {17/k} too; all other vertices left of P0P1 holds only for k = 1.
  convex <- timed "check: convex (left of P0P1)" (\_ -> all (\j -> G.orientation p0 p1 (vertex j) == GT) (2 .. 16)) (const [])
  end <- nowMs
  log ""
  log ("on unit circle: " <> show onUnit)
  log ("equal chords:   " <> show chords)
  log ("closes:         " <> show closes)
  log ("distinct:       " <> show distinct)
  log ("convex:         " <> show convex)
  log ("P1 = (" <> show (C.toNumber p1.x) <> ", " <> show (C.toNumber p1.y) <> ")")
  log ("total " <> show (round1 (end - start)) <> " ms")
  where
  pure' p = [ p ]
