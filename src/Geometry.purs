-- | Exact plane geometry over the constructible numbers.
-- |
-- | Lines are stored by an equation a·x + b·y + c = 0, computed from two
-- | points without division. Circles are stored by centre and squared radius.
-- | Intersections return their points in a fixed order (lexicographic by
-- | x, then y), so the order does not depend on the formulas used.
module Geometry
  ( Point
  , point
  , Line
  , Circle
  , Meet(..)
  , lineThrough
  , circleThrough
  , lineCoefficients
  , circleCenter
  , circleRadius2
  , meetLines
  , meetLineCircle
  , meetCircles
  , onLine
  , onCircle
  , dist2
  , cross
  , dot
  , orientation
  , samePoint
  , sameLine
  , sameCircle
  , comparePoints
  ) where

import Prelude

import Constructible.Field (Construct)
import Constructible.Field as C
import Data.Maybe (Maybe(..))

type Point = { x :: Construct, y :: Construct }

point :: Construct -> Construct -> Point
point x y = { x, y }

-- | a·x + b·y + c = 0 with (a, b) ≠ (0, 0).
newtype Line = Line { a :: Construct, b :: Construct, c :: Construct }

-- | Centre and squared radius (positive).
newtype Circle = Circle { center :: Point, r2 :: Construct }

-- | The intersection of two objects: no point, one point, two points (in
-- | lexicographic order), or infinitely many (the objects coincide).
data Meet = NoPoint | OnePoint Point | TwoPoints Point Point | Coincide

samePoint :: Point -> Point -> Boolean
samePoint p q = p.x == q.x && p.y == q.y

comparePoints :: Point -> Point -> Ordering
comparePoints p q = compare p.x q.x <> compare p.y q.y

ordered :: Point -> Point -> Meet
ordered p q = case comparePoints p q of
  GT -> TwoPoints q p
  _ -> TwoPoints p q

dist2 :: Point -> Point -> Construct
dist2 p q =
  let
    dx = q.x - p.x
    dy = q.y - p.y
  in
    dx * dx + dy * dy

-- | The z-component of (q − p) × (r − p): twice the signed area of pqr.
cross :: Point -> Point -> Point -> Construct
cross p q r = (q.x - p.x) * (r.y - p.y) - (q.y - p.y) * (r.x - p.x)

-- | (q − p) · (r − p)
dot :: Point -> Point -> Point -> Construct
dot p q r = (q.x - p.x) * (r.x - p.x) + (q.y - p.y) * (r.y - p.y)

-- | GT if p, q, r turn counterclockwise, LT if clockwise, EQ if collinear.
orientation :: Point -> Point -> Point -> Ordering
orientation p q r = C.sign (cross p q r)

-- | The line through two distinct points.
lineThrough :: Point -> Point -> Maybe Line
lineThrough p q
  | samePoint p q = Nothing
  | otherwise = Just $ normalise
      { a: p.y - q.y
      , b: q.x - p.x
      , c: p.x * q.y - q.x * p.y
      }

-- | Scales an equation so that its first non-zero coefficient is 1. This
-- | keeps the coefficients in the smallest field that contains the line:
-- | the points defining a line can carry square roots that cancel in the
-- | line itself (x = g through (g, ±√3·g) would otherwise keep √3).
normalise :: { a :: Construct, b :: Construct, c :: Construct } -> Line
normalise l
  | C.isZero l.a = Line { a: zero, b: one, c: l.c / l.b }
  | otherwise = Line { a: one, b: l.b / l.a, c: l.c / l.a }

-- | The circle with centre `o` through `p` (`o` ≠ `p`).
circleThrough :: Point -> Point -> Maybe Circle
circleThrough o p
  | samePoint o p = Nothing
  | otherwise = Just (Circle { center: o, r2: dist2 o p })

lineCoefficients :: Line -> { a :: Construct, b :: Construct, c :: Construct }
lineCoefficients (Line l) = l

circleCenter :: Circle -> Point
circleCenter (Circle c) = c.center

circleRadius2 :: Circle -> Construct
circleRadius2 (Circle c) = c.r2

onLine :: Point -> Line -> Boolean
onLine p (Line l) = C.isZero (l.a * p.x + l.b * p.y + l.c)

onCircle :: Point -> Circle -> Boolean
onCircle p (Circle c) = dist2 p c.center == c.r2

-- | Equal lines have proportional equations.
sameLine :: Line -> Line -> Boolean
sameLine (Line l) (Line m) =
  l.a * m.b == m.a * l.b && l.a * m.c == m.a * l.c && l.b * m.c == m.b * l.c

sameCircle :: Circle -> Circle -> Boolean
sameCircle (Circle c) (Circle d) = samePoint c.center d.center && c.r2 == d.r2

meetLines :: Line -> Line -> Meet
meetLines l0@(Line l) m0@(Line m) =
  let
    det = l.a * m.b - m.a * l.b
  in
    if C.isZero det then (if sameLine l0 m0 then Coincide else NoPoint)
    else OnePoint
      { x: (l.b * m.c - m.b * l.c) / det
      , y: (m.a * l.c - l.a * m.c) / det
      }

-- | Let n² = a² + b² and s = a·x₀ + b·y₀ + c for the centre (x₀, y₀). The
-- | foot of the perpendicular from the centre is F = centre − (s/n²)(a, b),
-- | and the intersections are F ± (√D/n²)(−b, a) with D = r²·n² − s².
meetLineCircle :: Line -> Circle -> Meet
meetLineCircle (Line l) (Circle c) =
  let
    o = c.center
    n2 = l.a * l.a + l.b * l.b
    s = l.a * o.x + l.b * o.y + l.c
    foot = { x: o.x - s * l.a / n2, y: o.y - s * l.b / n2 }
    d = c.r2 * n2 - s * s
  in
    case C.sign d of
      LT -> NoPoint
      EQ -> OnePoint foot
      GT -> case C.sqrt d of
        Nothing -> NoPoint -- unreachable: d > 0
        Just root ->
          let
            t = root / n2
          in
            ordered
              { x: foot.x - t * l.b, y: foot.y + t * l.a }
              { x: foot.x + t * l.b, y: foot.y - t * l.a }

-- | Two circles with different centres meet where either one meets their
-- | radical line 2(x₂−x₁)x + 2(y₂−y₁)y + (x₁²+y₁²−r₁²) − (x₂²+y₂²−r₂²) = 0.
meetCircles :: Circle -> Circle -> Meet
meetCircles c1@(Circle a) (Circle b)
  | samePoint a.center b.center = if a.r2 == b.r2 then Coincide else NoPoint
  | otherwise =
      let
        p = a.center
        q = b.center
        two = one + one
        radical = normalise
          { a: two * (q.x - p.x)
          , b: two * (q.y - p.y)
          , c: (p.x * p.x + p.y * p.y - a.r2) - (q.x * q.x + q.y * q.y - b.r2)
          }
      in
        meetLineCircle radical c1
