-- | Exact evaluation of the predicates that `assert` can state.
module Proof.Predicates
  ( Arg
  , Outcome
  , Detail
  , check
  , detailText
  , detailTypst
  , concyclicCentre
  ) where

import Prelude

import Constructible.Field (Construct)
import Constructible.Field as C
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (all)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String as String
import Geometry (Line, Point)
import Geometry as G
import Proof.Object (Kind(..), Obj(..), kindName, kindOf)
import Proof.Syntax (Cmp(..), PredF(..))
import Render.Pretty as Pretty

-- | A predicate argument: the object and the name it was referred to by.
type Arg = { name :: String, obj :: Obj }

-- | A value that decided a predicate, e.g. `|AB|` and its exact length.
type Detail = { label :: String, value :: Construct }

-- | Whether the predicate holds, with the values that decided it.
type Outcome = { holds :: Boolean, detail :: Array Detail }

-- | `|AB| = √3 ≈ 1.732, |AC| = 2`
detailText :: Array Detail -> String
detailText = joinWith ", " <<< map (\g -> joinWith " = " g.labels <> " = " <> Pretty.withApprox g.value) <<< sameValues

-- | The same as Typst math (without `$`), with the given radicand names.
detailTypst :: Map String String -> Array Detail -> String
detailTypst names =
  joinWith ", "
    <<< map (\g -> joinWith " = " (map quote g.labels) <> " = " <> Pretty.typstNamedWithApprox names g.value)
    <<< sameValues
  where
  -- a Typst string inside math
  quote str = "\""
    <> String.replaceAll (String.Pattern "\"") (String.Replacement "\\\"")
      (String.replaceAll (String.Pattern "\\") (String.Replacement "\\\\") str)
    <> "\""

-- | Adjacent details with the same value, shown as `|AB| = |CD| = v`.
sameValues :: Array Detail -> Array { labels :: Array String, value :: Construct }
sameValues = Array.foldl add []
  where
  add acc d = case Array.unsnoc acc of
    Just { init, last } | last.value == d.value -> Array.snoc init (last { labels = Array.snoc last.labels d.label })
    _ -> Array.snoc acc { labels: [ d.label ], value: d.value }

expected :: Kind -> Arg -> String
expected k a = a.name <> " is " <> kindName (kindOf a.obj) <> ", expected " <> kindName k

point :: Arg -> Either String Point
point a = case a.obj of
  OPoint p -> Right p
  _ -> Left (expected KPoint a)

line :: Arg -> Either String Line
line a = case a.obj of
  OLine l -> Right l
  _ -> Left (expected KLine a)

-- | The exact length |PQ|.
length :: Point -> Point -> Construct
length p q = case C.sqrt (G.dist2 p q) of
  Just l -> l
  Nothing -> zero -- unreachable: squared distances are non-negative

-- | `|AB|`, with a space between longer names (`|P0 P1|`).
segment :: Arg -> Arg -> String
segment a b = "|" <> a.name <> sep <> b.name <> "|"
  where
  sep = if String.length a.name == 1 && String.length b.name == 1 then "" else " "

holdsCmp :: Cmp -> Ordering -> Boolean
holdsCmp c o = case c of
  CEq -> o == EQ
  CNe -> o /= EQ
  CLt -> o == LT
  CLe -> o /= GT
  CGt -> o == GT
  CGe -> o /= LT

-- | Checks a predicate exactly. `Left` reports a malformed predicate (wrong
-- | kinds of objects, too few points, degenerate angles).
check :: PredF Arg -> Either String Outcome
check = case _ of
  On p x -> do
    pt <- point p
    case x.obj of
      OLine l -> pure { holds: G.onLine pt l, detail: [] }
      OCircle c -> pure (onCircle pt c)
      OPoint _ -> Left (x.name <> " is a point; 'on' expects a line or a circle")
  PointEq p q -> do
    a <- point p
    b <- point q
    pure { holds: G.samePoint a b, detail: [ { label: segment p q, value: length a b } ] }
  PointNe p q -> do
    a <- point p
    b <- point q
    pure { holds: not (G.samePoint a b), detail: [] }
  DistCmp c a b d e -> do
    pa <- point a
    pb <- point b
    pd <- point d
    pe <- point e
    let
      o = compare (G.dist2 pa pb) (G.dist2 pd pe)
    pure
      { holds: holdsCmp c o
      , detail: [ { label: segment a b, value: length pa pb }, { label: segment d e, value: length pd pe } ]
      }
  Collinear args -> do
    ps <- traverse' point args
    when (Array.length ps < 3) (Left "collinear needs at least three points")
    pure { holds: collinear ps, detail: [] }
  Concyclic args -> do
    ps <- traverse' point args
    when (Array.length ps < 4) (Left "concyclic needs at least four points")
    pure { holds: concyclic ps, detail: [] }
  Parallel l m -> do
    a <- (G.lineCoefficients <$> line l)
    b <- (G.lineCoefficients <$> line m)
    pure { holds: C.isZero (a.a * b.b - b.a * a.b), detail: [] }
  Perpendicular l m -> do
    a <- (G.lineCoefficients <$> line l)
    b <- (G.lineCoefficients <$> line m)
    pure { holds: C.isZero (a.a * b.a + a.b * b.b), detail: [] }
  AngleEq a b c d e f -> do
    pa <- point a
    pb <- point b
    pc <- point c
    pd <- point d
    pe <- point e
    pf <- point f
    when (G.samePoint pa pb || G.samePoint pc pb) (Left ("angle " <> a.name <> b.name <> c.name <> " is undefined"))
    when (G.samePoint pd pe || G.samePoint pf pe) (Left ("angle " <> d.name <> e.name <> f.name <> " is undefined"))
    let
      -- cos ∠ABC = dot / (|BA|·|BC|); compare cosines without square roots.
      dot1 = G.dot pb pa pc
      dot2 = G.dot pe pd pf
      n1 = G.dist2 pb pa * G.dist2 pb pc
      n2 = G.dist2 pe pd * G.dist2 pe pf
      equal = C.sign dot1 == C.sign dot2 && dot1 * dot1 * n2 == dot2 * dot2 * n1
    pure { holds: equal, detail: [] }
  Ccw a b c -> do
    pa <- point a
    pb <- point b
    pc <- point c
    pure
      { holds: G.orientation pa pb pc == GT
      , detail: [ { label: "(" <> b.name <> " − " <> a.name <> ") × (" <> c.name <> " − " <> a.name <> ")", value: G.cross pa pb pc } ]
      }
  Between a b c -> do
    pa <- point a
    pb <- point b
    pc <- point c
    let
      ok = C.isZero (G.cross pa pb pc) && C.sign (G.dot pb pa pc) == LT
    pure { holds: ok, detail: [] }
  where
  onCircle pt c =
    { holds: G.onCircle pt c
    , detail:
        [ { label: "distance to the centre", value: length pt (G.circleCenter c) }
        , { label: "radius", value: radiusOf c }
        ]
    }
  radiusOf c = case C.sqrt (G.circleRadius2 c) of
    Just r -> r
    Nothing -> zero

traverse' :: forall a b. (a -> Either String b) -> Array a -> Either String (Array b)
traverse' f = Array.foldM (\acc x -> Array.snoc acc <$> f x) []

-- | All points on one line (true if they are all equal).
collinear :: Array Point -> Boolean
collinear ps = case Array.uncons ps of
  Nothing -> true
  Just { head: p, tail } -> case Array.find (not <<< G.samePoint p) tail of
    Nothing -> true
    Just q -> all (\r -> C.isZero (G.cross p q r)) tail

-- | All points on one circle. Three distinct collinear points are on no
-- | circle; fewer than three distinct points are on many.
concyclic :: Array Point -> Boolean
concyclic ps =
  let
    distinct = Array.nubByEq G.samePoint ps
  in
    case Array.index distinct 0, Array.index distinct 1, Array.index distinct 2 of
      Just a, Just b, Just c
        | C.isZero (G.cross a b c) -> false
        | otherwise ->
            let
              o = circumcentre a b c
              r2 = G.dist2 o a
            in
              all (\p -> G.dist2 o p == r2) distinct
      _, _, _ -> true

-- | The centre of the circle through the first three distinct points, when
-- | they are not collinear (a hint for the Lean export).
concyclicCentre :: Array Point -> Maybe Point
concyclicCentre ps =
  case Array.index distinct 0, Array.index distinct 1, Array.index distinct 2 of
    Just a, Just b, Just c | not (C.isZero (G.cross a b c)) -> Just (circumcentre a b c)
    _, _, _ -> Nothing
  where
  distinct = Array.nubByEq G.samePoint ps

-- | The centre of the circle through three non-collinear points.
circumcentre :: Point -> Point -> Point -> Point
circumcentre a b c =
  let
    bx = b.x - a.x
    by = b.y - a.y
    cx = c.x - a.x
    cy = c.y - a.y
    d = (one + one) * (bx * cy - by * cx)
    b2 = bx * bx + by * by
    c2 = cx * cx + cy * cy
  in
    { x: a.x + (cy * b2 - by * c2) / d, y: a.y + (bx * c2 - cx * b2) / d }
