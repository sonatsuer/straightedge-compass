-- | The core export read by the Lean elaborator (see spec/core-format.md).
-- |
-- | It lists every primitive object in creation order with its definition,
-- | the assertions with their arguments resolved to object ids, and the
-- | exact coordinates of every point as untrusted hints: elements of one
-- | common tower of square roots, written as integer trees over a single
-- | denominator so that Lean's kernel needs no gcd to check them.
module Export.Core
  ( version
  , document
  , documentJson
  ) where

import Prelude

import Constructible.Field (Elt(..), Field(..))
import Constructible.Field as C
import Constructible.Rational (Rational)
import Constructible.Rational as Q
import Data.Argonaut.Core (Json, fromArray, fromNumber, fromObject, fromString, jsonNull, stringifyWithIndent)
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Proof.Predicates as P
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Proof.Eval (Choice(..), Def(..), ObjInfo, RefPt(..), Trace)
import Proof.Object (Obj(..))
import Proof.Syntax (Cmp, PredF(..), RatPoint, showCmp)

version :: Int
version = 1

obj :: Array (Tuple String Json) -> Json
obj = fromObject <<< Object.fromFoldable

int :: Int -> Json
int = fromNumber <<< Int.toNumber

ints :: Array Int -> Json
ints = fromArray <<< map int

rat :: Rational -> Json
rat = fromString <<< show

ratPoint :: RatPoint -> Json
ratPoint p = obj [ Tuple "x" (rat p.x), Tuple "y" (rat p.y) ]

-------------------------------------------------------------------------------
-- Elements of the common tower

-- | The rational leaves of an element.
leaves :: Elt -> Array Rational
leaves = case _ of
  R q -> [ q ]
  Zero -> []
  E a b -> leaves a <> leaves b

-- | `{ "num": tree, "den": d }`: the element is tree / d, where a tree is an
-- | integer string (a rational multiple of 1) or a pair [a, b] standing for
-- | a + b·√r at the corresponding tower level.
element :: Elt -> Json
element e =
  let
    den = foldl (\acc q -> Q.lcmInt acc (Q.denominator q)) one (leaves e)
    tree = case _ of
      R q -> fromString (Q.numeratorString (q * den))
      Zero -> fromString "0"
      E a b -> fromArray [ tree a, tree b ]
  in
    obj [ Tuple "num" (tree e), Tuple "den" (rat den) ]

-- | The radicands of a tower, from the bottom up.
radicands :: Field -> Array Elt
radicands = case _ of
  QF -> []
  Sqrt k r -> Array.snoc (radicands k) r

scaleElt :: Rational -> Elt -> Elt
scaleElt c = case _ of
  R q -> R (q * c)
  Zero -> Zero
  E a b -> E (scaleElt c a) (scaleElt c b)

-- | Rescales the generators so that every radicand has integer leaves: with
-- | r = R/d at level k (d the common denominator of its leaves), the new
-- | generator is d·√r = √(d²·r). An element a + b·√r becomes a + (b/d)·(d√r).
-- | Then products of integer trees stay integer trees (no denominators
-- | inside a tree), which keeps the Lean kernel's arithmetic in `Int`.
integralTower :: Field -> { radicands :: Array Elt, rescale :: Elt -> Elt }
integralTower field =
  foldl level { radicands: [], rescale: identity, scales: [] } (radicands field) # \r ->
    { radicands: r.radicands, rescale: r.rescale }
  where
  level acc r =
    let
      r' = acc.rescale r
      d = foldl (\m q -> Q.lcmInt m (Q.denominator q)) one (leaves r')
      scales = Array.snoc acc.scales d
    in
      { radicands: Array.snoc acc.radicands (scaleElt (d * d) r')
      , scales
      , rescale: rescaleWith scales
      }
  -- `scales` holds d₁ … d_k for levels 1 … k (bottom up).
  rescaleWith scales e = go (Array.length scales) e
    where
    go k = case _ of
      R q -> R q
      Zero -> Zero
      E a b -> case Array.index scales (k - 1) of
        Just d -> E (go (k - 1) a) (scaleElt (one / d) (go (k - 1) b))
        Nothing -> E a b

-------------------------------------------------------------------------------

document :: Trace -> String
document = stringifyWithIndent 1 <<< documentJson

documentJson :: Trace -> Json
documentJson t =
  let
    infos = Array.sortWith _.id (Array.fromFoldable (Map.values t.objects))
    -- Every coordinate pair that appears, in object order: each point, and
    -- the other point of each two-point intersection.
    coords = Array.concatMap pointsOf infos
    -- the centres hinted for concyclic assertions, in assertion order
    centres = Array.catMaybes (Array.mapWithIndex centreOf t.asserts)
    -- literal points used to choose an intersection point (`nearest (0, 1)`)
    literals = Array.mapMaybe literalOf infos
    common = C.joinAll
      (Array.concatMap (\p -> [ p.x, p.y ]) (coords <> map _.centre centres <> map _.point literals))
    integral = integralTower common.field
    hints = pairs (map integral.rescale common.elts)
    centreHints = Map.fromFoldable
      ( Array.mapWithIndex (\i c -> Tuple c.index (map hintJson (Array.index hints (Array.length coords + i))))
          centres
      )
    literalHints = Map.fromFoldable
      ( Array.mapWithIndex
          (\i l -> Tuple l.id (map hintJson (Array.index hints (Array.length coords + Array.length centres + i))))
          literals
      )
    -- Walk the objects again, taking their coordinates from `hints`.
    step acc o =
      let
        take k = Array.slice acc.next (acc.next + k) hints
      in
        case o.obj, o.def of
          OPoint _, DMeet _ _ _ (Just _) -> case take 2 of
            [ p, q ] -> { next: acc.next + 2, out: Map.insert o.id { coords: hintJson p, other: hintJson q } acc.out }
            _ -> acc
          OPoint _, _ -> case take 1 of
            [ p ] -> { next: acc.next + 1, out: Map.insert o.id { coords: hintJson p, other: jsonNull } acc.out }
            _ -> acc
          _, _ -> acc
    table = (foldl step { next: 0, out: Map.empty } infos).out
  in
    obj
      [ Tuple "format" (fromString "straightedge-compass-core")
      , Tuple "version" (int version)
      , Tuple "title" (maybe' fromString t.title)
      , Tuple "givens" (int t.givens)
      , Tuple "tower" (fromArray (map element integral.radicands))
      , Tuple "objects" (fromArray (map (\o -> objectJson (Map.lookup o.id table) (join (Map.lookup o.id literalHints)) o) infos))
      , Tuple "asserts"
          (fromArray (Array.mapWithIndex (\i a -> assertJson (join (Map.lookup i centreHints)) a) t.asserts))
      , Tuple "steps" (fromArray (map stepJson t.steps))
      ]
  where
  maybe' f = case _ of
    Just x -> f x
    Nothing -> jsonNull
  hintJson e = obj [ Tuple "x" (element e.x), Tuple "y" (element e.y) ]
  pairs xs = case Array.uncons xs of
    Just { head: x, tail } -> case Array.uncons tail of
      Just { head: y, tail: rest } -> Array.cons { x, y } (pairs rest)
      Nothing -> []
    Nothing -> []
  centreOf index a = case a.pred of
    Concyclic ids ->
      let
        pts = Array.mapMaybe (\i -> Map.lookup i t.objects >>= asPoint) ids
      in
        (\c -> { index, centre: c }) <$> P.concyclicCentre pts
    _ -> Nothing
  asPoint o = case o.obj of
    OPoint p -> Just p
    _ -> Nothing
  literalOf o = case o.def of
    DMeet _ _ (CExcept (RefRat p)) _ -> Just { id: o.id, point: ratPoint' p }
    DMeet _ _ (CNearest (RefRat p)) _ -> Just { id: o.id, point: ratPoint' p }
    DMeet _ _ (CFarthest (RefRat p)) _ -> Just { id: o.id, point: ratPoint' p }
    _ -> Nothing
  ratPoint' p = { x: C.fromRational p.x, y: C.fromRational p.y }
  pointsOf o = case o.obj, o.def of
    OPoint p, DMeet _ _ _ (Just q) -> [ p, q ]
    OPoint p, _ -> [ p ]
    _, _ -> []
  stepJson s = obj
    [ Tuple "line" (int s.line)
    , Tuple "text" (fromString s.text)
    , Tuple "objects" (ints s.objects)
    , Tuple "notes" (fromArray (map fromString s.notes))
    ]

type Hints = { coords :: Json, other :: Json }

objectJson :: Maybe Hints -> Maybe Json -> ObjInfo -> Json
objectJson hints literal o =
  obj
    ( [ Tuple "id" (int o.id)
      , Tuple "name" (fromString o.name)
      , Tuple "path" (fromArray (map fromString o.path))
      , Tuple "step" (int o.step)
      , Tuple "named" (if o.named then fromString "yes" else fromString "no")
      , Tuple "kind" (fromString kind)
      , Tuple "def" (defJson (maybe jsonNull _.other hints) literal o.def)
      ]
        <> case o.obj, hints of
          OPoint _, Just h -> [ Tuple "coords" h.coords ]
          _, _ -> []
    )
  where
  kind = case o.obj of
    OPoint _ -> "point"
    OLine _ -> "line"
    OCircle _ -> "circle"

defJson :: Json -> Maybe Json -> Def -> Json
defJson otherHint literal = case _ of
  DGiven p -> obj [ Tuple "given" (ratPoint p) ]
  DLine a b -> obj [ Tuple "line" (ints [ a, b ]) ]
  DCircle o p -> obj [ Tuple "circle" (ints [ o, p ]) ]
  -- not exported: scripts with free points are refused before this
  DPointOn o t -> obj [ Tuple "pointOn" (ints [ o ]), Tuple "t" (rat t) ]
  DMeet a b choice other -> obj
    [ Tuple "meet" (ints [ a, b ])
    , Tuple "choice" (choiceJson literal choice)
    , Tuple "other" case other of
        Just _ -> otherHint
        Nothing -> jsonNull
    ]

-- | A reference point: an object, or a literal with its coordinates as a
-- | hint in the common tower.
refJson :: Maybe Json -> RefPt -> Json
refJson literal = case _ of
  RefObj i -> obj [ Tuple "object" (int i) ]
  RefRat p -> obj ([ Tuple "point" (ratPoint p) ] <> maybe [] (\h -> [ Tuple "hint" h ]) literal)

choiceJson :: Maybe Json -> Choice -> Json
choiceJson literal = case _ of
  CUnique -> obj [ Tuple "kind" (fromString "unique") ]
  CLexFirst -> obj [ Tuple "kind" (fromString "lexFirst") ]
  CLexSecond -> obj [ Tuple "kind" (fromString "lexSecond") ]
  CExcept r -> obj [ Tuple "kind" (fromString "except"), Tuple "ref" (refJson literal r) ]
  CNearest r -> obj [ Tuple "kind" (fromString "nearest"), Tuple "ref" (refJson literal r) ]
  CFarthest r -> obj [ Tuple "kind" (fromString "farthest"), Tuple "ref" (refJson literal r) ]
  CLeft a b -> obj [ Tuple "kind" (fromString "left"), Tuple "line" (ints [ a, b ]) ]
  CRight a b -> obj [ Tuple "kind" (fromString "right"), Tuple "line" (ints [ a, b ]) ]

assertJson :: Maybe Json -> { pred :: PredF Int, text :: String, step :: Int, line :: Int, context :: Array String } -> Json
assertJson centre a = obj
  ( [ Tuple "pred" (predJson a.pred)
    , Tuple "text" (fromString a.text)
    , Tuple "step" (int a.step)
    , Tuple "context" (fromArray (map fromString a.context))
    ]
      <> case centre of
        Just c -> [ Tuple "centre" c ]
        Nothing -> []
  )

predJson :: PredF Int -> Json
predJson = case _ of
  On p x -> k "on" [ p, x ]
  PointEq p q -> k "eq" [ p, q ]
  PointNe p q -> k "ne" [ p, q ]
  DistCmp c a b d e -> cmpK c [ a, b, d, e ]
  Collinear ps -> k "collinear" ps
  Concyclic ps -> k "concyclic" ps
  Parallel l m -> k "parallel" [ l, m ]
  Perpendicular l m -> k "perpendicular" [ l, m ]
  AngleEq a b c d e f -> k "angle" [ a, b, c, d, e, f ]
  Ccw a b c -> k "ccw" [ a, b, c ]
  Between a b c -> k "between" [ a, b, c ]
  where
  k kind args = obj [ Tuple "kind" (fromString kind), Tuple "args" (ints args) ]

  cmpK :: Cmp -> Array Int -> Json
  cmpK c args = obj [ Tuple "kind" (fromString "dist"), Tuple "cmp" (fromString (showCmp c)), Tuple "args" (ints args) ]
