-- | The drawing of a construction as plain data in floating point. Rendered
-- | to Halogen HTML in the app and to an SVG string for the PDF, so both show
-- | the same figure.
module Render.Figure
  ( Role(..)
  , Item(..)
  , Viewport
  , Figure
  , Range
  , paletteSize
  , viewport
  , figure
  , newColours
  , zoomAt
  , panBy
  ) where

import Prelude

import Constructible.Field as C
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.Tuple (Tuple(..))
import Geometry as G
import Proof.Eval (Def(..), ObjInfo, Shown(..), Trace)
import Proof.Object (Obj(..))

-- | How an object is drawn: created in the current range (with a colour
-- | index), earlier, or as an auxiliary object of a macro expansion.
data Role = New Int | Old | Aux

derive instance Eq Role

-- | Labels carry their own position (`lx`, `ly`), computed here once for
-- | both renderers.
data Item
  = IPoint { x :: Number, y :: Number, label :: String, role :: Role, given :: Boolean }
  | ISegment
      { x1 :: Number
      , y1 :: Number
      , x2 :: Number
      , y2 :: Number
      , role :: Role
      , label :: String
      , lx :: Number
      , ly :: Number
      }
  | ICircle { cx :: Number, cy :: Number, r :: Number, role :: Role, label :: String, lx :: Number, ly :: Number }

-- | A rectangle in mathematical coordinates (y up).
type Viewport = { minX :: Number, minY :: Number, maxX :: Number, maxY :: Number }

type Figure = { view :: Viewport, items :: Array Item }

-- | The steps shown (`0 … upTo`) and those whose objects count as new
-- | (`newFrom … upTo`): one step in step mode, a page in page mode.
type Range = { upTo :: Int, newFrom :: Int, expanded :: Boolean }

-- | The number of distinct colours for new objects.
paletteSize :: Int
paletteSize = 8

num :: C.Construct -> Number
num = C.toNumber

-- | A square viewport that fits every named object of the whole construction,
-- | so the view does not move during playback.
viewport :: Trace -> Viewport
viewport t =
  let
    named = Array.filter _.named (Array.fromFoldable (Map.values t.objects))
    shownBoxes = Array.mapMaybe circleBox (shownItems t { upTo: Array.length t.steps, newFrom: Array.length t.steps + 1, expanded: false } 0)
    boxes = Array.mapMaybe box named <> shownBoxes
    start = { minX: -1.0, minY: -1.0, maxX: 1.0, maxY: 1.0 }
    union a b = { minX: min a.minX b.minX, minY: min a.minY b.minY, maxX: max a.maxX b.maxX, maxY: max a.maxY b.maxY }
    raw = case Array.uncons boxes of
      Nothing -> start
      Just { head, tail } -> foldl union head tail
    size = max (max (raw.maxX - raw.minX) (raw.maxY - raw.minY)) 1.0e-9
    cx = (raw.minX + raw.maxX) / 2.0
    cy = (raw.minY + raw.maxY) / 2.0
    half = size * 0.58
  in
    { minX: cx - half, minY: cy - half, maxX: cx + half, maxY: cy + half }
  where
  circleBox = case _ of
    ICircle c -> Just { minX: c.cx - c.r, minY: c.cy - c.r, maxX: c.cx + c.r, maxY: c.cy + c.r }
    _ -> Nothing

  box :: ObjInfo -> Maybe Viewport
  box o = case o.obj of
    OPoint p ->
      let
        x = num p.x
        y = num p.y
      in
        Just { minX: x, minY: y, maxX: x, maxY: y }
    OCircle c ->
      let
        o' = G.circleCenter c
        r = Number.sqrt (num (G.circleRadius2 c))
        x = num o'.x
        y = num o'.y
      in
        Just { minX: x - r, minY: y - r, maxX: x + r, maxY: y + r }
    OLine _ -> Nothing

-- | Zooms by `factor` (below 1 zooms in) keeping the point at the fractions
-- | `fx`, `fy` of the view (from the left and from the top) fixed.
zoomAt :: Number -> { fx :: Number, fy :: Number } -> Viewport -> Viewport
zoomAt factor at v =
  let
    w = v.maxX - v.minX
    h = v.maxY - v.minY
    mx = v.minX + at.fx * w
    my = v.maxY - at.fy * h
    w' = w * factor
    h' = h * factor
    minX = mx - at.fx * w'
    maxY = my + at.fy * h'
  in
    { minX, maxY, maxX: minX + w', minY: maxY - h' }

-- | Moves the drawing by the fractions `fx`, `fy` of the view (a drag to the
-- | right and down moves the drawing right and down).
panBy :: { fx :: Number, fy :: Number } -> Viewport -> Viewport
panBy d v =
  let
    dx = d.fx * (v.maxX - v.minX)
    dy = d.fy * (v.maxY - v.minY)
  in
    { minX: v.minX - dx, maxX: v.maxX - dx, minY: v.minY + dy, maxY: v.maxY + dy }

-- | The colour index of each object new in the range: named objects in
-- | creation order. A single new object gets index 0, the accent colour.
newColours :: Trace -> Range -> Map Int Int
newColours t r =
  let
    fresh = Array.filter (\o -> o.named && o.step >= r.newFrom && o.step <= r.upTo)
      (Array.sortWith _.id (Array.fromFoldable (Map.values t.objects)))
  in
    Map.fromFoldable (Array.mapWithIndex (\i o -> Tuple o.id (i `mod` paletteSize)) fresh)

figure :: Trace -> Viewport -> Range -> Figure
figure t view r =
  let
    colours = newColours t r
    all = Array.sortWith _.id (Array.fromFoldable (Map.values t.objects))
    classify o
      | o.step > r.upTo = Nothing
      | o.named = Just (Tuple (roleOf (Map.lookup o.id colours)) o)
      | r.expanded && o.step == r.upTo = Just (Tuple Aux o)
      | otherwise = Nothing
    visible = Array.mapMaybe classify all
    labels = mergedLabels (Array.mapMaybe (\(Tuple _ o) -> if o.named then Just o else Nothing) visible)
    relabel o = o { name = fromMaybe o.name (Map.lookup o.id labels) }
    rank (Tuple role o) = Tuple (roleRank role) (kindRank o)
    -- labels of lines and circles keep away from these
    points = Array.mapMaybe
      ( \(Tuple _ o) -> case o.obj of
          OPoint p -> Just { x: num p.x, y: num p.y }
          _ -> Nothing
      )
      visible
  in
    { view
    , items: shownItems t r (Map.size colours)
        <> Array.mapMaybe (\(Tuple role o) -> draw view points role (relabel o)) (Array.sortWith rank visible)
    }
  where
  roleOf = case _ of
    Just k -> New k
    Nothing -> Old
  roleRank = case _ of
    Aux -> 0
    Old -> 1
    New _ -> 2
  -- circles, then lines, then points on top
  kindRank o = case o.obj of
    OCircle _ -> 0
    OLine _ -> 1
    OPoint _ -> 2

-- | What `show` draws up to the range's last step, under the objects. Items
-- | shown in the range are new; they take the colours after those of the new
-- | objects (`firstColour` of them).
shownItems :: Trace -> Range -> Int -> Array Item
shownItems t r firstColour =
  let
    attached = Array.concatMap (\s -> map (Tuple s.index) s.shows) (Array.filter (\s -> s.index <= r.upTo) t.steps)
    pt id = case Map.lookup id t.objects of
      Just { obj: OPoint p } -> Just { x: num p.x, y: num p.y }
      _ -> Nothing
    segment role p q = ISegment { x1: p.x, y1: p.y, x2: q.x, y2: q.y, role, label: "", lx: 0.0, ly: 0.0 }
    circle role cx cy rad = ICircle { cx, cy, r: rad, role, label: "", lx: 0.0, ly: 0.0 }
    itemsOf role = case _ of
      ShownSegment a b -> Array.catMaybes [ segment role <$> pt a <*> pt b ]
      ShownPolygon ids ->
        let
          ps = Array.mapMaybe pt ids
        in
          Array.catMaybes (Array.zipWith (\p q -> Just (segment role p q)) ps (Array.drop 1 ps <> Array.take 1 ps))
      ShownCircle o a -> Array.catMaybes
        [ (\c p -> circle role c.x c.y (Number.sqrt ((p.x - c.x) * (p.x - c.x) + (p.y - c.y) * (p.y - c.y)))) <$> pt o <*> pt a ]
      ShownThrough a b c -> case pt a, pt b, pt c of
        Just pa, Just pb, Just pc -> case circumcircle pa pb pc of
          Just cc -> [ circle role cc.x cc.y cc.r ]
          Nothing -> []
        _, _, _ -> []
    go acc (Tuple i sh) =
      let
        isNew = i >= r.newFrom
        role = if isNew then New ((firstColour + acc.k) `mod` paletteSize) else Old
      in
        { k: if isNew then acc.k + 1 else acc.k, items: acc.items <> itemsOf role sh }
  in
    (foldl go { k: 0, items: [] } attached).items

-- | The circle through three points (Nothing if they are collinear).
circumcircle :: Pos -> Pos -> Pos -> Maybe { x :: Number, y :: Number, r :: Number }
circumcircle a b c =
  let
    bx = b.x - a.x
    by = b.y - a.y
    cx = c.x - a.x
    cy = c.y - a.y
    d = 2.0 * (bx * cy - by * cx)
    scale = max 1.0e-300 ((bx * bx + by * by) * (cx * cx + cy * cy))
  in
    if Number.abs d * Number.abs d < 1.0e-18 * scale then Nothing
    else
      let
        b2 = bx * bx + by * by
        c2 = cx * cx + cy * cy
        ux = (cy * b2 - by * c2) / d
        uy = (bx * c2 - cx * b2) / d
      in
        Just { x: a.x + ux, y: a.y + uy, r: Number.sqrt (ux * ux + uy * uy) }

-- | Points at the same position share one label, e.g. `P0 = P17`: the first
-- | (earliest) point carries it, the others get an empty label.
mergedLabels :: Array ObjInfo -> Map Int String
mergedLabels objs =
  let
    pts = Array.sortWith _.id (Array.mapMaybe asPoint objs)
    step acc o = case Array.find (\g -> G.samePoint g.p o.p) acc of
      Just g -> map (\h -> if h.id == g.id then h { label = h.label <> " = " <> o.name, others = Array.snoc h.others o.id } else h) acc
      Nothing -> Array.snoc acc { id: o.id, p: o.p, label: o.name, others: [] }
    groups = foldl step [] pts
  in
    Map.fromFoldable (Array.concatMap (\g -> [ Tuple g.id g.label ] <> map (\i -> Tuple i "") g.others) groups)
  where
  asPoint o = case o.obj of
    OPoint p -> Just { id: o.id, p, name: o.name }
    _ -> Nothing

type Pos = { x :: Number, y :: Number }

draw :: Viewport -> Array Pos -> Role -> ObjInfo -> Maybe Item
draw view points role o = case o.obj of
  OPoint p ->
    Just $ IPoint
      { x: num p.x
      , y: num p.y
      , label: if role == Aux then "" else o.name
      , role
      , given: case o.def of
          DGiven _ -> true
          _ -> false
      }
  OCircle c ->
    let
      ctr = G.circleCenter c
      cx = num ctr.x
      cy = num ctr.y
      r = Number.sqrt (num (G.circleRadius2 c))
    in
      Just $ case circleLabel view points cx cy r of
        Just at | role /= Aux -> ICircle { cx, cy, r, role, label: o.name, lx: at.x, ly: at.y }
        _ -> ICircle { cx, cy, r, role, label: "", lx: 0.0, ly: 0.0 }
  OLine l ->
    let
      k = G.lineCoefficients l
    in
      clip view (num k.a) (num k.b) (num k.c) <#> \s ->
        let
          at = lineLabel view points s
        in
          ISegment
            { x1: s.x1
            , y1: s.y1
            , x2: s.x2
            , y2: s.y2
            , role
            , label: if role == Aux then "" else o.name
            , lx: at.x
            , ly: at.y
            }

-- | The label offset, relative to the size of the view.
offset :: Viewport -> Number
offset v = 0.018 * max (v.maxX - v.minX) (v.maxY - v.minY)

-- | The candidate farthest from every point (the first on ties).
farthest :: Array Pos -> Array Pos -> Maybe Pos
farthest points candidates =
  let
    clearance c = foldl (\m p -> min m ((p.x - c.x) * (p.x - c.x) + (p.y - c.y) * (p.y - c.y))) 1.0e300 points
    better best c = case best of
      Just b | clearance b >= clearance c -> Just b
      _ -> Just c
  in
    foldl better Nothing candidates

-- | Near one end of the visible part of a line, on one side of it: the end
-- | and side that are farthest from the points.
lineLabel :: Viewport -> Array Pos -> { x1 :: Number, y1 :: Number, x2 :: Number, y2 :: Number } -> Pos
lineLabel v points s =
  let
    dx = s.x2 - s.x1
    dy = s.y2 - s.y1
    len = max (Number.sqrt (dx * dx + dy * dy)) 1.0e-12
    d = offset v
    nx = -dy / len * d
    ny = dx / len * d
    -- 6% in from either end
    ends = [ { x: s.x2 - 0.06 * dx, y: s.y2 - 0.06 * dy }, { x: s.x1 + 0.06 * dx, y: s.y1 + 0.06 * dy } ]
    candidates = Array.concatMap (\e -> [ { x: e.x + nx, y: e.y + ny }, { x: e.x - nx, y: e.y - ny } ]) ends
  in
    fromMaybe { x: s.x2 + nx, y: s.y2 + ny } (farthest points candidates)

-- | Just outside the circle, at the angle (a multiple of 45°) inside the view
-- | that is farthest from the points.
circleLabel :: Viewport -> Array Pos -> Number -> Number -> Number -> Maybe Pos
circleLabel v points cx cy r =
  let
    d = offset v
    inside p = p.x > v.minX && p.x < v.maxX && p.y > v.minY && p.y < v.maxY
    angles = map (\k -> Number.pi / 4.0 * Int.toNumber k) [ 1, 3, 7, 5, 2, 6, 0, 4 ]
    candidate a =
      { on: { x: cx + r * Number.cos a, y: cy + r * Number.sin a }
      , out: { x: cx + (r + d) * Number.cos a, y: cy + (r + d) * Number.sin a }
      }
    usable = map _.out (Array.filter (\c -> inside c.on && inside c.out) (map candidate angles))
  in
    farthest points usable

-- | The part of the line a·x + b·y + c = 0 inside the viewport
-- | (Liang–Barsky clipping of a long segment along the line).
clip :: Viewport -> Number -> Number -> Number -> Maybe { x1 :: Number, y1 :: Number, x2 :: Number, y2 :: Number }
clip v a b c =
  let
    n2 = a * a + b * b
    -- the foot of the perpendicular from the origin, and a unit direction
    px = -a * c / n2
    py = -b * c / n2
    len = Number.sqrt n2
    dx = -b / len
    dy = a / len
    reach = Number.abs px + Number.abs py + (v.maxX - v.minX) + (v.maxY - v.minY)
      + Number.abs v.minX
      + Number.abs v.minY
      + Number.abs v.maxX
      + Number.abs v.maxY
    -- p·t ≤ q for each of the four edges
    edges =
      [ { p: -dx, q: px - v.minX }
      , { p: dx, q: v.maxX - px }
      , { p: -dy, q: py - v.minY }
      , { p: dy, q: v.maxY - py }
      ]
    step acc e = case acc of
      Nothing -> Nothing
      Just r
        | e.p == 0.0 -> if e.q < 0.0 then Nothing else Just r
        | otherwise ->
            let
              t = e.q / e.p
            in
              if e.p < 0.0 then (if t > r.hi then Nothing else Just r { lo = max r.lo t })
              else (if t < r.lo then Nothing else Just r { hi = min r.hi t })
  in
    foldl step (Just { lo: -reach, hi: reach }) edges <#> \r ->
      { x1: px + r.lo * dx, y1: py + r.lo * dy, x2: px + r.hi * dx, y2: py + r.hi * dy }
