-- | Evaluation of proof scripts into a trace.
-- |
-- | Every object gets a global id when it is created and records how it was
-- | made (`Def`), so the trace contains both the step-by-step view used for
-- | playback and the PDF, and the flat list of primitive constructions used
-- | by the Lean export. Evaluation stops at the first error; the steps before
-- | it remain in the trace.
module Proof.Eval
  ( ObjId
  , RefPt(..)
  , Choice(..)
  , Def(..)
  , ObjInfo
  , Verdict
  , SubStep
  , StepKind(..)
  , Step
  , AssertInfo
  , EvalError
  , Trace
  , Valuation
  , PathChoice
  , Shown(..)
  , Frame
  , parameterNear
  , pointAt
  , evaluate
  , evaluateAt
  , evaluateSource
  , objectAt
  , isGeneral
  , Page
  , pages
  ) where

import Prelude

import Constructible.Field as C
import Constructible.Rational (Rational)
import Constructible.Rational as Q
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, get, modify_, runState)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldM, foldl, for_)
import Data.Int as Int
import Data.Number as Number
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Set (Set)
import Data.Set as Set
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Geometry (Meet(..), Point)
import Geometry as G
import Proof.Object (Obj(..), kindName, kindOf)
import Proof.Parser (ParseError, parseScript)
import Proof.Predicates as P
import Proof.Prelude as Prelude
import Proof.Syntax (Branch, CaseForm(..), Coord(..), Expr(..), Label(..), Located, Macro, Name, PredF(..), RatPoint, Ref(..), SampleItem, SampleValue(..), Script, Selector(..), ShowItem(..), Stmt(..), labelText, showItemText, showPred)

type ObjId = Int

-- | A point used to choose between intersections.
data RefPt = RefObj ObjId | RefRat RatPoint

-- | How an intersection point was chosen.
data Choice
  = CUnique -- ^ the only intersection point
  | CLexFirst -- ^ the lexicographically smaller of two (pair binding)
  | CLexSecond -- ^ the larger of two
  | CExcept RefPt
  | CNearest RefPt
  | CFarthest RefPt
  | CLeft ObjId ObjId
  | CRight ObjId ObjId

data Def
  = DGiven RatPoint
  | DLine ObjId ObjId
  | DCircle ObjId ObjId
  -- | The intersection of two objects, the choice, and the other
  -- | intersection point when there are two.
  | DMeet ObjId ObjId Choice (Maybe Point)
  -- | A free point on a line or circle, at the parameter t.
  | DPointOn ObjId Rational

type ObjInfo =
  { id :: ObjId
  , obj :: Obj
  , name :: String
  , path :: Array String -- macro calls it was created in
  , step :: Int -- top-level step that created it
  , def :: Def
  , named :: Boolean -- bound to a top-level name
  }

type Verdict = { text :: String, holds :: Boolean, detail :: Array P.Detail, context :: Array String }

-- | A statement inside a macro expansion.
type SubStep = { depth :: Int, text :: String, objects :: Array ObjId, verdicts :: Array Verdict }

data StepKind = SGiven | SConstruct | SAssert | SAssume | SCase

derive instance Eq StepKind

type Step =
  { index :: Int
  , line :: Int
  , text :: String
  , kind :: StepKind
  , objects :: Array ObjId -- bound to the step's names
  , aux :: Array ObjId -- created inside macro expansions
  , verdicts :: Array Verdict
  , substeps :: Array SubStep
  , notes :: Array String
  , macro :: Maybe Name
  , snapshot :: Boolean -- a PDF page ends after this step
  , samples :: Map Name SampleValue -- the script's sample values in force here
  , shows :: Array Shown -- drawn from this step on (`show`)
  }

-- | Something drawn by `show`, with the points it uses.
data Shown
  = ShownSegment ObjId ObjId
  | ShownPolygon (Array ObjId)
  | ShownCircle ObjId ObjId -- centre, a point on it
  | ShownThrough ObjId ObjId ObjId

-- | An assertion with its arguments resolved to object ids.
type AssertInfo = { pred :: PredF ObjId, text :: String, step :: Int, line :: Int, context :: Array String }

type EvalError = { line :: Int, message :: String, context :: Array String }

type Trace =
  { title :: Maybe String
  , intro :: Array String
  , steps :: Array Step
  , objects :: Map ObjId ObjInfo
  , asserts :: Array AssertInfo
  , scope :: Map Name ObjId
  , givens :: Int
  , macros :: Array Macro -- defined in the script
  , used :: Array Name -- macros called, directly or not
  , error :: Maybe EvalError
  -- universal statements
  , parametric :: Boolean -- the script has variables or free points
  , params :: Array { name :: Name, value :: Rational }
  , valuation :: Valuation -- the values this configuration uses (t for free points)
  , outside :: Maybe EvalError -- an assumption that fails at this configuration
  , path :: Array PathChoice -- the branches taken
  }

-- | Values for variables (numbers) and free points (a parameter t, or a
-- | position near which the point is placed).
type Valuation = Map Name SampleValue

-- | A branch taken at the `cases` statement on a line.
type PathChoice = { line :: Int, label :: Label }

objectAt :: Trace -> ObjId -> Maybe ObjInfo
objectAt t i = Map.lookup i t.objects

-- | With two given points, every construction commutes with similarities,
-- | so checking one configuration proves the general statement.
isGeneral :: Trace -> Boolean
isGeneral t = t.givens <= 2

-- | A page of the PDF: the steps `first` … `last` (inclusive).
type Page = { first :: Int, last :: Int }

-- | Pages end at the steps marked by `snapshot`; the steps after the last
-- | snapshot form a final page when there are any.
pages :: Trace -> Array Page
pages t =
  let
    n = Array.length t.steps
    ends = map _.index (Array.filter _.snapshot t.steps)
    allEnds = if n > 0 && Array.last ends /= Just (n - 1) then Array.snoc ends (n - 1) else ends
  in
    Array.mapWithIndex
      (\i e -> { first: if i == 0 then 0 else maybe' (Array.index allEnds (i - 1)) + 1, last: e })
      allEnds
  where
  maybe' = case _ of
    Just x -> x
    Nothing -> -1

-------------------------------------------------------------------------------

type St =
  { objects :: Map ObjId ObjInfo
  , next :: ObjId
  , asserts :: Array AssertInfo
  , step :: Int
  , created :: Array ObjId
  , verdicts :: Array Verdict
  , substeps :: Array SubStep
  , used :: Set Name
  , override :: Valuation -- values imposed from outside (dragging, PDF pages)
  , values :: Map Name Rational -- the variables' values
  , chosen :: Valuation -- the values used
  }

type M = ExceptT EvalError (State St)

type Ctx =
  { macros :: Map Name Macro
  , path :: Array String
  , depth :: Int
  , line :: Int -- the top-level line being evaluated
  , context :: Array String
  }

type Scope = Map Name ObjId

type Bound = { scope :: Scope, bound :: Array (Tuple Name ObjId) }

failWith :: forall a. Ctx -> String -> M a
failWith ctx message = throwError { line: ctx.line, message, context: ctx.context }

create :: Ctx -> Name -> Obj -> Def -> M ObjId
create ctx name obj def = do
  st <- get
  let
    id = st.next
    info = { id, obj, name, path: ctx.path, step: st.step, def, named: false }
  modify_ _ { objects = Map.insert id info st.objects, next = id + 1, created = Array.snoc st.created id }
  pure id

lookupName :: Ctx -> Scope -> Name -> M { id :: ObjId, obj :: Obj }
lookupName ctx scope n = case Map.lookup n scope of
  Nothing -> failWith ctx (n <> " is not defined")
  Just id -> do
    st <- get
    case Map.lookup id st.objects of
      Just info -> pure { id, obj: info.obj }
      Nothing -> failWith ctx ("internal error: unknown object " <> show id)

pointNamed :: Ctx -> Scope -> Name -> M { id :: ObjId, p :: Point }
pointNamed ctx scope n = do
  o <- lookupName ctx scope n
  case o.obj of
    OPoint p -> pure { id: o.id, p }
    other -> failWith ctx (n <> " is " <> kindName (kindOf other) <> ", expected a point")

ratPoint :: RatPoint -> Point
ratPoint r = { x: C.fromRational r.x, y: C.fromRational r.y }

bindNames :: Ctx -> Scope -> Array Name -> Array ObjId -> M Bound
bindNames ctx scope names ids = do
  st <- get
  let
    pairs = Array.zip names ids
  for_ pairs \(Tuple n _) ->
    when (Map.member n scope) (failWith ctx (n <> " is already defined"))
  when (Array.length (Array.nub names) /= Array.length names) (failWith ctx "the same name is bound twice")
  -- Objects created by this statement take the names they are bound to.
  for_ pairs \(Tuple n id) ->
    when (Array.elem id st.created) do
      modify_ \s -> s
        { objects = Map.update (\o -> Just o { name = n, path = ctx.path, named = ctx.depth == 0 }) id s.objects }
  pure { scope: foldl (\sc (Tuple n id) -> Map.insert n id sc) scope pairs, bound: pairs }

single :: Ctx -> String -> Array Name -> M Name
single ctx what names = case names of
  [ n ] -> pure n
  _ -> failWith ctx (what <> " gives one object; bind exactly one name")

-------------------------------------------------------------------------------
-- Statements

evalStmt :: Ctx -> Scope -> Located Stmt -> M Bound
evalStmt ctx scope stmt = case stmt.value of
  Given n p -> do
    id <- create ctx n (OPoint (ratPoint p)) (DGiven p)
    bindNames ctx scope [ n ] [ id ]
  Bind names expr -> do
    ids <- evalExpr ctx scope names expr
    bindNames ctx scope names ids
  Assert pred -> do
    args <- traverse (\n -> (\o -> { name: n, id: o.id, obj: o.obj }) <$> lookupName ctx scope n) pred
    st <- get
    let
      text = showPred identity pred
    case P.check (map (\a -> { name: a.name, obj: a.obj }) args) of
      Left msg -> failWith ctx msg
      Right outcome -> do
        let
          verdict = { text, holds: outcome.holds, detail: outcome.detail, context: ctx.path }
        modify_ _
          { verdicts = Array.snoc st.verdicts verdict
          , asserts = Array.snoc st.asserts
              { pred: map _.id args, text, step: st.step, line: ctx.line, context: ctx.path }
          }
        unless outcome.holds do
          failWith ctx
            ( "assertion failed: " <> text
                <> (if Array.null outcome.detail then "" else " (" <> P.detailText outcome.detail <> ")")
            )
        pure { scope, bound: [] }
  Note _ -> pure { scope, bound: [] }
  Snapshot _ -> pure { scope, bound: [] }
  Sample _ -> pure { scope, bound: [] }
  Param ps -> do
    for_ ps \d -> do
      st <- get
      when (Map.member d.name st.values) (failWith ctx (d.name <> " is already a variable"))
      let
        v = case Map.lookup d.name st.override of
          Just (SVNum q) -> q
          _ -> d.sample
      modify_ _ { values = Map.insert d.name v st.values, chosen = Map.insert d.name (SVNum v) st.chosen }
    pure { scope, bound: [] }
  GivenParam n x y -> do
    st <- get
    let
      value = case _ of
        CRat q -> Right q
        CParam v -> case Map.lookup v st.values of
          Just q -> Right q
          Nothing -> Left (v <> " is not a variable (declare it with param " <> v <> " ~ value)")
    case value x, value y of
      Right a, Right b -> do
        id <- create ctx n (OPoint (ratPoint { x: a, y: b })) (DGiven { x: a, y: b })
        bindNames ctx scope [ n ] [ id ]
      Left e, _ -> failWith ctx e
      _, Left e -> failWith ctx e
  Assume pred -> do
    args <- traverse (\n -> (\o -> { name: n, id: o.id, obj: o.obj }) <$> lookupName ctx scope n) pred
    st <- get
    case P.check (map (\a -> { name: a.name, obj: a.obj }) args) of
      Left msg -> failWith ctx msg
      Right outcome -> do
        modify_ _ { verdicts = Array.snoc st.verdicts { text: showPred identity pred, holds: outcome.holds, detail: outcome.detail, context: [ "assume" ] } }
        pure { scope, bound: [] }
  Cases _ _ -> failWith ctx "cases is evaluated at the top level"
  Show _ -> pure { scope, bound: [] }

evalExpr :: Ctx -> Scope -> Array Name -> Expr -> M (Array ObjId)
evalExpr ctx scope names = case _ of
  ELine a b -> do
    n <- single ctx "line" names
    pa <- pointNamed ctx scope a
    pb <- pointNamed ctx scope b
    case G.lineThrough pa.p pb.p of
      Nothing -> failWith ctx ("line " <> a <> " " <> b <> ": " <> a <> " and " <> b <> " are the same point")
      Just l -> pure <$> create ctx n (OLine l) (DLine pa.id pb.id)
  ECircle o p -> do
    n <- single ctx "circle" names
    po <- pointNamed ctx scope o
    pp <- pointNamed ctx scope p
    case G.circleThrough po.p pp.p of
      Nothing -> failWith ctx ("circle " <> o <> " " <> p <> ": the radius is zero (" <> o <> " = " <> p <> ")")
      Just c -> pure <$> create ctx n (OCircle c) (DCircle po.id pp.id)
  EMeet a b sel -> evalMeet ctx scope names a b sel
  ECall f args -> callMacro ctx scope names f args
  EPointOn o near -> do
    n <- single ctx "point on" names
    oo <- lookupName ctx scope o
    st <- get
    let
      pointOf id = case Map.lookup id st.objects of
        Just { obj: OPoint p } -> Just p
        _ -> Nothing
      def = (_.def) <$> Map.lookup oo.id st.objects
      frame = case oo.obj, def of
        OLine _, Just (DLine a b) -> (\pa pb -> { kind: "line", p: pa, q: pb }) <$> pointOf a <*> pointOf b
        OCircle _, Just (DCircle c q) -> (\pc pq -> { kind: "circle", p: pc, q: pq }) <$> pointOf c <*> pointOf q
        _, _ -> Nothing
    case frame of
      Nothing -> failWith ctx ("point on " <> o <> ": " <> o <> " must be a line or a circle")
      Just f -> do
        let
          t = case Map.lookup n st.override of
            Just (SVNum q) -> q
            Just (SVPoint pos) -> parameterNear f pos
            Nothing -> case near of
              Just pos -> parameterNear f pos
              Nothing -> Q.fromInts 1 2
          p = pointAt f t
        modify_ _ { chosen = Map.insert n (SVNum t) st.chosen }
        pure <$> create ctx n (OPoint p) (DPointOn oo.id t)

evalMeet :: Ctx -> Scope -> Array Name -> Name -> Name -> Maybe Selector -> M (Array ObjId)
evalMeet ctx scope names a b sel = do
  oa <- lookupName ctx scope a
  ob <- lookupName ctx scope b
  meet <- case oa.obj, ob.obj of
    OLine l, OLine m -> pure (G.meetLines l m)
    OLine l, OCircle c -> pure (G.meetLineCircle l c)
    OCircle c, OLine l -> pure (G.meetLineCircle l c)
    OCircle c, OCircle d -> pure (G.meetCircles c d)
    OPoint _, _ -> failWith ctx ("meet: " <> a <> " is a point, expected a line or a circle")
    _, OPoint _ -> failWith ctx ("meet: " <> b <> " is a point, expected a line or a circle")
  let
    mk n p choice other = create ctx n (OPoint p) (DMeet oa.id ob.id choice other)
    what = "meet " <> a <> " " <> b
  case meet, names, sel of
    NoPoint, _, _ -> failWith ctx (what <> ": " <> a <> " and " <> b <> " do not meet")
    Coincide, _, _ -> failWith ctx (what <> ": " <> a <> " and " <> b <> " are the same object")
    OnePoint p, [ n ], Nothing -> pure <$> mk n p CUnique Nothing
    OnePoint _, [ _ ], Just _ -> failWith ctx (what <> ": there is only one intersection point; drop the selector")
    OnePoint _, _, _ -> failWith ctx (what <> ": there is only one intersection point; bind one name")
    TwoPoints p q, [ n1, n2 ], Nothing -> do
      i <- mk n1 p CLexFirst (Just q)
      j <- mk n2 q CLexSecond (Just p)
      pure [ i, j ]
    TwoPoints _ _, [ _, _ ], Just _ -> failWith ctx (what <> ": a selector picks one point; bind one name")
    TwoPoints _ _, [ _ ], Nothing ->
      failWith ctx (what <> ": there are two intersection points; bind two names or add a selector (except, nearest, farthest, left, right)")
    TwoPoints p q, [ n ], Just s -> do
      chosen <- choose ctx scope what s p q
      pure <$> mk n chosen.point chosen.choice (Just chosen.other)
    TwoPoints _ _, _, _ -> failWith ctx (what <> ": bind one or two names")

choose
  :: Ctx
  -> Scope
  -> String
  -> Selector
  -> Point
  -> Point
  -> M { point :: Point, other :: Point, choice :: Choice }
choose ctx scope what sel p q = case sel of
  Except r -> do
    rp <- refPoint r
    if G.samePoint p rp.p then pure { point: q, other: p, choice: CExcept rp.ref }
    else if G.samePoint q rp.p then pure { point: p, other: q, choice: CExcept rp.ref }
    else failWith ctx (what <> ": " <> refText r <> " is not one of the intersection points")
  Nearest r -> byDistance r CNearest LT
  Farthest r -> byDistance r CFarthest GT
  LeftOf a b -> bySide a b CLeft GT "left of"
  RightOf a b -> bySide a b CRight LT "right of"
  where
  refText (RefName n) = n
  refText (RefLit lit) = "(" <> show lit.x <> ", " <> show lit.y <> ")"
  refPoint r = case r of
    RefName n -> (\o -> { p: o.p, ref: RefObj o.id }) <$> pointNamed ctx scope n
    RefLit lit -> pure { p: ratPoint lit, ref: RefRat lit }
  byDistance r mkChoice want = do
    rp <- refPoint r
    case compare (G.dist2 p rp.p) (G.dist2 q rp.p) of
      EQ -> failWith ctx (what <> ": both intersection points are equally far from " <> refText r)
      o
        | o == want -> pure { point: p, other: q, choice: mkChoice rp.ref }
        | otherwise -> pure { point: q, other: p, choice: mkChoice rp.ref }
  bySide a b mkChoice want label = do
    pa <- pointNamed ctx scope a
    pb <- pointNamed ctx scope b
    when (G.samePoint pa.p pb.p) (failWith ctx (what <> ": " <> a <> " and " <> b <> " are the same point"))
    let
      sp = G.orientation pa.p pb.p p == want
      sq = G.orientation pa.p pb.p q == want
    case sp, sq of
      true, false -> pure { point: p, other: q, choice: mkChoice pa.id pb.id }
      false, true -> pure { point: q, other: p, choice: mkChoice pa.id pb.id }
      _, _ -> failWith ctx (what <> ": " <> (if sp then "both" else "neither") <> " of the intersection points lie " <> label <> " " <> a <> " → " <> b)

-------------------------------------------------------------------------------
-- Free points on objects

-- | A line through p and q, or a circle with centre p through q.
type Frame = { kind :: String, p :: Point, q :: Point }

-- | The point with parameter t: p + t(q − p) on a line; on a circle, q
-- | rotated about p by the angle with cos = (1 − t²)/(1 + t²) and
-- | sin = 2t/(1 + t²) (so t = 0 is q itself).
pointAt :: Frame -> Rational -> Point
pointAt f t
  | f.kind == "line" =
      let
        tc = C.fromRational t
      in
        { x: f.p.x + tc * (f.q.x - f.p.x), y: f.p.y + tc * (f.q.y - f.p.y) }
  | otherwise =
      let
        tc = C.fromRational t
        d = one + tc * tc
        cs = (one - tc * tc) / d
        sn = (tc + tc) / d
        ux = f.q.x - f.p.x
        uy = f.q.y - f.p.y
      in
        { x: f.p.x + cs * ux - sn * uy, y: f.p.y + sn * ux + cs * uy }

-- | The parameter of the point nearest to a position, rounded to a simple
-- | rational.
parameterNear :: Frame -> RatPoint -> Rational
parameterNear f pos =
  let
    n = C.toNumber
    px = n f.p.x
    py = n f.p.y
    ux = n f.q.x - px
    uy = n f.q.y - py
    wx = Q.toNumber pos.x - px
    wy = Q.toNumber pos.y - py
    t
      | f.kind == "line" = (wx * ux + wy * uy) / (ux * ux + uy * uy)
      | otherwise =
          let
            cr = ux * wy - uy * wx
            dt = ux * wx + uy * wy
            nn = Number.sqrt ((ux * ux + uy * uy) * (wx * wx + wy * wy))
          in
            if nn + dt < 1.0e-9 then 1000.0 else cr / (nn + dt)
  in
    approxRational (max (-1000.0) (min 1000.0 t))

-- | A rational close to x with a denominator of at most 64.
approxRational :: Number -> Rational
approxRational x = go 0 x { h0: 0, h1: 1, k0: 1, k1: 0 }
  where
  go :: Int -> Number -> { h0 :: Int, h1 :: Int, k0 :: Int, k1 :: Int } -> Rational
  go i y c =
    let
      a = Number.floor y
      ai = Int.round a
      h = ai * c.h1 + c.h0
      k = ai * c.k1 + c.k0
      frac = y - a
    in
      if k > 64 || i > 12 then Q.fromInts c.h1 (if c.k1 == 0 then 1 else c.k1)
      else if frac < 1.0e-9 then Q.fromInts h k
      else go (i + 1) (1.0 / frac) { h0: c.h1, h1: h, k0: c.k1, k1: k }

-------------------------------------------------------------------------------
-- Cases at a configuration

-- | The branch this configuration lies in, with the points it binds.
evalCase :: Ctx -> Scope -> CaseForm -> Array Branch -> M { branch :: Branch, bound :: Bound }
evalCase ctx scope form branches = do
  outcome <- case form of
    CaseMeet a b -> do
      oa <- lookupName ctx scope a
      ob <- lookupName ctx scope b
      meet <- case oa.obj, ob.obj of
        OLine l, OLine m -> pure (G.meetLines l m)
        OLine l, OCircle c -> pure (G.meetLineCircle l c)
        OCircle c, OLine l -> pure (G.meetLineCircle l c)
        OCircle c, OCircle d -> pure (G.meetCircles c d)
        _, _ -> failWith ctx "cases meet: expected two lines or circles"
      pure case meet of
        NoPoint -> { label: LNone, points: [], ids: [ oa.id, ob.id ] }
        Coincide -> { label: LSame, points: [], ids: [ oa.id, ob.id ] }
        OnePoint p -> { label: LOne, points: [ p ], ids: [ oa.id, ob.id ] }
        TwoPoints p q -> { label: LTwo, points: [ p, q ], ids: [ oa.id, ob.id ] }
    CaseSide p a b -> do
      pp <- pointNamed ctx scope p
      pa <- pointNamed ctx scope a
      pb <- pointNamed ctx scope b
      pure
        { label: case G.orientation pa.p pb.p pp.p of
            GT -> LLeft
            EQ -> LOn
            LT -> LRight
        , points: []
        , ids: []
        }
    CaseCompare a b c d -> do
      pa <- pointNamed ctx scope a
      pb <- pointNamed ctx scope b
      pc <- pointNamed ctx scope c
      pd <- pointNamed ctx scope d
      pure
        { label: case compare (G.dist2 pa.p pb.p) (G.dist2 pc.p pd.p) of
            LT -> LLess
            EQ -> LEqual
            GT -> LGreater
        , points: []
        , ids: []
        }
    CasePred (PointEq p q) -> do
      pp <- pointNamed ctx scope p
      pq <- pointNamed ctx scope q
      pure { label: if G.samePoint pp.p pq.p then LEqual else LDifferent, points: [], ids: [] }
    CasePred pred -> do
      args <- traverse (\n -> (\o -> { name: n, obj: o.obj }) <$> lookupName ctx scope n) pred
      case P.check args of
        Left msg -> failWith ctx msg
        Right o -> pure { label: if o.holds then LYes else LNo, points: [], ids: [] }
  case Array.find (\br -> br.label == outcome.label) branches of
    Nothing ->
      failWith ctx
        ( "this configuration is in the case " <> labelText outcome.label
            <> ", which the script leaves out (so it is outside the theorem's hypotheses)"
        )
    Just br -> case outcome.points, br.names, outcome.ids of
      [ p ], [ n ], [ a, b ] -> do
        id <- create ctx n (OPoint p) (DMeet a b CUnique Nothing)
        bound <- bindNames ctx scope [ n ] [ id ]
        pure { branch: br, bound }
      [ p, q ], [ n1, n2 ], [ a, b ] -> do
        i <- create ctx n1 (OPoint p) (DMeet a b CLexFirst (Just q))
        j <- create ctx n2 (OPoint q) (DMeet a b CLexSecond (Just p))
        bound <- bindNames ctx scope [ n1, n2 ] [ i, j ]
        pure { branch: br, bound }
      _, _, _ -> pure { branch: br, bound: { scope, bound: [] } }

-------------------------------------------------------------------------------
-- Macros

callMacro :: Ctx -> Scope -> Array Name -> Name -> Array Name -> M (Array ObjId)
callMacro ctx scope names f args = do
  m <- case Map.lookup f ctx.macros of
    Just m -> pure m
    Nothing -> failWith ctx ("unknown macro " <> f)
  let
    label = f <> "(" <> joinWith ", " args <> ")"
  when (isJust (Array.find (\p -> startsWithName p) ctx.path)) do
    failWith ctx ("recursive call of macro " <> f)
  when (Array.length args /= Array.length m.params) do
    failWith ctx (f <> " takes " <> show (Array.length m.params) <> " arguments, got " <> show (Array.length args))
  when (Array.length names /= Array.length m.results) do
    failWith ctx (f <> " gives " <> show (Array.length m.results) <> " objects, but " <> show (Array.length names) <> " names are bound")
  argIds <- traverse (\n -> _.id <$> lookupName ctx scope n) args
  modify_ \s -> s { used = Set.insert f s.used }
  let
    inner = ctx
      { path = Array.snoc ctx.path label
      , depth = ctx.depth + 1
      , context = Array.snoc ctx.context ("in " <> label)
      }
    local = Map.fromFoldable (Array.zip m.params argIds)
  final <- foldM (subStatement inner) local m.body
  traverse
    ( \r -> case Map.lookup r final of
        Just id -> pure id
        Nothing -> failWith inner ("macro " <> f <> " does not define its result " <> r)
    )
    m.results
  where
  startsWithName p = p == f || startsWith (f <> "(") p

-- | Evaluates a statement inside a macro body and records it as a sub-step.
subStatement :: Ctx -> Scope -> Located Stmt -> M Scope
subStatement ctx scope stmt = do
  let
    here = ctx { context = Array.snoc ctx.context ("at: " <> stmt.text) }
  before <- get
  -- A call is listed before the statements of its expansion.
  case stmt.value of
    Bind _ (ECall _ _) -> pushSub { depth: ctx.depth, text: stmt.text, objects: [], verdicts: [] }
    Note text -> pushSub { depth: ctx.depth, text: "note: " <> text, objects: [], verdicts: [] }
    _ -> pure unit
  res <- evalStmt here scope stmt
  after <- get
  case stmt.value of
    Bind _ (ECall _ _) -> pure unit
    Note _ -> pure unit
    _ ->
      pushSub
        { depth: ctx.depth
        , text: stmt.text
        , objects: Array.drop (Array.length before.created) after.created
        , verdicts: Array.drop (Array.length before.verdicts) after.verdicts
        }
  pure res.scope
  where
  pushSub s = modify_ \st -> st { substeps = Array.snoc st.substeps s }

foreign import startsWith :: String -> String -> Boolean

-------------------------------------------------------------------------------
-- Scripts

type Acc =
  { st :: St
  , scope :: Scope
  , steps :: Array Step
  , intro :: Array String
  , error :: Maybe EvalError
  , givens :: Int
  , samples :: Map Name SampleValue
  , params :: Array { name :: Name, value :: Rational }
  , outside :: Maybe EvalError
  , path :: Array PathChoice
  , parametric :: Boolean
  }

initialState :: Valuation -> St
initialState override =
  { objects: Map.empty
  , next: 0
  , asserts: []
  , step: 0
  , created: []
  , verdicts: []
  , substeps: []
  , used: Set.empty
  , override
  , values: Map.empty
  , chosen: Map.empty
  }

evaluate :: Script -> Trace
evaluate = evaluateAt Map.empty

-- | Evaluates the script at a configuration: the given values replace the
-- | script's samples for the variables and free points they name.
evaluateAt :: Valuation -> Script -> Trace
evaluateAt override script =
  let
    preludeMap = Map.fromFoldable (map (\m -> Tuple m.name m) Prelude.macros)
    clash = Array.find (\m -> Map.member m.name preludeMap) script.macros
    macros = Map.union (Map.fromFoldable (map (\m -> Tuple m.name m) script.macros)) preludeMap
    start =
      { st: initialState override
      , scope: Map.empty
      , steps: []
      , intro: []
      , error: Nothing
      , givens: 0
      , samples: Map.empty
      , params: []
      , outside: Nothing
      , path: []
      , parametric: false
      }
    final = case clash of
      Just m -> start { error = Just { line: m.line, message: m.name <> " is a built-in construction; choose another name", context: [] } }
      Nothing -> topStatements macros start script.body
  in
    { title: script.title
    , intro: final.intro
    , steps: final.steps
    , objects: final.st.objects
    , asserts: final.st.asserts
    , scope: final.scope
    , givens: final.givens
    , macros: script.macros
    , used: Array.fromFoldable final.st.used
    , error: final.error
    , parametric: final.parametric
    , params: final.params
    , valuation: final.st.chosen
    , outside: final.outside
    , path: final.path
    }

topStatements :: Map Name Macro -> Acc -> Array (Located Stmt) -> Acc
topStatements macros = foldl (topStatement macros)

withSamples :: Array SampleItem -> Map Name SampleValue -> Map Name SampleValue
withSamples items m = foldl (\acc i -> Map.insert i.name i.value acc) m items

topStatement :: Map Name Macro -> Acc -> Located Stmt -> Acc
topStatement macros acc stmt
  | isJust acc.error = acc
  | otherwise = case stmt.value of
      Note text -> case Array.unsnoc acc.steps of
        Nothing -> acc { intro = Array.snoc acc.intro text }
        Just { init, last } -> acc { steps = Array.snoc init last { notes = Array.snoc last.notes text } }
      -- Marks the step before it, so consecutive snapshots count once.
      Snapshot items ->
        let
          acc' = acc { samples = withSamples items acc.samples }
        in
          case Array.unsnoc acc.steps of
            Nothing -> acc'
            Just { init, last } -> acc' { steps = Array.snoc init last { snapshot = true } }
      Sample items -> acc { samples = withSamples items acc.samples }
      -- drawing only: attached to the step above
      Show item ->
        let
          point n = case Map.lookup n acc.scope >>= \i -> Map.lookup i acc.st.objects of
            Just { id, obj: OPoint _ } -> Right id
            Just _ -> Left (n <> " is not a point")
            Nothing -> Left (n <> " is not defined")
          resolved = case item of
            ShowSegment a b -> ShownSegment <$> point a <*> point b
            ShowPolygon ps -> ShownPolygon <$> traverse point ps
            ShowCircle o a -> ShownCircle <$> point o <*> point a
            ShowThrough a b c -> ShownThrough <$> point a <*> point b <*> point c
          fail msg = acc { error = Just { line: stmt.line, message: "show " <> showItemText item <> ": " <> msg, context: [] } }
        in
          case resolved, Array.unsnoc acc.steps of
            Left msg, _ -> fail msg
            Right _, Nothing -> fail "nothing to show yet"
            Right sh, Just { init, last } -> acc { steps = Array.snoc init last { shows = Array.snoc last.shows sh } }
      Param ps ->
        let
          ctx = { macros, path: [], depth: 0, line: stmt.line, context: [] }
          Tuple result st1 = runState (runExceptT (evalStmt ctx acc.scope stmt)) acc.st
        in
          case result of
            Left err -> acc { error = Just err }
            Right _ -> acc
              { st = st1
              , parametric = true
              , samples = foldl (\m d -> Map.insert d.name (SVNum d.sample) m) acc.samples ps
              , params = acc.params <> Array.mapMaybe (\d -> (\v -> { name: d.name, value: v }) <$> Map.lookup d.name st1.values) ps
              }
      Cases form branches ->
        let
          index = Array.length acc.steps
          ctx = { macros, path: [], depth: 0, line: stmt.line, context: [] }
          st0 = acc.st { step = index, created = [], verdicts = [], substeps = [] }
          Tuple result st1 = runState (runExceptT (evalCase ctx acc.scope form branches)) st0
        in
          case result of
            Left err -> acc { error = Just err }
            Right r ->
              let
                boundIds = map (\(Tuple _ id) -> id) r.bound.bound
                step = (newStep acc index stmt)
                  { text = stmt.text <> " → " <> r.branch.text
                  , kind = SCase
                  , objects = boundIds
                  }
                leading = Array.concatMap
                  ( \s -> case s.value of
                      Sample is -> is
                      _ -> []
                  )
                  ( Array.takeWhile
                      ( \s -> case s.value of
                          Sample _ -> true
                          Note _ -> true
                          _ -> false
                      )
                      r.branch.body
                  )
                samples = withSamples leading acc.samples
                acc1 = acc
                  { st = st1
                  , scope = r.bound.scope
                  , steps = Array.snoc acc.steps step { samples = samples }
                  , path = Array.snoc acc.path { line: stmt.line, label: r.branch.label }
                  , samples = samples
                  }
              in
                topStatements macros acc1 r.branch.body
      _ ->
        let
          index = Array.length acc.steps
          ctx = { macros, path: [], depth: 0, line: stmt.line, context: [] }
          st0 = acc.st { step = index, created = [], verdicts = [], substeps = [] }
          Tuple result st1 = runState (runExceptT (evalStmt ctx acc.scope stmt)) st0
        in
          case result of
            Left err -> acc { error = Just err }
            Right res ->
              let
                boundIds = map (\(Tuple _ id) -> id) res.bound
                samples = case stmt.value of
                  Bind [ n ] (EPointOn _ near) -> case Map.lookup n st1.chosen, near of
                    _, Just pos -> Map.insert n (SVPoint pos) acc.samples
                    Just v, Nothing -> Map.insert n v acc.samples
                    Nothing, Nothing -> acc.samples
                  _ -> acc.samples
                step = (newStep acc index stmt)
                  { kind = case stmt.value of
                      Given _ _ -> SGiven
                      GivenParam _ _ _ -> SGiven
                      Assert _ -> SAssert
                      Assume _ -> SAssume
                      _ -> SConstruct
                  , objects = boundIds
                  , aux = Array.filter (\i -> not (Array.elem i boundIds)) st1.created
                  , verdicts = st1.verdicts
                  , substeps = st1.substeps
                  , macro = case stmt.value of
                      Bind _ (ECall f _) -> Just f
                      _ -> Nothing
                  , samples = samples
                  }
                failed = case stmt.value of
                  Assume _ -> Array.find (not <<< _.holds) st1.verdicts
                  _ -> Nothing
              in
                acc
                  { st = st1
                  , scope = res.scope
                  , steps = Array.snoc acc.steps step
                  , samples = samples
                  , givens = acc.givens + case stmt.value of
                      Given _ _ -> 1
                      _ -> 0
                  , parametric = acc.parametric || case stmt.value of
                      GivenParam _ _ _ -> true
                      Bind _ (EPointOn _ _) -> true
                      _ -> false
                  , outside = case acc.outside, failed of
                      Nothing, Just v -> Just { line: stmt.line, message: "the assumption " <> v.text <> " fails at this configuration", context: [] }
                      o, _ -> o
                  }

newStep :: Acc -> Int -> Located Stmt -> Step
newStep acc index stmt =
  { index
  , line: stmt.line
  , text: stmt.text
  , kind: SConstruct
  , objects: []
  , aux: []
  , verdicts: []
  , substeps: []
  , notes: []
  , macro: Nothing
  , snapshot: false
  , samples: acc.samples
  , shows: []
  }

-- | Parses and evaluates a script.
evaluateSource :: String -> Either ParseError Trace
evaluateSource src = evaluate <$> parseScript src
