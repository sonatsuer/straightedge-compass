-- | Proofs of universal statements: the script evaluated symbolically, over
-- | towers of square roots on ℚ(variables).
-- |
-- | Every decision the construction makes is either settled on the whole
-- | domain or turned into a hypothesis:
-- |
-- | - degeneracies (a line through distinct points, lines that are not
-- |   parallel) and existence (two intersection points) become *computed*
-- |   hypotheses, stated with the step they come from;
-- | - `assume` adds *assumed* hypotheses, `cases` adds *branch* conditions;
-- | - a selector, a lexicographic order or a claim whose truth depends on the
-- |   configuration is an error, with two witness configurations.
-- |
-- | Equalities must be identities. A branch whose condition is an equation
-- | (a tangency, `equal`, `on`) is handled by solving the equation for a
-- | variable and evaluating the script again with the solution substituted.
-- |
-- | The theorem of a branch: for all values of the variables satisfying its
-- | hypotheses, its claims hold. Every branch has a sample configuration that
-- | is checked exactly against its hypotheses, so no branch is vacuous.
module Proof.Prove
  ( Proof
  , PNode(..)
  , PStep
  , PSplit
  , PBranch
  , PCase
  , PVerdict
  , PError
  , Hyp
  , HypOrigin(..)
  , VarInfo
  , SObj(..)
  , SPoint
  , SDef(..)
  , SInfo
  , SChoice(..)
  , SRef(..)
  , Claim
  , Cover
  , substPoly
  , prove
  , proveSource
  , namesOf
  , leaves
  , hypDisplay
  , condDisplay
  , stepsAlong
  ) where

import Prelude

import Constructible.Field as C
import Constructible.ParamField (PC, PM, Sgn(..))
import Constructible.ParamField as PF
import Constructible.Poly (Poly)
import Constructible.Poly as P
import Constructible.RatFun (RatFun)
import Constructible.RatFun as RF
import Constructible.Rational as Q
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, get, modify_, put, runState)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (all, foldM, foldl, for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.String (joinWith)
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Unsafe.Reference (unsafeRefEq)
import Proof.Eval (Frame, StepKind(..), parameterNear)
import Proof.Parser (ParseError, parseScript)
import Proof.Prelude as Prelude
import Proof.Signs (Answer(..), Context, Rel(..), Valuation)
import Proof.Signs as S
import Proof.Syntax (Branch, CaseForm(..), Cmp(..), Coord(..), Expr(..), Label(..), Located, Macro, Name, PredF(..), RatPoint, Ref(..), SampleItem, SampleValue(..), Script, Selector(..), ShowItem(..), Stmt(..), labelText, labelsOf, showPred)
import Render.Sym (Names)
import Render.Sym as Sym

-------------------------------------------------------------------------------
-- Results

type SPoint = { x :: PC, y :: PC }

data SObj
  = SPt SPoint
  | SLn { a :: PC, b :: PC, c :: PC }
  | SCi { cx :: PC, cy :: PC, r2 :: PC }

data HypOrigin = Assumed | BranchCond | Computed

derive instance Eq HypOrigin

-- | A hypothesis: a polynomial in the variables compared with zero.
type Hyp = { poly :: Poly, rel :: Rel, origin :: HypOrigin, line :: Int, step :: Int, text :: String }

type VarInfo = { name :: Name, text :: String, typst :: String, free :: Maybe { object :: Name, frame :: { kind :: String, p :: SPoint, q :: SPoint } } }

type PVerdict = { text :: String, holds :: Boolean, why :: String, detail :: Array { label :: String, value :: PC } }

type PStep =
  { index :: Int
  , line :: Int
  , text :: String
  , kind :: StepKind
  , objects :: Array { name :: Name, obj :: SObj }
  , hyps :: Array Hyp -- introduced at this step
  , verdicts :: Array PVerdict
  , notes :: Array String
  , snapshot :: Boolean
  , figure :: Map Name SampleValue -- the configuration drawn for this step
  }

type PError = { line :: Int, message :: String }

newtype PNode = PNode
  { steps :: Array PStep
  , split :: Maybe PSplit
  , error :: Maybe PError
  , hyps :: Array Hyp -- in force at the end
  , theorem :: Array Hyp -- the hypotheses of the theorem: implied computed ones dropped
  , vars :: Array VarInfo
  , subst :: Array { var :: String, value :: String } -- substituted variables
  , substMap :: Map Int RatFun
  , objects :: Array SInfo -- every object, in creation order
  , named :: Array Int -- the objects bound to a top-level name
  , claims :: Array Claim -- the assertions, on object ids
  }

-- | An assertion that was proved, on object ids.
type Claim = { pred :: PredF Int, text :: String }

type PSplit = { line :: Int, text :: String, branches :: Array PBranch }

type PBranch =
  { label :: Label
  , text :: String -- the branch's line, or the label
  , condition :: String -- the condition in words and symbols
  , cases :: Array PCase -- one, or one per solution of an equation
  , impossible :: Maybe String
  , missing :: Maybe String -- an error: possible but not written
  , cover :: Maybe Cover -- the condition, for the Lean coverage theorem
  }

-- | A branch's condition: inequalities, or equations with their solutions
-- | (the full substitution, the conditions for solving, and whether the
-- | case is possible).
type Cover =
  { ineq :: Array { poly :: Poly, rel :: Rel, text :: String }
  , refuted :: Boolean
  , eqs :: Array Poly
  , options :: Array { subst :: Map Int RatFun, hyps :: Array { poly :: Poly, rel :: Rel, text :: String }, possible :: Boolean, text :: String }
  }

type PCase = { text :: String, node :: Maybe PNode, impossible :: Maybe String }

type Proof =
  { title :: Maybe String
  , intro :: Array String
  , root :: PNode
  , errors :: Array PError
  , parametric :: Boolean
  }

-------------------------------------------------------------------------------
-- State

-- | How an object was made, for the Lean export.
data SDef
  = DefGiven
  | DefLine Int Int
  | DefCircle Int Int
  | DefMeet { a :: Int, b :: Int, choice :: SChoice, other :: Maybe SPoint }
  | DefPointOn { obj :: Int, var :: Int }

-- | Which intersection point: the only one, the first or second (by x,
-- | then y), one of an unordered pair (the other is `other`), or by a
-- | selector.
data SChoice
  = SCUnique
  | SCLexFirst
  | SCLexSecond
  | SCPair
  | SCExcept SRef
  | SCNearest SRef
  | SCFarthest SRef
  | SCLeft Int Int
  | SCRight Int Int

data SRef = SRefObj Int | SRefLit RatPoint

type SInfo = { id :: Int, name :: Name, obj :: SObj, def :: SDef }

type St =
  { objs :: Map Int SInfo
  , next :: Int
  , scope :: Map Name Int
  , hyps :: Array Hyp
  , vars :: Array VarInfo
  , varIdx :: Map Name Int
  , subst :: Map Int RatFun
  , samples :: Map Name SampleValue
  , sampled :: Boolean -- a sample was set since the segment started
  , step :: Int
  , line :: Int
  , curHyps :: Array Hyp
  , curVerdicts :: Array PVerdict
  , created :: Array Int
  , macros :: Map Name Macro
  , rename :: Map Name String -- display names inside a macro call
  , unordered :: Array (Tuple Name Name) -- pairs used only symmetrically here
  , claims :: Array Claim
  }

data Signal
  = Fail PError
  | Restart (Array Option) -- an equation to impose: one option per solution

-- | A solution of equations: variables to substitute, and the conditions
-- | under which the solution is valid.
type Option = { subst :: Map Int RatFun, text :: String, hyps :: Array { poly :: Poly, rel :: Rel, text :: String } }

type M = ExceptT Signal (State St)

failAt :: forall a. String -> M a
failAt message = do
  st <- get
  throwError (Fail { line: st.line, message })

namesOf :: Array VarInfo -> Names
namesOf vars =
  { text: \i -> maybe' (Array.index vars i) _.text ("x" <> show i)
  , typst: \i -> maybe' (Array.index vars i) _.typst ("x_" <> show i)
  }
  where
  maybe' m f d = case m of
    Just v -> f v
    Nothing -> d

names :: St -> Names
names st = namesOf st.vars

-- | The variables' values at the samples in force.
valuationOf :: St -> Map Name SampleValue -> Valuation
valuationOf st samples = foldl add [] (Array.mapWithIndex Tuple st.vars)
  where
  add acc (Tuple i v) = Array.snoc acc case Map.lookup i st.subst of
    Just f -> RF.evalAt f acc
    Nothing -> case v.free, Map.lookup v.name samples of
      Nothing, Just (SVNum q) -> Just q
      Nothing, _ -> Nothing
      Just _, Just (SVNum t) -> Just t
      Just f, Just (SVPoint pos) -> parameterAt acc f.frame pos
      Just f, Nothing -> parameterAt acc f.frame { x: zero, y: zero }
  parameterAt acc frame pos = do
    p <- point acc frame.p
    q <- point acc frame.q
    pure (parameterNear ({ kind: frame.kind, p, q } :: Frame) pos)
  point acc p = (\x y -> { x, y }) <$> PF.evalAt acc p.x <*> PF.evalAt acc p.y

context :: St -> Context
context st = { conds: map (\h -> { poly: h.poly, rel: h.rel }) st.hyps, samples: [ valuationOf st st.samples ] }

oracle :: St -> PF.Oracle
oracle st = S.oracleWith (Sym.valuationText n) (Sym.ratText n) (context st)
  where
  n = names st

pm :: forall a. PM a -> M a
pm m = do
  st <- get
  case PF.runPM (oracle st) m of
    Left msg -> failAt msg
    Right a -> pure a

e :: PC -> PF.Ex
e = PF.ex

-------------------------------------------------------------------------------
-- Hypotheses

addHyp :: HypOrigin -> String -> Poly -> Rel -> M Unit
addHyp origin text poly0 rel = do
  st <- get
  let
    poly = if rel == RNonZero then simplifyNonZero (context st) poly0 else poly0
    h = { poly, rel, origin, line: st.line, step: st.step, text }
    -- a constant that satisfies the relation says nothing
    trivial = case P.constValue poly of
      Just c -> S.relHolds rel (Q.signum c)
      Nothing -> false
  unless (trivial || Array.any (\x -> x.poly == poly && x.rel == rel) st.hyps) do
    put st { hyps = Array.snoc st.hyps h, curHyps = Array.snoc st.curHyps h }

-- | A simpler polynomial that is nonzero exactly where p is (on the domain):
-- | powers of a variable count once, factors known to be nonzero are
-- | dropped, and a constant times a square is replaced by its root.
simplifyNonZero :: Context -> Poly -> Poly
simplifyNonZero ctx p0 = foldl (*) one (pieces ((P.normalise p0).p))
  where
  -- square-free pieces whose non-vanishing is not already known
  pieces f
    | P.constValue f /= Nothing = []
    | S.isNonZero ctx f = []
    | otherwise = case Array.findMap (repeated f) (P.vars f) of
        Just g -> pieces g <> pieces (divideAll f g)
        Nothing -> [ (P.normalise f).p ]
  -- a factor of f that occurs squared: gcd(f, ∂f/∂v)
  repeated f v =
    let
      g = P.gcd f (P.deriv f v)
    in
      if P.constValue g == Nothing && P.degIn g v > 0 then Just g
      else if P.divExact f (P.var v) /= Nothing && P.vars f /= [ v ] then Just (P.var v)
      else Nothing
  divideAll f g = case P.divExact f g of
    Just h | P.constValue g == Nothing -> divideAll h g
    _ -> f

-- | A polynomial whose non-vanishing implies x ≠ 0: the numerator for a
-- | rational function, the norm (down the tower) otherwise.
nonZeroPoly :: PC -> PM Poly
nonZeroPoly x = case PF.deconstruct x of
  PF.PBase f -> pure (RF.num f)
  PF.PQuad a b r -> do
    a2 <- PF.mul a a
    b2 <- PF.mul b b
    rb2 <- PF.mul r b2
    n <- PF.sub a2 rb2
    nonZeroPoly n

-- | The construction needs x ≠ 0 here.
requireNonZero :: String -> PC -> M Unit
requireNonZero text x = do
  when (PF.isZero x) (failAt (text <> " fails for every configuration"))
  s <- pm (PF.sign x)
  case s of
    Pos -> pure unit
    Neg -> pure unit
    _ -> do
      p <- pm (nonZeroPoly x)
      st <- get
      unless (S.isNonZero (context st) p) (addHyp Computed text p RNonZero)

-- | The construction needs x > 0 here.
requirePositive :: String -> PC -> M Unit
requirePositive text x = do
  s <- pm (PF.sign x)
  case s of
    Pos -> pure unit
    NonNeg -> requireNonZero text x
    Unknown -> case PF.toRatFun x of
      Just f -> addHyp Computed text (RF.num f * RF.den f) RPos
      Nothing -> failAt (text <> ": the condition involves square roots; split the cases with cases")
    _ -> failAt ("the construction is impossible: " <> text <> " never holds")

-- | The sign of x must be the same everywhere on the domain.
decideStrict :: String -> String -> PC -> M Ordering
decideStrict what hint x = do
  s <- pm (PF.sign x)
  case s of
    Pos -> pure GT
    Neg -> pure LT
    Zero -> pure EQ
    NonNeg -> requireNonZero what x $> GT
    NonPos -> requireNonZero what x $> LT
    Unknown -> do
      st <- get
      let
        w = witnesses st x
      failAt (what <> " depends on the configuration" <> w <> hint)

-- | Two configurations where x has opposite signs, as text.
witnesses :: St -> PC -> String
witnesses st x =
  let
    pts = S.findPoints (context st) 24
    valued = Array.mapMaybe (\p -> (\v -> { p, s: C.sign v }) <$> PF.evalAt p x) pts
    n = names st
  in
    case Array.find (\v -> v.s == GT) valued, Array.find (\v -> v.s == LT) valued of
      Just a, Just b -> " (compare " <> Sym.valuationText n a.p <> " with " <> Sym.valuationText n b.p <> ")"
      _, _ -> ""

-- | The name to show for an object (inside a macro: the caller's name).
shown :: Name -> M String
shown n = do
  st <- get
  pure (fromMaybe n (Map.lookup n st.rename))

-- | Pairs `P, Q = meet …` whose points are used only as `line P Q` (and are
-- | not results): their order does not matter, so it need not be decided.
unorderedPairs :: Array Name -> Array (Located Stmt) -> Array (Tuple Name Name)
unorderedPairs results body = Array.mapMaybe pair body
  where
  pair s = case s.value of
    Bind [ p, q ] (EMeet _ _ _) | onlyInLine p q -> Just (Tuple p q)
    _ -> Nothing
  onlyInLine p q =
    not (Array.elem p results) && not (Array.elem q results)
      && all (symmetric p q) body
  symmetric p q s = case s.value of
    Bind [ a, b ] (EMeet _ _ _) | a == p && b == q -> true
    Bind _ (ELine a b) | (a == p && b == q) || (a == q && b == p) -> true
    other -> not (mentions p other) && not (mentions q other)
  mentions n = case _ of
    Bind ns ex -> Array.elem n ns || exprMentions n ex
    Assert pr -> Array.elem n (Array.fromFoldable pr)
    Assume pr -> Array.elem n (Array.fromFoldable pr)
    _ -> false
  exprMentions n = case _ of
    ELine a b -> a == n || b == n
    ECircle a b -> a == n || b == n
    EMeet a b sel -> a == n || b == n || case sel of
      Just (Except (RefName r)) -> r == n
      Just (Nearest (RefName r)) -> r == n
      Just (Farthest (RefName r)) -> r == n
      Just (LeftOf x y) -> x == n || y == n
      Just (RightOf x y) -> x == n || y == n
      _ -> false
    ECall _ args -> Array.elem n args
    EPointOn o _ -> o == n

-- | A name in Typst math: one letter as itself, longer names quoted.
typstName :: String -> String
typstName n = if String.length n == 1 then n else "italic(\"" <> n <> "\")"

-------------------------------------------------------------------------------
-- Objects

create :: Name -> SObj -> SDef -> M Int
create name obj def = do
  st <- get
  let
    id = st.next
  put st { objs = Map.insert id { id, name, obj, def } st.objs, next = id + 1, created = Array.snoc st.created id }
  pure id

lookupObj :: Map Name Int -> Name -> M SInfo
lookupObj scope n = do
  st <- get
  case Map.lookup n scope >>= \i -> Map.lookup i st.objs of
    Just o -> pure o
    Nothing -> failAt (n <> " is not defined")

pointOf :: Map Name Int -> Name -> M SPoint
pointOf scope n = do
  o <- lookupObj scope n
  case o.obj of
    SPt p -> pure p
    _ -> failAt (n <> " is not a point")

lineOf :: Map Name Int -> Name -> M { a :: PC, b :: PC, c :: PC }
lineOf scope n = do
  o <- lookupObj scope n
  case o.obj of
    SLn l -> pure l
    _ -> failAt (n <> " is not a line")

constPoint :: RatPoint -> SPoint
constPoint p = { x: PF.fromRational p.x, y: PF.fromRational p.y }

-- | The value of a variable: its substitution, or the variable itself.
varValue :: Int -> M PC
varValue i = do
  st <- get
  pure case Map.lookup i st.subst of
    Just f -> PF.fromRatFun f
    Nothing -> PF.var i

newVar :: VarInfo -> M Int
newVar v = do
  st <- get
  when (Map.member v.name st.varIdx) (failAt (v.name <> " is already a variable"))
  let
    i = Array.length st.vars
  put st { vars = Array.snoc st.vars v, varIdx = Map.insert v.name i st.varIdx }
  pure i

-------------------------------------------------------------------------------
-- Geometry over ℚ(x)

dist2 :: SPoint -> SPoint -> PM PC
dist2 p q = PF.run (sq (e p.x - e q.x) + sq (e p.y - e q.y))
  where
  sq a = a * a

-- | (q − p) × (r − p)
cross :: SPoint -> SPoint -> SPoint -> PM PC
cross p q r = PF.run ((e q.x - e p.x) * (e r.y - e p.y) - (e q.y - e p.y) * (e r.x - e p.x))

-- | (q − p) · (r − p)
dot :: SPoint -> SPoint -> SPoint -> PM PC
dot p q r = PF.run ((e q.x - e p.x) * (e r.x - e p.x) + (e q.y - e p.y) * (e r.y - e p.y))

lineThrough :: SPoint -> SPoint -> PM { a :: PC, b :: PC, c :: PC }
lineThrough p q = do
  a <- PF.run (e p.y - e q.y)
  b <- PF.run (e q.x - e p.x)
  c <- PF.run (zero - (e a * e p.x + e b * e p.y))
  pure { a, b, c }

samePoint :: SPoint -> SPoint -> PM Boolean
samePoint p q = (&&) <$> PF.eq p.x q.x <*> PF.eq p.y q.y

-- | A line and a circle: the discriminant (> 0 for two points), and the
-- | intersection points F ± √Δ·d once √Δ is known.
type LineCircle = { delta :: PC, fx :: PC, fy :: PC, dx :: PC, dy :: PC }

lineCircle :: { a :: PC, b :: PC, c :: PC } -> { cx :: PC, cy :: PC, r2 :: PC } -> PM LineCircle
lineCircle l c = do
  n2 <- PF.run (e l.a * e l.a + e l.b * e l.b)
  ev <- PF.run (e l.a * e c.cx + e l.b * e c.cy + e l.c)
  delta <- PF.run (e c.r2 * e n2 - e ev * e ev)
  fx <- PF.run (e c.cx - e l.a * e ev / e n2)
  fy <- PF.run (e c.cy - e l.b * e ev / e n2)
  dx <- PF.run (zero - e l.b / e n2)
  dy <- PF.run (e l.a / e n2)
  pure { delta, fx, fy, dx, dy }

pointsOf :: LineCircle -> PM (Tuple SPoint SPoint)
pointsOf lc = do
  s <- PF.sqrtAny lc.delta
  px <- PF.run (e lc.fx + e s * e lc.dx)
  py <- PF.run (e lc.fy + e s * e lc.dy)
  qx <- PF.run (e lc.fx - e s * e lc.dx)
  qy <- PF.run (e lc.fy - e s * e lc.dy)
  pure (Tuple { x: px, y: py } { x: qx, y: qy })

radicalLine :: { cx :: PC, cy :: PC, r2 :: PC } -> { cx :: PC, cy :: PC, r2 :: PC } -> PM { a :: PC, b :: PC, c :: PC }
radicalLine c d = do
  a <- PF.run (two * (e d.cx - e c.cx))
  b <- PF.run (two * (e d.cy - e c.cy))
  k <- PF.run ((e c.cx * e c.cx + e c.cy * e c.cy - e c.r2) - (e d.cx * e d.cx + e d.cy * e d.cy - e d.r2))
  pure { a, b, c: k }
  where
  two = one + one

-------------------------------------------------------------------------------
-- Statements

type Scope = Map Name Int

type Bound = { scope :: Scope, bound :: Array (Tuple Name Int) }

bindNames :: Scope -> Array Name -> Array Int -> M Bound
bindNames scope ns ids = do
  for_ ns \n -> when (Map.member n scope) (failAt (n <> " is already defined"))
  st <- get
  for_ (Array.zip ns ids) \(Tuple n id) ->
    when (Array.elem id st.created) do
      modify_ \s -> s { objs = Map.update (\o -> Just o { name = n }) id s.objs }
  pure { scope: foldl (\sc (Tuple n id) -> Map.insert n id sc) scope (Array.zip ns ids), bound: Array.zip ns ids }

single :: String -> Array Name -> M Name
single what = case _ of
  [ n ] -> pure n
  _ -> failAt (what <> " gives one object; bind exactly one name")

evalStmt :: Scope -> Int -> Stmt -> M Bound
evalStmt scope depth = case _ of
  Given n p -> do
    id <- create n (SPt (constPoint p)) DefGiven
    bindNames scope [ n ] [ id ]
  GivenParam n x y -> do
    let
      coord = case _ of
        CRat q -> pure (PF.fromRational q)
        CParam v -> do
          st <- get
          case Map.lookup v st.varIdx of
            Just i | isNothing (Array.index st.vars i >>= _.free) -> varValue i
            _ -> failAt (v <> " is not a variable (declare it with param " <> v <> " ~ value)")
    px <- coord x
    py <- coord y
    id <- create n (SPt { x: px, y: py }) DefGiven
    bindNames scope [ n ] [ id ]
  Bind ns expr -> do
    ids <- evalExpr scope depth ns expr
    bindNames scope ns ids
  Assert pred -> do
    v <- claim scope pred
    modify_ \st -> st { curVerdicts = Array.snoc st.curVerdicts v }
    unless v.holds (failAt ("assertion not proved: " <> v.text <> " (" <> v.why <> ")"))
    ids <- traverse (\n -> _.id <$> lookupObj scope n) pred
    modify_ \st -> st { claims = Array.snoc st.claims { pred: ids, text: showPred identity pred } }
    pure { scope, bound: [] }
  Assume pred -> do
    assume scope pred
    modify_ \st -> st { curVerdicts = Array.snoc st.curVerdicts { text: showPred identity pred, holds: true, why: "assumed", detail: [] } }
    pure { scope, bound: [] }
  _ -> pure { scope, bound: [] }

evalExpr :: Scope -> Int -> Array Name -> Expr -> M (Array Int)
evalExpr scope depth ns = case _ of
  ELine a b -> do
    n <- single "line" ns
    pa <- lookupObj scope a
    pb <- lookupObj scope b
    p <- asPoint pa
    q <- asPoint pb
    d <- pm (dist2 p q)
    sa <- shown a
    sb <- shown b
    when (PF.isZero d) (failAt (sa <> " and " <> sb <> " are the same point"))
    requireNonZero (sa <> " ≠ " <> sb) d
    l <- pm (lineThrough p q)
    pure <$> create n (SLn l) (DefLine pa.id pb.id)
  ECircle o q -> do
    n <- single "circle" ns
    po <- lookupObj scope o
    pq <- lookupObj scope q
    c <- asPoint po
    p <- asPoint pq
    r2 <- pm (dist2 c p)
    so <- shown o
    sq <- shown q
    when (PF.isZero r2) (failAt (so <> " and " <> sq <> " are the same point (a circle of radius zero)"))
    requireNonZero (so <> " ≠ " <> sq) r2
    pure <$> create n (SCi { cx: c.x, cy: c.y, r2 }) (DefCircle po.id pq.id)
  EMeet a b sel -> meet scope ns a b sel
  ECall f args -> callMacro scope depth ns f args
  EPointOn o near -> do
    n <- single "point on" ns
    oo <- lookupObj scope o
    st <- get
    let
      ptOf id = case Map.lookup id st.objs of
        Just { obj: SPt p } -> Just p
        _ -> Nothing
      frame = case oo.def of
        DefLine a b -> (\p q -> { kind: "line", p, q }) <$> ptOf a <*> ptOf b
        DefCircle c q -> (\p q' -> { kind: "circle", p, q: q' }) <$> ptOf c <*> ptOf q
        _ -> Nothing
    case frame of
      Nothing -> failAt ("point on " <> o <> ": " <> o <> " must be a line or a circle")
      Just fr -> do
        i <- newVar { name: n, text: "t_" <> n, typst: "t_(" <> typstName n <> ")", free: Just { object: o, frame: fr } }
        modify_ \s -> s
          { samples = case near of
              Just pos -> Map.insert n (SVPoint pos) s.samples
              Nothing -> Map.insert n (SVNum (Q.fromInts 1 2)) s.samples
          }
        t <- varValue i
        p <- pm (pointAt fr t)
        pure <$> create n (SPt p) (DefPointOn { obj: oo.id, var: i })
  where
  asPoint o = case o.obj of
    SPt p -> pure p
    _ -> failAt (o.name <> " is not a point")

-- | The point with parameter t (see `Proof.Eval.pointAt`).
pointAt :: { kind :: String, p :: SPoint, q :: SPoint } -> PC -> PM SPoint
pointAt f t
  | f.kind == "line" = do
      x <- PF.run (e f.p.x + e t * (e f.q.x - e f.p.x))
      y <- PF.run (e f.p.y + e t * (e f.q.y - e f.p.y))
      pure { x, y }
  | otherwise = do
      let
        tt = e t * e t
        d = one + tt
        cs = (one - tt) / d
        sn = (e t + e t) / d
        ux = e f.q.x - e f.p.x
        uy = e f.q.y - e f.p.y
      x <- PF.run (e f.p.x + cs * ux - sn * uy)
      y <- PF.run (e f.p.y + sn * ux + cs * uy)
      pure { x, y }

-- | The intersection of two objects for a `meet` statement.
meet :: Scope -> Array Name -> Name -> Name -> Maybe Selector -> M (Array Int)
meet scope ns a b sel = do
  oa <- lookupObj scope a
  ob <- lookupObj scope b
  case oa.obj, ob.obj of
    SLn l, SLn m -> do
      det <- pm (PF.run (e l.a * e m.b - e m.a * e l.b))
      when (PF.isZero det) (failAt (what <> ": the lines are parallel"))
      sa <- shown a
      sb <- shown b
      requireNonZero (sa <> " and " <> sb <> " are not parallel") det
      n <- single what ns
      when (isJust sel) (failAt (what <> ": there is only one intersection point; drop the selector"))
      x <- pm (PF.run ((e l.b * e m.c - e m.b * e l.c) / e det))
      y <- pm (PF.run ((e m.a * e l.c - e l.a * e m.c) / e det))
      pure <$> create n (SPt { x, y }) (meetDef SCUnique Nothing)
    SLn l, SCi c -> lc l c
    SCi c, SLn l -> lc l c
    SCi c, SCi d -> do
      cd <- pm (dist2 { x: c.cx, y: c.cy } { x: d.cx, y: d.cy })
      when (PF.isZero cd) (failAt (what <> ": the circles are concentric"))
      sa <- shown a
      sb <- shown b
      requireNonZero ("the centres of " <> sa <> " and " <> sb <> " differ") cd
      l <- pm (radicalLine c d)
      lc l c
    _, _ -> failAt (what <> ": expected two lines or circles")
  where
  what = "meet " <> a <> " " <> b
  meetDef choice other = DefMeet { a: fromMaybe (-1) (Map.lookup a scope), b: fromMaybe (-1) (Map.lookup b scope), choice, other }
  lc l c = do
    parts <- pm (lineCircle l c)
    case ns, sel of
      [ n ], Nothing -> do
        unless (PF.isZero parts.delta) do
          failAt (what <> ": there are two intersection points except when they touch; bind two names, add a selector, or split with cases meet " <> a <> " " <> b)
        pure <$> create n (SPt { x: parts.fx, y: parts.fy }) (meetDef SCUnique Nothing)
      [ n1, n2 ], Nothing -> do
        sa <- shown a
        sb <- shown b
        requirePositive (sa <> " and " <> sb <> " meet in two points") parts.delta
        Tuple p q <- pm (pointsOf parts)
        st <- get
        let
          unordered = Array.elem (Tuple n1 n2) st.unordered
        Tuple first second <- if unordered then pure (Tuple p q) else lexOrder p q
        i <- create n1 (SPt first) (meetDef (if unordered then SCPair else SCLexFirst) (Just second))
        j <- create n2 (SPt second) (meetDef (if unordered then SCPair else SCLexSecond) (Just first))
        pure [ i, j ]
      [ n ], Just s -> do
        sa <- shown a
        sb <- shown b
        requirePositive (sa <> " and " <> sb <> " meet in two points") parts.delta
        Tuple p q <- pm (pointsOf parts)
        r <- choose scope what s p q
        pure <$> create n (SPt r.chosen) (meetDef r.choice (Just r.other))
      _, _ -> failAt (what <> ": bind one or two names")

-- | The lexicographically smaller point first.
lexOrder :: SPoint -> SPoint -> M (Tuple SPoint SPoint)
lexOrder p q = do
  dx <- pm (PF.sub p.x q.x)
  o <-
    if PF.isZero dx then do
      dy <- pm (PF.sub p.y q.y)
      decideStrict "which intersection point comes first (by y)" hint dy
    else decideStrict "which intersection point comes first (by x)" hint dx
  pure (if o == LT then Tuple p q else Tuple q p)
  where
  hint = "; name them with a selector instead (left, right, nearest, …)"

choose :: Scope -> String -> Selector -> SPoint -> SPoint -> M { chosen :: SPoint, other :: SPoint, choice :: SChoice }
choose scope what sel p q = case sel of
  Except r -> do
    rp <- refPoint r
    sp <- pm (samePoint p rp)
    sq <- pm (samePoint q rp)
    ref <- sref r
    if sp then pure { chosen: q, other: p, choice: SCExcept ref }
    else if sq then pure { chosen: p, other: q, choice: SCExcept ref }
    else failAt (what <> ": " <> refText r <> " is not one of the intersection points for every configuration")
  Nearest r -> do
    ref <- sref r
    pick (SCNearest ref) <$> byDistance r LT
  Farthest r -> do
    ref <- sref r
    pick (SCFarthest ref) <$> byDistance r GT
  LeftOf a b -> do
    ia <- _.id <$> lookupObj scope a
    ib <- _.id <$> lookupObj scope b
    pick (SCLeft ia ib) <$> bySide a b GT "left of"
  RightOf a b -> do
    ia <- _.id <$> lookupObj scope a
    ib <- _.id <$> lookupObj scope b
    pick (SCRight ia ib) <$> bySide a b LT "right of"
  where
  pick choice chosen = { chosen, other: if unsafeRefEq chosen p then q else p, choice }
  sref = case _ of
    RefName n -> SRefObj <<< _.id <$> lookupObj scope n
    RefLit lit -> pure (SRefLit lit)
  refText (RefName n) = n
  refText (RefLit lit) = "(" <> show lit.x <> ", " <> show lit.y <> ")"
  refPoint = case _ of
    RefName n -> pointOf scope n
    RefLit lit -> pure (constPoint lit)
  byDistance r want = do
    rp <- refPoint r
    d <- pm do
      dp <- dist2 p rp
      dq <- dist2 q rp
      PF.sub dp dq
    o <- decideStrict (what <> ": which point is nearer to " <> refText r) "; split with cases compare" d
    case o of
      EQ -> failAt (what <> ": both points are equally far from " <> refText r)
      _ -> pure (if o == want then p else q)
  bySide a b want label = do
    pa <- pointOf scope a
    pb <- pointOf scope b
    cp <- pm (cross pa pb p)
    cq <- pm (cross pa pb q)
    sp <- decideStrict (what <> ": the side of " <> a <> " → " <> b <> " of the first point") hint cp
    sq <- decideStrict (what <> ": the side of " <> a <> " → " <> b <> " of the second point") hint cq
    case sp == want, sq == want of
      true, false -> pure p
      false, true -> pure q
      _, _ -> failAt (what <> ": " <> (if sp == want then "both" else "neither") <> " of the points lie " <> label <> " " <> a <> " → " <> b)
    where
    hint = "; split with cases side"

-------------------------------------------------------------------------------
-- Macros (as in Proof.Eval)

callMacro :: Scope -> Int -> Array Name -> Name -> Array Name -> M (Array Int)
callMacro scope depth ns f args = do
  st <- get
  m <- case Map.lookup f st.macros of
    Just m -> pure m
    Nothing -> failAt ("unknown macro " <> f)
  when (Array.length args /= Array.length m.params) (failAt (f <> " takes " <> show (Array.length m.params) <> " arguments"))
  when (Array.length ns /= Array.length m.results) (failAt (f <> " gives " <> show (Array.length m.results) <> " objects"))
  when (depth > 32) (failAt ("recursive call of macro " <> f))
  argIds <- traverse (\n -> _.id <$> lookupObj scope n) args
  outerNames <- traverse shown args
  let
    local = Map.fromFoldable (Array.zip m.params argIds)
    label = f <> "(" <> joinWith ", " outerNames <> ")"
    locals = Array.concatMap
      ( \s -> case s.value of
          Bind bs _ -> bs
          _ -> []
      )
      m.body
    rename = Map.fromFoldable (Array.zip m.params outerNames <> map (\n -> Tuple n (n <> " of " <> label)) locals)
  saved <- get
  modify_ _ { rename = rename, unordered = unorderedPairs m.results m.body }
  final <- foldM (\sc s -> _.scope <$> evalStmt sc (depth + 1) s.value) local m.body
  modify_ _ { rename = saved.rename, unordered = saved.unordered }
  traverse
    ( \r -> case Map.lookup r final of
        Just id -> pure id
        Nothing -> failAt ("macro " <> f <> " does not define its result " <> r)
    )
    m.results

-------------------------------------------------------------------------------
-- Claims and assumptions

identity' :: String -> Array PC -> M PVerdict
identity' text xs = pure
  { text
  , holds: all PF.isZero xs
  , why: if all PF.isZero xs then "an identity in the variables" else "not an identity"
  , detail: []
  }

-- | A strict or weak sign claim on a value.
signClaim :: String -> PC -> (Sgn -> Boolean) -> String -> M PVerdict
signClaim text x ok wanted = do
  s <- pm (PF.sign x)
  st <- get
  let
    why = case PF.toRatFun x of
      Just f -> answerWhy (S.signOfRat (context st) f)
      Nothing -> "from the signs of its parts"
  pure
    { text
    , holds: ok s
    , why: if ok s then why else wanted <> " is not proved (" <> sgnText s <> witnesses st x <> ")"
    , detail: [ { label: "value", value: x } ]
    }

answerWhy :: Answer -> String
answerWhy = case _ of
  APos w -> w
  ANeg w -> w
  ANonNeg w -> w
  ANonPos w -> w
  AZero -> "zero"
  AUnknown w -> w
  ANonZero w -> w
  AVaries _ _ -> "the sign changes"

sgnText :: Sgn -> String
sgnText = case _ of
  Pos -> "positive"
  Neg -> "negative"
  Zero -> "zero"
  NonNeg -> "only ≥ 0 is known"
  NonPos -> "only ≤ 0 is known"
  Unknown -> "the sign is not determined"

claim :: Scope -> PredF Name -> M PVerdict
claim scope pred = case pred of
  On p x -> do
    pt <- pointOf scope p
    ox <- lookupObj scope x
    v <- case ox.obj of
      SLn l -> pm (PF.run (e l.a * e pt.x + e l.b * e pt.y + e l.c))
      SCi c -> pm do
        d <- dist2 pt { x: c.cx, y: c.cy }
        PF.sub d c.r2
      _ -> failAt (x <> " is a point; 'on' expects a line or a circle")
    identity' text [ v ]
  PointEq p q -> do
    a <- pointOf scope p
    b <- pointOf scope q
    dx <- pm (PF.sub a.x b.x)
    dy <- pm (PF.sub a.y b.y)
    identity' text [ dx, dy ]
  PointNe p q -> do
    a <- pointOf scope p
    b <- pointOf scope q
    d <- pm (dist2 a b)
    signClaim text d (_ == Pos) "≠"
  DistCmp c a b d f -> do
    pa <- pointOf scope a
    pb <- pointOf scope b
    pd <- pointOf scope d
    pf <- pointOf scope f
    d1 <- pm (dist2 pa pb)
    d2 <- pm (dist2 pd pf)
    diff <- pm (PF.sub d1 d2)
    v <- case c of
      CEq -> identity' text [ diff ]
      CNe -> signClaim text diff (\s -> s == Pos || s == Neg) "≠"
      CLt -> signClaim text diff (_ == Neg) "<"
      CLe -> signClaim text diff (\s -> s == Neg || s == NonPos || s == Zero) "≤"
      CGt -> signClaim text diff (_ == Pos) ">"
      CGe -> signClaim text diff (\s -> s == Pos || s == NonNeg || s == Zero) "≥"
    pure v { detail = [ { label: "|" <> a <> b <> "|²", value: d1 }, { label: "|" <> d <> f <> "|²", value: d2 } ] }
  Collinear ps -> do
    pts <- traverse (pointOf scope) ps
    when (Array.length pts < 3) (failAt "collinear needs at least three points")
    case Array.uncons pts of
      Nothing -> identity' text []
      Just { head: p0, tail } -> do
        others <- Array.filterA (\q -> not <$> pm (samePoint p0 q)) tail
        case Array.head others of
          Nothing -> identity' text []
          Just p1 -> do
            cs <- traverse (\r -> pm (cross p0 p1 r)) tail
            identity' text cs
  Concyclic ps -> do
    pts <- traverse (pointOf scope) ps
    when (Array.length pts < 4) (failAt "concyclic needs at least four points")
    case pts of
      _ | Just a <- Array.index pts 0, Just b <- Array.index pts 1, Just c <- Array.index pts 2 -> do
        cr <- pm (cross a b c)
        requireNonZero ("the first three points of " <> text <> " are not collinear") cr
        o <- pm (circumcentre a b c)
        r2 <- pm (dist2 o a)
        ds <- traverse (\p -> pm (dist2 o p >>= \d -> PF.sub d r2)) (Array.drop 3 pts)
        identity' text ds
      _ -> failAt "concyclic needs at least four points"
  Parallel l m -> do
    a <- lineOf scope l
    b <- lineOf scope m
    det <- pm (PF.run (e a.a * e b.b - e b.a * e a.b))
    identity' text [ det ]
  Perpendicular l m -> do
    a <- lineOf scope l
    b <- lineOf scope m
    d <- pm (PF.run (e a.a * e b.a + e a.b * e b.b))
    identity' text [ d ]
  AngleEq a b c d f g -> do
    pa <- pointOf scope a
    pb <- pointOf scope b
    pc <- pointOf scope c
    pd <- pointOf scope d
    pf <- pointOf scope f
    pg <- pointOf scope g
    ba <- pm (dist2 pb pa)
    bc <- pm (dist2 pb pc)
    fd <- pm (dist2 pf pd)
    fg <- pm (dist2 pf pg)
    for_ [ Tuple ba (b <> " ≠ " <> a), Tuple bc (b <> " ≠ " <> c), Tuple fd (f <> " ≠ " <> d), Tuple fg (f <> " ≠ " <> g) ] \(Tuple v t) ->
      requireNonZero t v
    d1 <- pm (dot pb pa pc)
    d2 <- pm (dot pf pd pg)
    eqn <- pm (PF.run (e d1 * e d1 * e fd * e fg - e d2 * e d2 * e ba * e bc))
    s1 <- decideStrict ("the sign of cos ∠" <> a <> b <> c) "" d1
    s2 <- decideStrict ("the sign of cos ∠" <> d <> f <> g) "" d2
    pure
      { text
      , holds: PF.isZero eqn && s1 == s2
      , why: if PF.isZero eqn && s1 == s2 then "equal cosines: an identity" else "the cosines differ"
      , detail: []
      }
  Ccw a b c -> do
    pa <- pointOf scope a
    pb <- pointOf scope b
    pc <- pointOf scope c
    x <- pm (cross pa pb pc)
    v <- signClaim text x (_ == Pos) "> 0"
    pure v { detail = [ { label: "(" <> b <> " − " <> a <> ") × (" <> c <> " − " <> a <> ")", value: x } ] }
  Between a b c -> do
    pa <- pointOf scope a
    pb <- pointOf scope b
    pc <- pointOf scope c
    x <- pm (cross pa pb pc)
    d <- pm (dot pb pa pc)
    v <- signClaim text d (_ == Neg) "(A − B)·(C − B) < 0"
    pure v { holds = v.holds && PF.isZero x, why = if PF.isZero x then v.why else "not collinear" }
  where
  text = showPred identity pred

circumcentre :: SPoint -> SPoint -> SPoint -> PM SPoint
circumcentre a b c = do
  let
    bx = e b.x - e a.x
    by = e b.y - e a.y
    cx = e c.x - e a.x
    cy = e c.y - e a.y
    d = (one + one) * (bx * cy - by * cx)
    b2 = bx * bx + by * by
    c2 = cx * cx + cy * cy
  x <- PF.run (e a.x + (cy * b2 - by * c2) / d)
  y <- PF.run (e a.y + (bx * c2 - cx * b2) / d)
  pure { x, y }

-- | A condition on a value as polynomials: (num·den, rel).
condPoly :: String -> PC -> M Poly
condPoly what x = case PF.toRatFun x of
  Just f -> pure (RF.num f * RF.den f)
  Nothing -> failAt (what <> ": the condition involves square roots, which is not supported here")

eqPoly :: String -> PC -> M Poly
eqPoly what x = case PF.toRatFun x of
  Just f -> pure (RF.num f)
  Nothing -> failAt (what <> ": the condition involves square roots, which is not supported here")

assume :: Scope -> PredF Name -> M Unit
assume scope pred = case pred of
  Ccw a b c -> do
    pa <- pointOf scope a
    pb <- pointOf scope b
    pc <- pointOf scope c
    x <- pm (cross pa pb pc)
    p <- condPoly text x
    ineq p RPos
  PointNe p q -> do
    a <- pointOf scope p
    b <- pointOf scope q
    d <- pm (dist2 a b)
    p' <- condPoly text d
    ineq p' RPos
  DistCmp c a b d f -> do
    pa <- pointOf scope a
    pb <- pointOf scope b
    pd <- pointOf scope d
    pf <- pointOf scope f
    diff <- pm (dist2 pa pb >>= \x -> dist2 pd pf >>= \y -> PF.sub x y)
    case c of
      CEq -> eqPoly text diff >>= \p -> equation [ p ]
      _ -> do
        p <- condPoly text diff
        ineq p case c of
          CNe -> RNonZero
          CLt -> RNeg
          CLe -> RNonPos
          CGt -> RPos
          _ -> RNonNeg
  PointEq p q -> do
    a <- pointOf scope p
    b <- pointOf scope q
    dx <- pm (PF.sub a.x b.x) >>= eqPoly text
    dy <- pm (PF.sub a.y b.y) >>= eqPoly text
    equation [ dx, dy ]
  _ -> do
    -- the remaining predicates are equations: on, collinear (3), concyclic
    -- (4), parallel, perpendicular
    f <- predEquation scope pred
    case f of
      Just p -> equation [ p ]
      Nothing -> failAt ("assume " <> text <> " is not supported; construct the configuration instead")
  where
  text = showPred identity pred
  ineq p rel = do
    st <- get
    let
      contradicts = case S.signOf (context st) p, rel of
        APos _, RNeg -> true
        APos _, RNonPos -> true
        ANeg _, RPos -> true
        ANeg _, RNonNeg -> true
        AZero, RPos -> true
        AZero, RNeg -> true
        AZero, RNonZero -> true
        _, _ -> false
    when contradicts (failAt ("the assumption " <> text <> " never holds"))
    addHyp Assumed text p rel
  equation ps = do
    st <- get
    case solve st ps of
      Left msg -> failAt ("assume " <> text <> ": " <> msg)
      -- an identity after the substitutions so far
      Right [ o ] | Map.size o.subst == Map.size st.subst -> pure unit
      Right opts -> throwError (Restart opts)

-- | The equation of an equality predicate (one polynomial), when it has one.
predEquation :: Scope -> PredF Name -> M (Maybe Poly)
predEquation scope pred = case pred of
  On p x -> do
    pt <- pointOf scope p
    ox <- lookupObj scope x
    v <- case ox.obj of
      SLn l -> pm (PF.run (e l.a * e pt.x + e l.b * e pt.y + e l.c))
      SCi c -> pm (dist2 pt { x: c.cx, y: c.cy } >>= \d -> PF.sub d c.r2)
      _ -> failAt (x <> " is a point")
    Just <$> eqPoly text v
  Parallel l m -> do
    a <- lineOf scope l
    b <- lineOf scope m
    v <- pm (PF.run (e a.a * e b.b - e b.a * e a.b))
    Just <$> eqPoly text v
  Perpendicular l m -> do
    a <- lineOf scope l
    b <- lineOf scope m
    v <- pm (PF.run (e a.a * e b.a + e a.b * e b.b))
    Just <$> eqPoly text v
  Collinear [ a, b, c ] -> do
    pa <- pointOf scope a
    pb <- pointOf scope b
    pc <- pointOf scope c
    v <- pm (cross pa pb pc)
    Just <$> eqPoly text v
  Concyclic [ a, b, c, d ] -> do
    pa <- pointOf scope a
    pb <- pointOf scope b
    pc <- pointOf scope c
    pd <- pointOf scope d
    -- with A as the origin: det [[bx, by, |b|²], [cx, cy, |c|²], [dx, dy, |d|²]]
    v <- pm do
      let
        rel p = { x: e p.x - e pa.x, y: e p.y - e pa.y }
        u = rel pb
        w = rel pc
        z = rel pd
        n q = q.x * q.x + q.y * q.y
      PF.run
        ( u.x * (w.y * n z - n w * z.y)
            - u.y * (w.x * n z - n w * z.x)
            + n u * (w.x * z.y - w.y * z.x)
        )
    Just <$> eqPoly text v
  _ -> pure Nothing
  where
  text = showPred identity pred

-------------------------------------------------------------------------------
-- Solving equations (for equality branches and assumptions)

-- | Substitutes in a polynomial; the result's numerator.
substPoly :: Map Int RatFun -> Poly -> Poly
substPoly s p = RF.num (substRat s (RF.fromPoly p))

substRat :: Map Int RatFun -> RatFun -> RatFun
substRat s f = foldl (\acc (Tuple v g) -> RF.substFrac acc v (RF.num g) (RF.den g)) f (Map.toUnfoldable s :: Array (Tuple Int RatFun))

-- | The solutions of the equations ps = 0, one variable at a time: by the
-- | rational roots of a polynomial in one variable, or by solving an equation
-- | that is linear in some variable.
solve :: St -> Array Poly -> Either String (Array Option)
solve st ps = Array.nubByEq (\x y -> x.text == y.text) <$> go { subst: st.subst, text: [], hyps: [] } (Array.toUnfoldable ps)
  where
  n = names st
  go acc qs = case Array.uncons qs of
    Nothing -> Right [ { subst: acc.subst, text: joinWith ", " acc.text, hyps: acc.hyps } ]
    Just { head: p, tail } ->
      let
        q = substPoly acc.subst p
      in
        if P.isZero q then go acc tail
        else case P.constValue q of
          Just _ -> Right []
          Nothing -> case P.vars q of
            [ v ] -> do
              u <- noteE "internal: univariate" (P.toUni q v)
              roots <- noteE ("cannot enumerate the rational roots of " <> Sym.polyText n q <> " = 0") (P.ratRoots u)
              let
                rest = foldl (\f r -> divideOut f (P.var v - P.constant r)) q roots
              when (P.degIn rest v > 0 && hasRealRoot rest v) do
                Left (Sym.polyText n q <> " = 0 has irrational solutions; re-parametrise so that the case is a rational value")
              opts <- traverse (\r -> go (extend acc v (RF.fromRational r) (n.text v <> " = " <> Sym.ratText n (RF.fromRational r)) []) tail) roots
              pure (Array.concat opts)
            vs -> case Array.find (\v -> P.degIn q v == 1) (Array.sortWith (\v -> if isConstCoeff q v then 0 else 1) vs) of
              Nothing -> Left ("cannot solve " <> Sym.polyText n q <> " = 0 for one variable; re-parametrise")
              Just v ->
                let
                  cs = P.coeffsIn q v
                  a = fromMaybe zero (Array.index cs 1)
                  b = fromMaybe zero (Array.index cs 0)
                  value = RF.fromPoly (zero - b) / RF.fromPoly a
                  side = case P.constValue a of
                    Just _ -> []
                    Nothing -> [ { poly: a, rel: RNonZero, text: "the equation can be solved for " <> n.text v } ]
                in
                  go (extend acc v value (n.text v <> " = " <> Sym.ratText n value) side) tail
  extend acc v value text side =
    let
      one' = Map.singleton v value
      s' = map (substRat one') acc.subst
    in
      { subst: Map.insert v value s', text: Array.snoc acc.text text, hyps: acc.hyps <> side }
  isConstCoeff q v = isJust (Array.index (P.coeffsIn q v) 1 >>= P.constValue)
  divideOut f g = case P.divExact f g of
    Just h -> divideOut h g
    Nothing -> f
  hasRealRoot f v = case P.toUni f v of
    Just u -> Array.any _.root (P.cells [ u ])
    Nothing -> true

noteE :: forall a. String -> Maybe a -> Either String a
noteE msg = case _ of
  Just x -> Right x
  Nothing -> Left msg

-------------------------------------------------------------------------------
-- Branch conditions

data Cond
  = CondIneq (Array { poly :: Poly, rel :: Rel, text :: String })
  | CondEq (Array Poly) String
  | CondImpossible String

branchCondition :: Scope -> CaseForm -> Label -> M Cond
branchCondition scope form label = case form of
  CaseMeet a b -> do
    oa <- lookupObj scope a
    ob <- lookupObj scope b
    case oa.obj, ob.obj of
      SLn l, SLn m -> do
        det <- pm (PF.run (e l.a * e m.b - e m.a * e l.b))
        case label of
          LOne ->
            if PF.isZero det then pure (CondImpossible "the lines are parallel")
            else (\p -> CondIneq [ { poly: p, rel: RNonZero, text: a <> " and " <> b <> " are not parallel" } ]) <$> eqPoly "meet" det
          LTwo -> pure (CondImpossible "two lines meet in at most one point")
          _ -> do
            -- the lines are the same when, besides det = 0, u = v = 0
            u <- pm (PF.run (e l.a * e m.c - e m.a * e l.c))
            v <- pm (PF.run (e l.b * e m.c - e m.b * e l.c))
            let
              same = PF.isZero u && PF.isZero v
            case label of
              LSame ->
                if PF.isZero det && same then pure (CondIneq [])
                else do
                  ps <- traverse (eqPoly "meet") (Array.filter (not <<< PF.isZero) [ det, u, v ])
                  pure (CondEq ps (a <> " = " <> b))
              _ ->
                if PF.isZero det then
                  if same then pure (CondImpossible "the lines are the same")
                  else do
                    pu <- eqPoly "meet" u
                    pv <- eqPoly "meet" v
                    pure (CondIneq [ { poly: pu * pu + pv * pv, rel: RNonZero, text: a <> " and " <> b <> " are distinct" } ])
                else (\p -> CondEq [ p ] (a <> " ∥ " <> b)) <$> eqPoly "meet" det
      SLn l, SCi c -> lineCircleCond l c false
      SCi c, SLn l -> lineCircleCond l c false
      SCi c, SCi d -> do
        cd <- pm (dist2 { x: c.cx, y: c.cy } { x: d.cx, y: d.cy })
        case label of
          LSame -> do
            dx <- pm (PF.sub c.cx d.cx) >>= eqPoly "same"
            dy <- pm (PF.sub c.cy d.cy) >>= eqPoly "same"
            dr <- pm (PF.sub c.r2 d.r2) >>= eqPoly "same"
            st <- get
            if S.isNonZero (context st) (RF.num (fromMaybe one (PF.toRatFun cd))) then pure (CondImpossible "the centres differ")
            else pure (CondEq [ dx, dy, dr ] (a <> " = " <> b))
          _ -> do
            when (PF.isZero cd) (failAt ("cases meet " <> a <> " " <> b <> ": the circles are concentric"))
            requireNonZero ("the centres of " <> a <> " and " <> b <> " differ") cd
            l <- pm (radicalLine c d)
            lineCircleCond l c true
      _, _ -> failAt "cases meet: expected two lines or circles"
    where
    lineCircleCond l c circles = do
      parts <- pm (lineCircle l c)
      case label of
        LTwo -> (\p -> CondIneq [ { poly: p, rel: RPos, text: a <> " and " <> b <> " meet in two points" } ]) <$> condPoly "meet" parts.delta
        LNone -> (\p -> CondIneq [ { poly: p, rel: RNeg, text: a <> " and " <> b <> " do not meet" } ]) <$> condPoly "meet" parts.delta
        LOne -> (\p -> CondEq [ p ] (a <> " and " <> b <> " touch")) <$> eqPoly "meet" parts.delta
        _ -> pure (CondImpossible (if circles then "the centres differ" else "a line is not a circle"))
  CaseSide p a b -> do
    pa <- pointOf scope a
    pb <- pointOf scope b
    pp <- pointOf scope p
    d <- pm (dist2 pa pb)
    requireNonZero (a <> " ≠ " <> b) d
    x <- pm (cross pa pb pp)
    case label of
      LLeft -> (\q -> CondIneq [ { poly: q, rel: RPos, text: p <> " is left of " <> a <> " → " <> b } ]) <$> condPoly "side" x
      LRight -> (\q -> CondIneq [ { poly: q, rel: RNeg, text: p <> " is right of " <> a <> " → " <> b } ]) <$> condPoly "side" x
      _ -> (\q -> CondEq [ q ] (p <> " is on " <> a <> b)) <$> eqPoly "side" x
  CaseCompare a b c d -> do
    pa <- pointOf scope a
    pb <- pointOf scope b
    pc <- pointOf scope c
    pd <- pointOf scope d
    x <- pm (dist2 pa pb >>= \u -> dist2 pc pd >>= \v -> PF.sub u v)
    let
      t = "|" <> a <> b <> "| "
      t' = " |" <> c <> d <> "|"
    case label of
      LLess -> (\q -> CondIneq [ { poly: q, rel: RNeg, text: t <> "<" <> t' } ]) <$> condPoly "compare" x
      LGreater -> (\q -> CondIneq [ { poly: q, rel: RPos, text: t <> ">" <> t' } ]) <$> condPoly "compare" x
      _ -> (\q -> CondEq [ q ] (t <> "=" <> t')) <$> eqPoly "compare" x
  CasePred (PointEq p q) -> do
    a <- pointOf scope p
    b <- pointOf scope q
    case label of
      LEqual -> do
        dx <- pm (PF.sub a.x b.x) >>= eqPoly "=="
        dy <- pm (PF.sub a.y b.y) >>= eqPoly "=="
        pure (CondEq [ dx, dy ] (p <> " = " <> q))
      _ -> do
        d <- pm (dist2 a b)
        (\x -> CondIneq [ { poly: x, rel: RPos, text: p <> " ≠ " <> q } ]) <$> condPoly "!=" d
  CasePred pred -> case pred of
    Ccw a b c -> do
      pa <- pointOf scope a
      pb <- pointOf scope b
      pc <- pointOf scope c
      x <- pm (cross pa pb pc) >>= condPoly "ccw"
      pure case label of
        LYes -> CondIneq [ { poly: x, rel: RPos, text: "ccw " <> a <> " " <> b <> " " <> c } ]
        _ -> CondIneq [ { poly: x, rel: RNonPos, text: "not ccw " <> a <> " " <> b <> " " <> c } ]
    DistCmp c a b d f -> do
      pa <- pointOf scope a
      pb <- pointOf scope b
      pd <- pointOf scope d
      pf <- pointOf scope f
      x <- pm (dist2 pa pb >>= \u -> dist2 pd pf >>= \v -> PF.sub u v)
      let
        yes = label == LYes
        t = showPred identity pred
        ineq rel = (\q -> CondIneq [ { poly: q, rel, text: (if yes then "" else "not ") <> t } ]) <$> condPoly "dist" x
      case c, yes of
        CEq, true -> (\q -> CondEq [ q ] t) <$> eqPoly "dist" x
        CEq, false -> ineq RNonZero
        CNe, true -> ineq RNonZero
        CNe, false -> (\q -> CondEq [ q ] ("not " <> t)) <$> eqPoly "dist" x
        CLt, true -> ineq RNeg
        CLt, false -> ineq RNonNeg
        CLe, true -> ineq RNonPos
        CLe, false -> ineq RPos
        CGt, true -> ineq RPos
        CGt, false -> ineq RNonPos
        CGe, true -> ineq RNonNeg
        CGe, false -> ineq RNeg
    _ -> do
      f <- predEquation scope pred
      case f of
        Nothing -> failAt ("cases " <> showPred identity pred <> " is not supported; use side or compare")
        Just q -> pure case label of
          LYes -> CondEq [ q ] (showPred identity pred)
          _ -> CondIneq [ { poly: q, rel: RNonZero, text: "not " <> showPred identity pred } ]

-- | The points a branch binds (`one T`, `two T1, T2`), after its condition.
caseObjects :: Scope -> CaseForm -> Branch -> M Bound
caseObjects scope form br = case form, br.label, br.names of
  CaseMeet a b, LOne, [ n ] -> do
    oa <- lookupObj scope a
    ob <- lookupObj scope b
    p <- case oa.obj, ob.obj of
      SLn l, SLn m -> do
        det <- pm (PF.run (e l.a * e m.b - e m.a * e l.b))
        x <- pm (PF.run ((e l.b * e m.c - e m.b * e l.c) / e det))
        y <- pm (PF.run ((e m.a * e l.c - e l.a * e m.c) / e det))
        pure { x, y }
      SLn l, SCi c -> foot l c
      SCi c, SLn l -> foot l c
      SCi c, SCi d -> pm (radicalLine c d) >>= \l -> foot l c
      _, _ -> failAt "cases meet: expected two lines or circles"
    id <- create n (SPt p) (DefMeet { a: oa.id, b: ob.id, choice: SCUnique, other: Nothing })
    bindNames scope [ n ] [ id ]
  CaseMeet a b, LTwo, [ n1, n2 ] -> do
    oa <- lookupObj scope a
    ob <- lookupObj scope b
    parts <- case oa.obj, ob.obj of
      SLn l, SCi c -> pm (lineCircle l c)
      SCi c, SLn l -> pm (lineCircle l c)
      SCi c, SCi d -> pm (radicalLine c d >>= \l -> lineCircle l c)
      _, _ -> failAt "two intersection points need a circle"
    Tuple p q <- pm (pointsOf parts)
    Tuple first second <- lexOrder p q
    i <- create n1 (SPt first) (DefMeet { a: oa.id, b: ob.id, choice: SCLexFirst, other: Just second })
    j <- create n2 (SPt second) (DefMeet { a: oa.id, b: ob.id, choice: SCLexSecond, other: Just first })
    bindNames scope [ n1, n2 ] [ i, j ]
  _, _, _ -> pure { scope, bound: [] }
  where
  foot l c = do
    parts <- pm (lineCircle l c)
    unless (PF.isZero parts.delta) (failAt "internal: the circles do not touch after substitution")
    pure { x: parts.fx, y: parts.fy }

-------------------------------------------------------------------------------
-- Walking the script

type Forced = { line :: Int, label :: Label, option :: Int }

type Env =
  { script :: Script
  , forced :: Array Forced -- choices to replay before recording (substitutions)
  , path :: Array Forced -- choices made so far
  , subst :: Map Int RatFun
  , extra :: Array { poly :: Poly, rel :: Rel, text :: String }
  }

type Walked = { node :: PNode, reached :: Boolean }

initialSt :: Env -> St
initialSt env =
  { objs: Map.empty
  , next: 0
  , scope: Map.empty
  , hyps: map (\h -> { poly: h.poly, rel: h.rel, origin: Computed, line: 0, step: 0, text: h.text }) env.extra
  , vars: []
  , varIdx: Map.empty
  , subst: env.subst
  , samples: Map.empty
  , sampled: false
  , step: 0
  , line: 0
  , curHyps: []
  , curVerdicts: []
  , created: []
  , rename: Map.empty
  , unordered: unorderedPairs [] env.script.body
  , claims: []
  , macros: Map.union (Map.fromFoldable (map (\m -> Tuple m.name m) env.script.macros))
      (Map.fromFoldable (map (\m -> Tuple m.name m) Prelude.macros))
  }

-- | The configuration drawn for the current samples: variables (substituted
-- | ones by their value) and free points by their parameter.
figureOf :: St -> Map Name SampleValue
figureOf st =
  let
    vals = valuationOf st st.samples
  in
    Map.fromFoldable
      ( Array.catMaybes
          (Array.mapWithIndex (\i v -> (\q -> Tuple v.name (SVNum q)) <$> join (Array.index vals i)) st.vars)
      )

-- | Drops the computed hypotheses that follow from the others.
essential :: St -> Array Hyp
essential st = foldl keep [] (Array.mapWithIndex Tuple st.hyps)
  where
  keep acc (Tuple i h)
    | h.origin /= Computed = Array.snoc acc h
    | otherwise =
        let
          others = Array.filter (\(Tuple j _) -> j /= i) (Array.mapWithIndex Tuple st.hyps)
          kept = map (\(Tuple _ x) -> { poly: x.poly, rel: x.rel })
            (Array.filter (\(Tuple j x) -> j > i || Array.any (\y -> y.poly == x.poly && y.rel == x.rel) acc) others)
          ctx = { conds: kept, samples: (context st).samples }
          implied = case S.signOf ctx h.poly, h.rel of
            APos _, RPos -> true
            APos _, RNonNeg -> true
            APos _, RNonZero -> true
            ANeg _, RNeg -> true
            ANeg _, RNonPos -> true
            ANeg _, RNonZero -> true
            ANonNeg _, RNonNeg -> true
            ANonPos _, RNonPos -> true
            ANonZero _, RNonZero -> true
            _, RNonZero -> S.isNonZero ctx h.poly
            _, _ -> false
        in
          if implied then acc else Array.snoc acc h

mkNode :: St -> Array PStep -> Maybe PSplit -> Maybe PError -> PNode
mkNode st steps split error = PNode
  { steps
  , split
  , error
  , hyps: st.hyps
  , theorem: essential st
  , vars: st.vars
  , subst: map (\(Tuple v f) -> { var: (names st).text v, value: Sym.ratText (names st) f }) (Map.toUnfoldable st.subst)
  , substMap: st.subst
  , objects: Array.fromFoldable (Map.values st.objs)
  , named: Array.fromFoldable (Map.values st.scope)
  , claims: st.claims
  }

-- | Runs one statement as a step.
runStep :: Env -> St -> Located Stmt -> Tuple (Either Signal PStep) St
runStep _ st0 stmt =
  let
    st1 = st0 { line = stmt.line, curHyps = [], curVerdicts = [], created = [] }
    Tuple r st2 = runState (runExceptT (evalStmt st1.scope 0 stmt.value)) st1
  in
    case r of
      Left sig -> Tuple (Left sig) st2
      Right b ->
        let
          objects = Array.mapMaybe
            (\(Tuple n id) -> (\o -> { name: n, obj: o.obj }) <$> Map.lookup id st2.objs)
            b.bound
          step =
            { index: st0.step
            , line: stmt.line
            , text: stmt.text
            , kind: case stmt.value of
                Given _ _ -> SGiven
                GivenParam _ _ _ -> SGiven
                Assert _ -> SAssert
                Assume _ -> SAssume
                _ -> SConstruct
            , objects
            , hyps: st2.curHyps
            , verdicts: st2.curVerdicts
            , notes: []
            , snapshot: false
            , figure: figureOf st2
            }
        in
          Tuple (Right step) st2 { scope = b.scope, step = st0.step + 1 }

-- | The samples a branch sets before its first step: they apply to the
-- | branch's case step too.
leadingSamples :: Array (Located Stmt) -> Array SampleItem
leadingSamples body = Array.concatMap items (Array.takeWhile lead body)
  where
  lead s = case s.value of
    Sample _ -> true
    Note _ -> true
    _ -> false
  items s = case s.value of
    Sample is -> is
    _ -> []

withSamples :: Array SampleItem -> St -> St
withSamples items st = st
  { samples = foldl (\m i -> Map.insert i.name i.value m) st.samples items
  , sampled = st.sampled || not (Array.null items)
  }

walk :: Env -> St -> Array PStep -> Array (Located Stmt) -> Walked
walk env st acc stmts = case Array.uncons stmts of
  Nothing -> leaf env st acc
  Just { head: stmt, tail } -> case stmt.value of
    Note text -> walk env st (mapLast (\s -> s { notes = Array.snoc s.notes text }) acc) tail
    Snapshot items ->
      let
        st' = withSamples items st
      in
        walk env st' (mapLast (\s -> s { snapshot = true }) acc) tail
    Sample items -> walk env (withSamples items st) acc tail
    -- drawing only; its points must exist
    Show item ->
      let
        used = case item of
          ShowSegment a b -> [ a, b ]
          ShowPolygon ps -> ps
          ShowCircle o a -> [ o, a ]
          ShowThrough a b c -> [ a, b, c ]
        bad = Array.find
          ( \n -> case Map.lookup n st.scope >>= \i -> Map.lookup i st.objs of
              Just { obj: SPt _ } -> false
              _ -> true
          )
          used
      in
        case bad of
          Just n -> { node: mkNode st acc Nothing (Just { line: stmt.line, message: "show: " <> n <> " is not a point" }), reached: Array.null env.forced }
          Nothing -> walk env st acc tail
    Param ps ->
      let
        declare s d = case runState (runExceptT (newVar { name: d.name, text: d.name, typst: typstName d.name, free: Nothing })) s of
          Tuple (Left _) s' -> s'
          Tuple (Right _) s' -> s' { samples = Map.insert d.name (SVNum d.sample) s'.samples }
      in
        walk env (foldl declare st { line = stmt.line } ps) acc tail
    Cases form branches -> cases env st { line = stmt.line } acc stmt form branches tail
    _ -> case runStep env st stmt of
      Tuple (Left (Fail err)) st' -> { node: mkNode st' acc Nothing (Just err), reached: Array.null env.forced }
      Tuple (Left (Restart opts)) st' -> case opts of
        [ o ] -> rerun env o env.path
        [] -> { node: mkNode st' acc Nothing (Just { line: stmt.line, message: "the assumption has no solution" }), reached: Array.null env.forced }
        _ -> { node: mkNode st' acc Nothing (Just { line: stmt.line, message: "the assumption has several solutions (" <> joinWith "; " (map _.text opts) <> "); split them with cases" }), reached: Array.null env.forced }
      Tuple (Right step) st' -> walk env st' (Array.snoc acc step) tail

mapLast :: forall a. (a -> a) -> Array a -> Array a
mapLast f xs = case Array.unsnoc xs of
  Just { init, last } -> Array.snoc init (f last)
  Nothing -> xs

-- | Evaluates the whole script again with a substitution, replaying the
-- | choices of `path`; the result starts after the last of them.
rerun :: Env -> Option -> Array Forced -> Walked
rerun env o path =
  let
    env' = env { subst = o.subst, extra = env.extra <> o.hyps, forced = path, path = [] }
  in
    walk env' (initialSt env') [] env.script.body

leaf :: Env -> St -> Array PStep -> Walked
leaf env st acc = case checkSamples st acc of
  Left err -> { node: mkNode st acc Nothing (Just err), reached: Array.null env.forced }
  Right acc' -> { node: mkNode st acc' Nothing Nothing, reached: Array.null env.forced }

-- | Every configuration drawn in a segment must satisfy its hypotheses. A
-- | segment without samples of its own gets one found automatically.
checkSamples :: St -> Array PStep -> Either PError (Array PStep)
checkSamples st steps =
  let
    ctx = context st
    bad = Array.find (\s -> not (satisfies s.figure)) steps
    satisfies fig =
      let
        vals = valuationOf st (Map.union fig st.samples)
      in
        all (S.condHolds vals) ctx.conds
    n = names st
    failing fig =
      let
        vals = valuationOf st (Map.union fig st.samples)
      in
        map _.text (Array.filter (\h -> not (S.condHolds vals { poly: h.poly, rel: h.rel })) st.hyps)
  in
    case bad of
      Nothing -> Right steps
      Just s ->
        let
          found = Array.head (S.findPoints ctx 1)
          figure v = Map.fromFoldable
            (Array.catMaybes (Array.mapWithIndex (\i x -> (\q -> Tuple x.name (SVNum q)) <$> join (Array.index v i)) st.vars))
        in
          if st.sampled then
            Left
              { line: s.line
              , message: "the sample is not in this case: " <> joinWith "; " (failing s.figure) <> " fails"
                  <> case found of
                    Just v -> " (" <> Sym.valuationText n v <> " works)"
                    Nothing -> ""
              }
          else case found of
            Just v -> Right (map (\x -> x { figure = figure v }) steps)
            Nothing -> Left { line: s.line, message: "no configuration satisfies the hypotheses of this case; give one with sample" }

cases :: Env -> St -> Array PStep -> Located Stmt -> CaseForm -> Array Branch -> Array (Located Stmt) -> Walked
cases env st acc stmt form branches tail = case Array.uncons env.forced of
  Just { head: f, tail: rest } | f.line == stmt.line ->
    -- replaying a choice
    case Array.find (\b -> b.label == f.label) branches of
      Nothing -> { node: mkNode st acc Nothing (Just { line: stmt.line, message: "internal: replayed branch missing" }), reached: false }
      Just br ->
        let
          env' = env { forced = rest, path = Array.snoc env.path f }
        in
          case enter env' st stmt form br of
            -- the solution falls into another case
            Left err -> { node: mkNode st [] Nothing (Just err), reached: false }
            Right (Tuple step st') ->
              if Array.null rest then walk env' st' [ step ] (br.body <> tail)
              else walk env' st' [] (br.body <> tail)
  Just _ -> { node: mkNode st acc Nothing (Just { line: stmt.line, message: "internal: replay out of order" }), reached: false }
  Nothing -> case checkSamples st acc of
    Left err -> { node: mkNode st acc Nothing (Just err), reached: true }
    Right acc' ->
      let
        bs = map (branchResult env st stmt form branches tail) (labelsOf form)
      in
        { node: mkNode st acc' (Just { line: stmt.line, text: stmt.text, branches: bs }) Nothing, reached: true }

-- | Enters a branch: adds its condition (inequalities) and binds its points.
enter :: Env -> St -> Located Stmt -> CaseForm -> Branch -> Either PError (Tuple PStep St)
enter _ st0 stmt form br =
  let
    st1 = (withSamples (leadingSamples br.body) st0 { sampled = false }) { line = br.line, curHyps = [], curVerdicts = [], created = [] }
    act = do
      c <- branchCondition st1.scope form br.label
      case c of
        CondIneq hs -> for_ hs \h -> addHyp BranchCond h.text h.poly h.rel
        CondEq ps text -> do
          st <- get
          let
            residual = Array.filter (not <<< P.isZero) (map (substPoly st.subst) ps)
          unless (Array.null residual) (failAt ("internal: " <> text <> " is not an identity after substitution"))
          for_ ps \_ -> pure unit
        CondImpossible why -> failAt why
      caseObjects st1.scope form br
    Tuple r st2 = runState (runExceptT act) st1
  in
    case r of
      Left (Fail err) -> Left err
      Left (Restart _) -> Left { line: br.line, message: "internal: restart in a branch condition" }
      Right b ->
        let
          objects = Array.mapMaybe (\(Tuple n id) -> (\o -> { name: n, obj: o.obj }) <$> Map.lookup id st2.objs) b.bound
          step =
            { index: st0.step
            , line: stmt.line
            , text: stmt.text <> " → " <> br.text
            , kind: SCase
            , objects
            , hyps: st2.curHyps
            , verdicts: []
            , notes: []
            , snapshot: false
            , figure: figureOf st2
            }
        in
          Right (Tuple step st2 { scope = b.scope, step = st0.step + 1 })

branchResult :: Env -> St -> Located Stmt -> CaseForm -> Array Branch -> Array (Located Stmt) -> Label -> PBranch
branchResult env st stmt form branches tail label =
  let
    written = Array.find (\b -> b.label == label) branches
    cond = runState (runExceptT (branchCondition st.scope form label)) st { line = stmt.line }
    base =
      { label
      , text: maybe' written _.text (labelText label)
      , condition: ""
      , cases: []
      , impossible: Nothing
      , missing: Nothing
      , cover: Nothing
      }
    ineqCover hs refuted = Just { ineq: hs, refuted, eqs: [], options: [] }
    n = names st
    choice i = { line: stmt.line, label, option: i }
  in
    case cond of
      Tuple (Left (Fail err)) _ -> base { missing = Just err.message }
      Tuple (Left (Restart _)) _ -> base { missing = Just "internal: restart" }
      Tuple (Right c) st' -> case c of
        CondImpossible why -> base { impossible = Just why, condition = why }
        CondIneq hs ->
          let
            ctx = context st'
            ctx' = ctx { conds = ctx.conds <> map (\h -> { poly: h.poly, rel: h.rel }) hs }
            conditionText = joinWith "; " (map (\h -> h.text <> ": " <> Sym.polyText n h.poly <> " " <> Sym.relText h.rel) hs)
            refuted = Array.find (\h -> contradicts (S.signOf ctx h.poly) h.rel) hs
          in
            case refuted of
              Just h -> base { impossible = Just (h.text <> " never holds"), condition = conditionText, cover = ineqCover hs true }
              Nothing -> case written of
                Nothing -> case Array.head (S.findPoints ctx' 1) of
                  Just v -> base { condition = conditionText, missing = Just ("the case " <> labelText label <> " is not covered: for example " <> Sym.valuationText n v) }
                  Nothing -> base { condition = conditionText, missing = Just ("the case " <> labelText label <> " is not covered, and it could not be shown impossible") }
                Just br ->
                  let
                    env' = env { path = Array.snoc env.path (choice (-1)) }
                  in
                    case enter env' st' stmt form br of
                      Left err -> base { condition = conditionText, cases = [ { text: "", node: Just (mkNode st' [] Nothing (Just err)), impossible: Nothing } ], cover = ineqCover hs false }
                      Right (Tuple step st'') ->
                        let
                          w = walk env' st'' [ step ] (br.body <> tail)
                        in
                          base { condition = conditionText, cases = [ { text: "", node: Just w.node, impossible: Nothing } ], cover = ineqCover hs false }
        CondEq ps text -> case solve st' ps of
          Left msg -> base { condition = text, missing = Just ("the case " <> labelText label <> " (" <> text <> "): " <> msg) }
          Right [] -> base { condition = text, impossible = Just (text <> " has no solution") }
          Right opts ->
            let
              results = Array.mapWithIndex
                ( \i o ->
                    let
                      w = rerun env o (Array.snoc env.path (choice i))
                    in
                      if w.reached then { text: o.text, node: Just w.node, impossible: Nothing }
                      else { text: o.text, node: Nothing, impossible: Just (errorText w.node) }
                )
                opts
              possible = Array.filter (isNothing <<< _.impossible) results
              cover = Just
                { ineq: []
                , refuted: false
                , eqs: ps
                , options: Array.zipWith (\o r -> { subst: o.subst, hyps: o.hyps, possible: isNothing r.impossible, text: o.text }) opts results
                }
            in
              if Array.null possible then base { condition = text, impossible = Just (joinWith "; " (Array.mapMaybe (\r -> (\why -> r.text <> ": " <> why) <$> r.impossible) results)), cases = results, cover = cover }
              else case written of
                Nothing -> base { condition = text, missing = Just ("the case " <> labelText label <> " is not covered: for example " <> joinWith " or " (map _.text possible)) }
                Just _ -> base { condition = text, cases = results, cover = cover }
  where
  maybe' m f d = case m of
    Just v -> f v
    Nothing -> d
  contradicts a rel = case a, rel of
    APos _, RNeg -> true
    APos _, RNonPos -> true
    ANeg _, RPos -> true
    ANeg _, RNonNeg -> true
    ANonNeg _, RNeg -> true
    ANonPos _, RPos -> true
    AZero, RPos -> true
    AZero, RNeg -> true
    AZero, RNonZero -> true
    _, _ -> false
  errorText (PNode nd) = case nd.error of
    Just err -> err.message
    Nothing -> "impossible"

-------------------------------------------------------------------------------
-- Entry points

prove :: Script -> Proof
prove script =
  let
    env = { script, forced: [], path: [], subst: Map.empty, extra: [] }
    w = walk env (initialSt env) [] script.body
    PNode root = w.node
    parametric = Array.any isParam script.body
  in
    { title: script.title
    , intro: introNotes script.body
    , root: w.node
    , errors: collectErrors w.node
    , parametric: parametric || not (Array.null root.vars)
    }
  where
  isParam s = case s.value of
    Param _ -> true
    Cases _ _ -> true
    Bind _ (EPointOn _ _) -> true
    _ -> false
  introNotes body = Array.mapMaybe
    ( \s -> case s.value of
        Note t -> Just t
        _ -> Nothing
    )
    (Array.takeWhile isNoteOrTitle body)
  isNoteOrTitle s = case s.value of
    Note _ -> true
    _ -> false

proveSource :: String -> Either ParseError Proof
proveSource src = prove <$> parseScript src

-- | Every error in the tree: failed steps and uncovered cases.
collectErrors :: PNode -> Array PError
collectErrors (PNode n) =
  maybe' n.error
    <> case n.split of
      Nothing -> []
      Just sp -> Array.concatMap
        ( \b ->
            maybe' (map (\m -> { line: sp.line, message: m }) b.missing)
              <> Array.concatMap (\c -> maybe [] collectErrors c.node) b.cases
        )
        sp.branches
  where
  maybe' = case _ of
    Just x -> [ x ]
    Nothing -> []
  maybe d f = case _ of
    Just x -> f x
    Nothing -> d

-- | A branch line without its colon: `two T1, T2`.
labelOf :: String -> String
labelOf t = fromMaybe t (String.stripSuffix (String.Pattern ":") t)

-- | The leaves of the tree, with the branch choices leading to them.
leaves :: PNode -> Array { path :: Array String, node :: PNode }
leaves = go []
  where
  go path node@(PNode n) = case n.split of
    Nothing -> [ { path, node } ]
    Just sp -> Array.concatMap
      ( \b -> Array.concatMap
          ( \c -> case c.node of
              Just nd -> go (Array.snoc path (labelOf b.text <> (if c.text == "" then "" else " (" <> c.text <> ")"))) nd
              Nothing -> []
          )
          b.cases
      )
      sp.branches

-- | The proof steps along a path of branch labels (the first possible case
-- | of an equation branch), and the hypotheses in force at its end.
stepsAlong :: PNode -> Array Label -> { steps :: Array PStep, node :: PNode }
stepsAlong node@(PNode n) labels = case n.split, Array.uncons labels of
  Just sp, Just { head: l, tail } ->
    case Array.find (\b -> b.label == l) sp.branches >>= \b -> Array.findMap _.node b.cases of
      Just next ->
        let
          r = stepsAlong next tail
        in
          { steps: n.steps <> r.steps, node: r.node }
      Nothing -> { steps: n.steps, node }
  _, _ -> { steps: n.steps, node }

-- | A hypothesis in words and symbols; `v ≠ c` when it is linear in one
-- | variable.
hypDisplay :: Names -> Hyp -> String
hypDisplay n h = h.text <> ": " <> condDisplay n h.poly h.rel

condDisplay :: Names -> Poly -> Rel -> String
condDisplay n p rel = case P.vars p, rel of
  [ v ], RNonZero | P.degIn p v == 1 ->
    let
      cs = P.coeffsIn p v
      a = fromMaybe zero (Array.index cs 1 >>= P.constValue)
      b = fromMaybe zero (Array.index cs 0 >>= P.constValue)
    in
      n.text v <> " ≠ " <> Sym.ratText n (RF.fromRational (negate b / a))
  _, _ -> Sym.polyText n p <> " " <> Sym.relText rel
