-- | The export of a universal proof for Lean (see spec/universal-format.md).
-- |
-- | Every case of the proof tree (a leaf) becomes one theorem: for all values
-- | of its variables that satisfy its hypotheses (the same as in the PDF),
-- | there are points that satisfy every construction step and every claim.
-- | The proof is by reflection: the points have polynomial coordinates in the
-- | variables and the square roots of the case's tower, and every fact the
-- | statement needs is derived from the hypotheses by a chain of sign
-- | certificates (`Proof.Cert`), which Lean's kernel checks.
-- |
-- | The certificates follow the prover: the signs of base elements come from
-- | the sign engine (`Proof.Signs`), the signs in the tower from the signs of
-- | the parts (a + b√r) and of norms, as `Constructible.ParamField` decides
-- | them.
module Export.Universal
  ( document
  , documentJson
  ) where

import Prelude

import Constructible.ParamField (PC)
import Constructible.ParamField as PF
import Constructible.Poly (Poly)
import Constructible.Poly as P
import Constructible.RatFun as RF
import Constructible.Rational (Rational)
import Constructible.Rational as Q
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, get, modify_, put, runState)
import Data.Argonaut.Core (Json, fromArray, fromNumber, fromObject, fromString, jsonNull, stringifyWithIndent)
import Data.Array ((..))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (all, foldl, for_)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (joinWith)
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Proof.Cert (Chain, ChainM, Deriv(..), FRel(..), Rule(..), byTerms, factOf, termWith, unitTerm)
import Proof.Cert as C
import Proof.Prove (HypOrigin, PNode(..), Proof, SChoice(..), SDef(..), SObj(..), SPoint, SRef(..), VarInfo, substPoly)
import Proof.Signs (Answer(..), Context, Rel(..), certNonZero, certSign, knownFact)
import Proof.Signs as S
import Proof.Syntax (Cmp(..), PredF(..), labelText)
import Render.Sym as Sym

document :: Proof -> Either String String
document p = stringifyWithIndent 1 <$> documentJson p

documentJson :: Proof -> Either String Json
documentJson proof = do
  ls <- traverse (\l -> leafJson l.name l.label l.node) (leavesOf proof.root)
  cs <- traverse (\l -> coverJson l.name l.label l.node) (splitsOf proof.root)
  pure
    ( obj
        [ Tuple "format" (fromString "straightedge-compass-universal")
        , Tuple "version" (int 1)
        , Tuple "title" (maybe' fromString proof.title)
        , Tuple "leaves" (fromArray ls)
        , Tuple "covers" (fromArray cs)
        ]
    )
  where
  maybe' f = case _ of
    Just x -> f x
    Nothing -> jsonNull

-- | The leaves with a Lean name (`case_two_none`, `case_one_2`) and a label.
leavesOf :: PNode -> Array { name :: String, label :: String, node :: PNode }
leavesOf = go [] []
  where
  go names labels node@(PNode n) = case n.split of
    Nothing ->
      [ { name: if Array.null names then "" else "case_" <> joinWith "_" names
        , label: joinWith " › " labels
        , node
        }
      ]
    Just sp -> Array.concatMap
      ( \b ->
          let
            many = Array.length b.cases > 1
          in
            Array.concat
              ( Array.mapWithIndex
                  ( \i c -> case c.node of
                      Just nd -> go
                        (Array.snoc names (labelText b.label <> (if many then "_" <> solutionName i c.text else "")))
                        (Array.snoc labels (labelText b.label <> (if c.text == "" then "" else " (" <> c.text <> ")")))
                        nd
                      Nothing -> []
                  )
                  b.cases
              )
      )
      sp.branches

-- | A Lean name part for a solution: `d = −2` → `d_eq_neg2`.
solutionName :: Int -> String -> String
solutionName i text =
  let
    r1 = String.replaceAll (String.Pattern " = ") (String.Replacement "_eq_") text
    r2 = String.replaceAll (String.Pattern ", ") (String.Replacement "_") r1
    r3 = String.replaceAll (String.Pattern "−") (String.Replacement "neg") r2
    r4 = String.replaceAll (String.Pattern "-") (String.Replacement "neg") r3
    r5 = String.replaceAll (String.Pattern "/") (String.Replacement "_div_") r4
    ok c = String.contains (String.Pattern (String.singleton c)) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"
    clean = String.fromCodePointArray (Array.filter ok (String.toCodePointArray r5))
  in
    if clean == "" || String.length clean > 32 || String.length clean < String.length r5 then show (i + 1) else clean

-------------------------------------------------------------------------------
-- JSON helpers

obj :: Array (Tuple String Json) -> Json
obj = fromObject <<< Object.fromFoldable

int :: Int -> Json
int = fromNumber <<< Int.toNumber

str :: String -> Json
str = fromString

strs :: Array String -> Json
strs = fromArray <<< map fromString

ints :: Array Int -> Json
ints = fromArray <<< map int

-------------------------------------------------------------------------------
-- The export monad: the chain, the goals, and the conjuncts so far

type Goal = { fn :: String, args :: Array String, neg :: Boolean, rel :: FRel }

type ESt =
  { chain :: Chain
  , goals :: Array { goal :: Goal, index :: Int }
  , ctx :: Context
  }

type EM = ExceptT String (State ESt)

liftC :: forall a. ChainM a -> EM a
liftC m = do
  st <- get
  let
    Tuple r ch = C.runChain st.chain m
  put st { chain = ch }
  case r of
    Left e -> throwError e
    Right a -> pure a

liftE :: forall a. Either String a -> EM a
liftE = case _ of
  Left e -> throwError e
  Right a -> pure a

note :: forall a. String -> Maybe a -> EM a
note msg = case _ of
  Just a -> pure a
  Nothing -> throwError msg

-------------------------------------------------------------------------------
-- Polynomial forms

-- | The fixed data of a leaf: the variables and the tower.
type Env =
  { nparams :: Int
  , ren :: Array Int -- prover variable → Lean variable (−1: substituted)
  , oracle :: PF.Oracle
  , sFacts :: Array Deriv -- sᵢ ≥ 0 or > 0
  }

ren :: Env -> Poly -> Poly
ren env p = P.rename p env.ren

sVar :: Env -> Int -> Poly
sVar env i = P.var (env.nparams + i)

-- | x = X / D with X a polynomial in the variables and the square roots,
-- | D in the variables.
type Flat = { x :: Poly, d :: Poly }

flatten :: Env -> PC -> Flat
flatten env v = case PF.deconstruct v of
  PF.PBase f -> { x: ren env (RF.num f), d: ren env (RF.den f) }
  PF.PQuad a b _ ->
    let
      parts = quadParts env a b
    in
      { x: parts.fa.x * parts.ca + parts.fb.x * parts.cb * sVar env (PF.depth a), d: parts.d }

-- | The parts of a + b√r: D = lcm of the denominators, and the cofactors.
quadParts :: Env -> PC -> PC -> { fa :: Flat, fb :: Flat, d :: Poly, ca :: Poly, cb :: Poly }
quadParts env a b =
  let
    fa = flatten env a
    fb = flatten env b
    g = P.gcd fa.d fb.d
    d = fa.d * fromMaybe fb.d (P.divExact fb.d g)
  in
    { fa, fb, d, ca: fromMaybe one (P.divExact d fa.d), cb: fromMaybe one (P.divExact d fb.d) }

-- | P = X · D: the polynomial whose sign is the sign of x.
signPoly :: Env -> PC -> Poly
signPoly env v = let f = flatten env v in f.x * f.d

-- | p = c · q with c a constant.
ratioOf :: Poly -> Poly -> Maybe Rational
ratioOf p q
  | P.isZero p || P.isZero q = Nothing
  | otherwise =
      let
        np = P.normalise p
        nq = P.normalise q
      in
        if np.p == nq.p then Just (np.c / nq.c) else Nothing

-------------------------------------------------------------------------------
-- Signs in the tower (as `ParamField.sgnK` decides them)

-- | A derived fact about σ · P (σ = ±1).
type TSign = { sigma :: Rational, rel :: FRel, deriv :: Deriv }

fromAnswer :: S.Proved -> Maybe TSign
fromAnswer p = case p.answer of
  APos _ -> Just { sigma: one, rel: FPos, deriv: p.deriv }
  ANeg _ -> Just { sigma: negate one, rel: FPos, deriv: p.deriv }
  ANonNeg _ -> Just { sigma: one, rel: FNonNeg, deriv: p.deriv }
  ANonPos _ -> Just { sigma: negate one, rel: FNonNeg, deriv: p.deriv }
  _ -> Nothing

-- | c², for a term: a constant, the square of a nonzero fact, or c² itself
-- | (then the term is not strict).
type SqPart = { sqrefs :: Array Deriv, sq :: Poly, strict :: Boolean }

sqPart :: Context -> Poly -> SqPart
sqPart ctx c = case P.constValue c of
  Just k | not (Q.isZero k) -> { sqrefs: [], sq: c, strict: true }
  _ -> case certNonZero ctx c of
    Just d -> { sqrefs: [ d ], sq: one, strict: true }
    Nothing -> { sqrefs: [], sq: c, strict: false }

towerSign :: Env -> Context -> PC -> Maybe TSign
towerSign env ctx v = case PF.deconstruct v of
  PF.PBase f -> baseSign (ren env (RF.num f)) (ren env (RF.den f))
  PF.PQuad a b r -> quad a b r
  where
  baseSign n d
    | d == one = certSign ctx n >>= fromAnswer
    | otherwise =
        let
          sn = certSign ctx n >>= fromAnswer
          sd = certSign ctx d >>= fromAnswer >>= strong d
        in
          case sn, sd of
            Just x, Just y ->
              let
                rel = if x.rel == FPos && y.rel == FPos then FPos else FNonNeg
                sigma = x.sigma * y.sigma
              in
                Just { sigma, rel, deriv: byTerms { p: P.scale sigma (n * d), rel } [ termWith one [ x.deriv, y.deriv ] [] one ] }
            _, _ -> certSign ctx (n * d) >>= fromAnswer
  -- a denominator is nonzero: ≥ 0 gives > 0
  strong d t
    | t.rel == FPos = Just t
    | otherwise = certNonZero ctx d <#> \z ->
        t { rel = FPos, deriv = Derived { p: P.scale t.sigma d, rel: FPos } (ByStrict t.deriv z) }
  quad a b r = do
    let
      i = PF.depth a
      s = sVar env i
      parts = quadParts env a b
      px = signPoly env v
    sF <- Array.index env.sFacts i
    let
      sPos = (factOf sF).rel == FPos
      qa = sqPart ctx parts.ca
      qb = sqPart ctx parts.cb
    if PF.isZero a then do
      tb <- towerSign env ctx b
      let
        rel = if tb.rel == FPos && sPos && qb.strict then FPos else FNonNeg
      pure { sigma: tb.sigma, rel, deriv: byTerms { p: P.scale tb.sigma px, rel } [ termWith one [ tb.deriv, sF ] qb.sqrefs qb.sq ] }
    else do
      ta <- towerSign env ctx a
      tb <- towerSign env ctx b
      let
        termA = termWith one [ ta.deriv ] qa.sqrefs qa.sq
        termB = termWith one [ tb.deriv, sF ] qb.sqrefs qb.sq
        strictAB = (ta.rel == FPos && qa.strict) || (tb.rel == FPos && sPos && qb.strict)
        relAB = if strictAB then FPos else FNonNeg
      if ta.sigma == tb.sigma then
        pure { sigma: ta.sigma, rel: relAB, deriv: byTerms { p: P.scale ta.sigma px, rel: relAB } [ termA, termB ] }
      else do
        -- the conjugate a − b√r has the sign of a; x · conj = a² − r b²
        let
          pa = parts.fa.x * parts.fa.d
          pb = parts.fb.x * parts.fb.d
          pq = pa * parts.ca * parts.ca - pb * parts.cb * parts.cb * s
          qDeriv = byTerms { p: P.scale ta.sigma pq, rel: relAB } [ termA, termB ]
        n <- hush (PF.runPM env.oracle (PF.run (PF.ex a * PF.ex a - PF.ex r * PF.ex b * PF.ex b)))
        tn <- towerSign env ctx n
        let
          fn = flatten env n
          qd = sqPart ctx parts.d
          qdn = sqPart ctx fn.d
          sigma = ta.sigma * tn.sigma
        rel <-
          if tn.rel == FPos && qd.strict then Just FPos
          else if relAB == FPos && qdn.strict then Just FNonNeg
          else Nothing
        pure
          { sigma
          , rel
          , deriv: Derived { p: P.scale sigma px, rel }
              ( ByLin
                  { mult: termWith one [ qDeriv ] qdn.sqrefs qdn.sq
                  , terms: [ termWith one [ tn.deriv ] (qd.sqrefs <> qd.sqrefs) (qd.sq * qd.sq) ]
                  }
              )
          }

-- | A derived fact that P ≠ 0 (or ±P > 0), as `Prove.nonZeroPoly` decides.
towerNonZero :: Env -> Context -> PC -> Maybe Deriv
towerNonZero env ctx v = case towerSign env ctx v of
  Just t | t.rel == FPos -> Just t.deriv
  _ -> case PF.deconstruct v of
    PF.PBase f ->
      let
        n = ren env (RF.num f)
        d = ren env (RF.den f)
      in
        if d == one then certNonZero ctx n
        else do
          zn <- certNonZero ctx n
          zd <- certNonZero ctx d
          k <- ratioOf (n * d) ((factOf zn).p * (factOf zd).p)
          pure (Derived { p: n * d, rel: FNonZero } (ByNonZero { m: one, c: k, refs: [ zn, zd ], eqs: [] }))
    PF.PQuad a b r -> do
      let
        i = PF.depth a
        s = sVar env i
        parts = quadParts env a b
        px = signPoly env v
      sF <- Array.index env.sFacts i
      if PF.isZero a then do
        when ((factOf sF).rel /= FPos) Nothing
        zb <- towerNonZero env ctx b
        let
          zc = nzRefs parts.cb
          pb = parts.fb.x * parts.fb.d
          refs = [ zb, sF ] <> zc <> zc
        k <- ratioOf (pb * parts.cb * parts.cb * s) (prodOf refs)
        pure (Derived { p: px, rel: FNonZero } (ByNonZero { m: one, c: k, refs, eqs: [] }))
      else do
        n <- hush (PF.runPM env.oracle (PF.run (PF.ex a * PF.ex a - PF.ex r * PF.ex b * PF.ex b)))
        zn <- towerNonZero env ctx n
        let
          fn = flatten env n
          pa = parts.fa.x * parts.fa.d
          pb = parts.fb.x * parts.fb.d
          pq = pa * parts.ca * parts.ca - pb * parts.cb * parts.cb * s
          pn = fn.x * fn.d
          zd = nzRefs parts.d
          refs = [ zn ] <> zd <> zd <> zd <> zd
          d4 = P.pow parts.d 4
        k <- ratioOf (pn * d4) (prodOf refs)
        pure (Derived { p: px, rel: FNonZero } (ByLinNZ { m: pq * fn.d * fn.d, c: k, refs }))
  where
  -- c ≠ 0 as facts: none for a constant
  nzRefs c = case P.constValue c of
    Just _ -> []
    Nothing -> case certNonZero ctx c of
      Just d -> [ d ]
      Nothing -> [ Known { p: c, rel: FNonZero } ] -- fails in the chain
  prodOf = foldl (\acc d -> acc * (factOf d).p) one

hush :: forall a b. Either a b -> Maybe b
hush = case _ of
  Right b -> Just b
  Left _ -> Nothing

-------------------------------------------------------------------------------
-- Points with polynomial coordinates (mirroring `StraightedgeCompass.UPt`)

type UPt = { x :: Poly, y :: Poly, d :: Poly }

uptOf :: Env -> SPoint -> UPt
uptOf env p =
  let
    fx = flatten env p.x
    fy = flatten env p.y
    g = P.gcd fx.d fy.d
    d = fx.d * fromMaybe fy.d (P.divExact fy.d g)
    u = { x: fx.x * fromMaybe one (P.divExact d fx.d), y: fy.x * fromMaybe one (P.divExact d fy.d), d }
  in
    integral u

-- | Scaled to integer coefficients (the point is the same).
integral :: UPt -> UPt
integral u =
  let
    dens = Array.concatMap (\p -> map (Q.denominator <<< _.coeff) (P.terms p)) [ u.x, u.y, u.d ]
    l = foldl Q.lcmInt one dens
  in
    { x: P.scale l u.x, y: P.scale l u.y, d: P.scale l u.d }

dxN :: UPt -> UPt -> Poly
dxN a b = b.x * a.d - a.x * b.d

dyN :: UPt -> UPt -> Poly
dyN a b = b.y * a.d - a.y * b.d

dist2N :: UPt -> UPt -> Poly
dist2N a b = dxN a b * dxN a b + dyN a b * dyN a b

crossN :: UPt -> UPt -> UPt -> Poly
crossN a b x = dxN a b * dyN a x - dyN a b * dxN a x

dotN :: UPt -> UPt -> UPt -> Poly
dotN a b c = dxN a b * dxN a c + dyN a b * dyN a c

circN :: UPt -> UPt -> UPt -> Poly
circN o a x = dist2N o x * (a.d * a.d) - dist2N o a * (x.d * x.d)

cmpN :: UPt -> UPt -> UPt -> UPt -> Poly
cmpN a b c d = dist2N a b * ((c.d * d.d) * (c.d * d.d)) - dist2N c d * ((a.d * b.d) * (a.d * b.d))

leftN :: UPt -> UPt -> UPt -> Poly
leftN a b x = crossN a b x * (b.d * x.d)

lexXN :: UPt -> UPt -> Poly
lexXN x y = dxN x y * (x.d * y.d)

lexYN :: UPt -> UPt -> Poly
lexYN x y = dyN x y * (x.d * y.d)

crossDirN :: UPt -> UPt -> UPt -> UPt -> Poly
crossDirN a b c d = dxN a b * dyN c d - dyN a b * dxN c d

dotDirN :: UPt -> UPt -> UPt -> UPt -> Poly
dotDirN a b c d = dxN a b * dxN c d + dyN a b * dyN c d

dotSignN :: UPt -> UPt -> UPt -> Poly
dotSignN b a c = dotN b a c * (a.d * c.d)

angleN :: UPt -> UPt -> UPt -> UPt -> UPt -> UPt -> Poly
angleN a b c d e f =
  (dotN b a c * dotN b a c) * (dist2N e d * dist2N e f) - (dotN e d f * dotN e d f) * (dist2N b a * dist2N b c)

alongU :: UPt -> UPt -> Poly -> UPt
alongU a b t = { x: a.x * b.d + t * dxN a b, y: a.y * b.d + t * dyN a b, d: a.d * b.d }

rotU :: UPt -> UPt -> Poly -> UPt
rotU o b t =
  let
    tt = t * t
    two = P.fromInt 2
    u = dxN o b
    w = dyN o b
  in
    { x: o.x * b.d * (one + tt) + ((one - tt) * u - two * t * w)
    , y: o.y * b.d * (one + tt) + (two * t * u + (one - tt) * w)
    , d: o.d * b.d * (one + tt)
    }

-------------------------------------------------------------------------------
-- A leaf

-- | A point of the statement: its Lean data and its value in the tower.
type Pt = { name :: String, upt :: UPt, value :: SPoint, def :: Json }

leafJson :: String -> String -> PNode -> Either String Json
leafJson leanName label (PNode n) = do
  let
    -- Lean variables: the prover's variables that are not substituted
    keep = Array.filter (\i -> not (Map.member i n.substMap)) (0 .. (Array.length n.vars - 1))
    nparams = Array.length keep
    renArr = map (\i -> fromMaybe (-1) (Array.elemIndex i keep)) (0 .. (Array.length n.vars - 1))
    paramNames = Array.mapMaybe (\i -> leanVar <$> Array.index n.vars i) keep
    proverCtx = { conds: map (\h -> { poly: h.poly, rel: h.rel }) n.hyps, samples: [] }
    oracle = S.oracleFor proverCtx
    renamedConds = map (\h -> { poly: P.rename h.poly renArr, rel: h.rel }) n.hyps
    env0 = { nparams, ren: renArr, oracle, sFacts: [] }
  -- every polynomial must avoid the substituted variables
  let
    pointsOf o = case o.obj of
      SPt p -> [ p ]
      _ -> []
    others o = case o.def of
      DefMeet m -> case m.other of
        Just q -> [ q ]
        Nothing -> []
      _ -> []
    literalsOf o = case o.def of
      DefMeet { choice: SCExcept (SRefLit l) } -> [ lit l ]
      DefMeet { choice: SCNearest (SRefLit l) } -> [ lit l ]
      DefMeet { choice: SCFarthest (SRefLit l) } -> [ lit l ]
      _ -> []
    lit l = { x: PF.fromRational l.x, y: PF.fromRational l.y }
    allPts = Array.concatMap (\o -> pointsOf o <> others o <> literalsOf o) n.objects
    vals = Array.concatMap (\p -> [ p.x, p.y ]) allPts
    used = Array.nub (Array.concatMap (Array.concatMap RF.vars <<< PF.baseElements) vals)
  for_ used \v -> unless (Array.index renArr v /= Just (-1) && v < Array.length renArr) do
    Left ("internal: a substituted variable remains in the case " <> label)
  joined <- PF.runPM oracle (PF.joinAll vals)
  let
    tower = case Array.head joined of
      Just v -> PF.radicands v
      Nothing -> []
    -- the joined values, in the order of `allPts`
    lifted = pairs joined
    st0 = { chain: C.emptyChain nparams, goals: [], ctx: { conds: renamedConds, samples: [] } }
    Tuple r _ = runState (runExceptT (body env0 tower lifted paramNames)) st0
  r
  where
  pairs xs = case Array.uncons xs of
    Just { head: x, tail } -> case Array.uncons tail of
      Just { head: y, tail: rest } -> Array.cons { x, y } (pairs rest)
      Nothing -> []
    Nothing -> []
  body env0 tower lifted paramNames = do
    -- 1. hypotheses: the theorem's, then the dropped ones derived
    let
      renH h = { poly: P.rename h.poly env0.ren, rel: h.rel }
      isKept h = Array.any (\t -> t.poly == h.poly && t.rel == h.rel) n.theorem
      indexed = Array.mapWithIndex Tuple n.hyps
    liftC (C.addHyps (map (\h -> factOf (knownFact (renH h)).deriv) n.theorem))
    for_ (Array.reverse indexed) \(Tuple i h) -> unless (isKept h) do
      let
        ctx =
          { conds: map (renH <<< (\(Tuple _ x) -> x))
              (Array.filter (\(Tuple j x) -> j > i || (j < i && isKept x)) indexed)
          , samples: []
          }
        c = renH h
      d <- note ("the hypothesis '" <> h.text <> "' (dropped in the PDF) could not be derived") (deriveHyp ctx c)
      liftC (void (C.addDeriv d))
    -- 2. the square roots
    env <- foldlM' (sqrtLevel) env0 (Array.mapWithIndex Tuple tower)
    -- 3. the points
    let
      ptsList = Array.mapMaybe (\o -> case o.obj of
          SPt _ -> Just o
          _ -> Nothing) n.objects
    pts <- buildPoints env lifted
    -- 4. the conjuncts
    conjuncts <- buildConjuncts env pts
    st <- get
    pure
      ( obj
          [ Tuple "name" (str leanName)
          , Tuple "label" (str label)
          , Tuple "params" (strs paramNames)
          , Tuple "hyps" (fromArray (map (hypJson env0) n.theorem))
          , Tuple "chain" (C.chainJson st.chain)
          , Tuple "upts" (fromArray (map (\p -> obj [ Tuple "name" (str p.name), Tuple "def" p.def ]) pts.all))
          , Tuple "points" (fromArray (map (\o -> obj [ Tuple "id" (int o.id), Tuple "binder" (str (binderName o)), Tuple "upt" (str ("u" <> show o.id)) ]) ptsList))
          , Tuple "literals" (fromArray pts.literals)
          , Tuple "goals" (fromArray (map goalJson st.goals))
          , Tuple "conjuncts" (fromArray conjuncts)
          ]
      )
  -- binder names: script names for top-level points, `name#id` for others
  binderName o = if Array.elem o.id n.named then o.name else o.name <> "#" <> show o.id
  -- a derivation of a hypothesis from the others
  deriveHyp ctx c = case c.rel of
    RNonZero -> certNonZero ctx c.poly
    _ -> certSign ctx c.poly >>= \p -> case c.rel, p.answer of
      RPos, APos _ -> Just p.deriv
      RNeg, ANeg _ -> Just p.deriv
      RNonNeg, APos _ -> Just p.deriv
      RNonNeg, ANonNeg _ -> Just p.deriv
      RNonPos, ANeg _ -> Just p.deriv
      RNonPos, ANonPos _ -> Just p.deriv
      _, _ -> Nothing
  -- a square root level: s = √(n / d) with d > 0
  sqrtLevel env (Tuple i r) = do
    st <- get
    let
      ctx = st.ctx
      fr = flatten env r
      dAns = certSign ctx fr.d >>= fromAnswer
    { n: n0, d: d0, rad, den } <- case dAns of
      Just t | t.rel == FPos -> do
        tw <- note ("the radicand of √" <> show (i + 1) <> " is not known to be ≥ 0") (towerSign env ctx r)
        when (tw.sigma /= one) (throwError "internal: a negative radicand")
        let
          nn = P.scale t.sigma fr.x
          dd = P.scale t.sigma fr.d
        pure { n: nn, d: dd, rad: tw, den: t.deriv }
      _ -> do
        tw <- note ("the radicand of √" <> show (i + 1) <> " is not known to be ≥ 0") (towerSign env ctx r)
        when (tw.sigma /= one) (throwError "internal: a negative radicand")
        zd <- note "the denominator of a radicand is not known to be nonzero" (certNonZero ctx fr.d)
        let
          dd = fr.d * fr.d
          nn = fr.x * fr.d
          radD = byTerms { p: nn * dd, rel: tw.rel } [ termWith one [ tw.deriv ] [ zd ] one ]
          denD = byTerms { p: dd, rel: FPos } [ termWith one [] [ zd ] one ]
        pure { n: nn, d: dd, rad: { sigma: one, rel: tw.rel, deriv: radD }, den: denD }
    -- integer coefficients, the same quotient
    let
      dens = Array.concatMap (\p -> map (Q.denominator <<< _.coeff) (P.terms p)) [ n0, d0 ]
      l = foldl Q.lcmInt one dens
      nI = P.scale l n0
      dI = P.scale l d0
      v = env.nparams + i
      radI = byTerms { p: nI * dI, rel: rad.rel } [ termWith (l * l) [ rad.deriv ] [] one ]
      denI = byTerms { p: dI, rel: FPos } [ termWith l [ den ] [] one ]
    liftC (C.addSqrt { v, n: nI, d: dI } radI denI)
    pure env { sFacts = Array.snoc env.sFacts (Known { p: P.var v, rel: rad.rel }) }
  foldlM' f z xs = case Array.uncons xs of
    Nothing -> pure z
    Just { head, tail } -> f z head >>= \z' -> foldlM' f z' tail

  -- the points of the statement and the auxiliary ones, with their Lean data
  buildPoints env lifted = do
    let
      -- the lifted values in the order they were collected
      step acc o = case o.obj of
        SPt _ ->
          let
            mine = Array.drop acc.next lifted
            withOther = case o.def of
              DefMeet { other: Just _ } -> 1
              _ -> 0
            withLit = case o.def of
              DefMeet { choice: SCExcept (SRefLit _) } -> 1
              DefMeet { choice: SCNearest (SRefLit _) } -> 1
              DefMeet { choice: SCFarthest (SRefLit _) } -> 1
              _ -> 0
            vals = Array.take (1 + withOther + withLit) mine
          in
            { next: acc.next + 1 + withOther + withLit, out: Array.snoc acc.out { o, vals } }
        _ -> acc
      collected = (foldl step { next: 0, out: [] } n.objects).out
    -- object points first (frames of free points come before them)
    let
      addPoint m { o, vals } = case Array.head vals of
        Nothing -> m
        Just v ->
          let
            u = case o.def of
              DefPointOn { obj: oi, var } -> case Map.lookup oi frames of
                Just (Tuple "line" (Tuple a b)) -> lookupFrame m a b \ua ub -> alongU ua ub (P.var (fromMaybe 0 (Array.index env.ren var)))
                Just (Tuple _ (Tuple c q)) -> lookupFrame m c q \uc uq -> rotU uc uq (P.var (fromMaybe 0 (Array.index env.ren var)))
                Nothing -> Nothing
              _ -> Just (uptOf env v)
            def = case o.def of
              DefPointOn { obj: oi, var } -> case Map.lookup oi frames of
                Just (Tuple k (Tuple a b)) -> obj [ Tuple (if k == "line" then "along" else "rot") (strs [ "u" <> show a, "u" <> show b ]), Tuple "t" (int (fromMaybe 0 (Array.index env.ren var))) ]
                Nothing -> jsonNull
              _ -> fromMaybe jsonNull (uptJson <$> u)
          in
            case u of
              Just u' -> Map.insert o.id { name: "u" <> show o.id, upt: u', value: v, def } m
              Nothing -> m
      frames = Map.fromFoldable
        ( Array.mapMaybe
            ( \o -> case o.def of
                DefLine a b -> Just (Tuple o.id (Tuple "line" (Tuple a b)))
                DefCircle c q -> Just (Tuple o.id (Tuple "circle" (Tuple c q)))
                _ -> Nothing
            )
            n.objects
        )
      lookupFrame m a b f = do
        pa <- Map.lookup a m
        pb <- Map.lookup b m
        pure (f pa.upt pb.upt)
      byId = foldl addPoint Map.empty collected
      extra = Array.concatMap
        ( \{ o, vals } ->
            let
              rest = Array.drop 1 vals
              otherPt = case o.def, Array.head rest of
                DefMeet { other: Just _ }, Just v -> [ { name: "o" <> show o.id, upt: uptOf env v, value: v } ]
                _, _ -> []
              litPt = case o.def, Array.last rest of
                DefMeet { choice: SCExcept (SRefLit l) }, Just v | Array.length rest >= 1 -> [ litOf o.id l v ]
                DefMeet { choice: SCNearest (SRefLit l) }, Just v -> [ litOf o.id l v ]
                DefMeet { choice: SCFarthest (SRefLit l) }, Just v -> [ litOf o.id l v ]
                _, _ -> []
            in
              otherPt <> map _.pt litPt
        )
        collected
      litOf id l v = { pt: { name: "r" <> show id, upt: integral { x: P.constant l.x, y: P.constant l.y, d: one }, value: v }, l }
      literals = Array.concatMap
        ( \{ o, vals } -> case o.def, Array.last vals of
            DefMeet { choice: SCExcept (SRefLit l) }, Just v -> [ litOf o.id l v ]
            DefMeet { choice: SCNearest (SRefLit l) }, Just v -> [ litOf o.id l v ]
            DefMeet { choice: SCFarthest (SRefLit l) }, Just v -> [ litOf o.id l v ]
            _, _ -> []
        )
        collected
      named = Array.fromFoldable (Map.values byId)
      allPts = named <> map (\p -> { name: p.name, upt: p.upt, value: p.value, def: uptJson p.upt }) extra
    when (Array.length named /= Array.length (Array.filter (\x -> isJust (Array.head x.vals)) collected)) do
      throwError "internal: a point without coordinates"
    -- the denominators are nonzero
    for_ allPts \p -> do
      d <- denominator p.upt
      void (addGoal { fn: "d", args: [ p.name ], neg: false, rel: FNonZero } p.upt.d d)
    let
      byName = Map.fromFoldable (map (\p -> Tuple p.name p) allPts)
    lits <- traverse
      ( \x -> do
          g <- goalIndex { fn: "d", args: [ x.pt.name ], neg: false, rel: FNonZero }
          pure
            ( obj
                [ Tuple "upt" (str x.pt.name)
                , Tuple "stmt" (str ("!₂[" <> ratReal x.l.x <> ", " <> ratReal x.l.y <> "]"))
                , Tuple "x" (constCoord x.pt.upt.d x.l.x)
                , Tuple "y" (constCoord x.pt.upt.d x.l.y)
                , Tuple "poly" (uptJson x.pt.upt)
                , Tuple "dgoal" (int g)
                ]
            )
      )
      literals
    pure { all: allPts, byName, byId, literals: lits }

  -- u.d ≠ 0 (u.d is in the variables only)
  denominator u = do
    st <- get
    d <- note ("a denominator could not be shown nonzero: " <> P.key u.d) (certNonZero st.ctx u.d)
    -- known from now on
    modify_ \s -> s { ctx = s.ctx { conds = Array.snoc s.ctx.conds { poly: u.d, rel: RNonZero } } }
    pure d

  hypJson env h = condJson (paramVar env) env.ren h

  goalJson g = obj
    [ Tuple "fn" (str g.goal.fn)
    , Tuple "args" (strs g.goal.args)
    , Tuple "neg" (fromBool g.goal.neg)
    , Tuple "rel" (str (C.relName g.goal.rel))
    , Tuple "index" (int g.index)
    ]
  fromBool b = fromString (if b then "yes" else "no")

  -----------------------------------------------------------------------------
  -- Conjuncts

  buildConjuncts env pts = do
    objConj <- traverse (objectConjuncts env pts) n.objects
    claimConj <- traverse (claimConjuncts env pts) (Array.mapWithIndex Tuple n.claims)
    pure (Array.concat objConj <> Array.concat claimConj)

  objectConjuncts env pts o = case o.def of
    DefLine a b -> pure <$> neConj env pts ("u" <> show a) ("u" <> show b)
    DefCircle a b -> pure <$> neConj env pts ("u" <> show a) ("u" <> show b)
    DefGiven -> do
      p <- note "internal: a given point" (Map.lookup o.id pts.byId)
      g <- goalIndex { fn: "d", args: [ p.name ], neg: false, rel: FNonZero }
      let
        var = paramVar env
      pure
        [ obj
            [ Tuple "kind" (str "given")
            , Tuple "p" (str p.name)
            , Tuple "x" (coordJson env var p.upt.d p.value.x)
            , Tuple "y" (coordJson env var p.upt.d p.value.y)
            , Tuple "poly" (uptJson p.upt)
            , Tuple "dgoal" (int g)
            ]
        ]
    DefPointOn { obj: oi, var } -> do
      p <- note "internal: a free point" (Map.lookup o.id pts.byId)
      frame <- note "internal: the frame of a free point" (Array.find (\x -> x.id == oi) n.objects)
      let
        t = fromMaybe 0 (Array.index env.ren var)
      case frame.def of
        DefLine a b -> pure [ obj [ Tuple "kind" (str "along"), Tuple "p" (str p.name), Tuple "a" (str ("u" <> show a)), Tuple "b" (str ("u" <> show b)), Tuple "t" (int t) ] ]
        DefCircle c q -> pure [ obj [ Tuple "kind" (str "rot"), Tuple "p" (str p.name), Tuple "a" (str ("u" <> show c)), Tuple "b" (str ("u" <> show q)), Tuple "t" (int t) ] ]
        _ -> throwError "internal: a free point on a point"
    DefMeet m -> do
      let
        x = "u" <> show o.id
      ca <- curveOf m.a
      cb <- curveOf m.b
      memA <- memConj env pts x ca
      memB <- memConj env pts x cb
      distinct <- case ca, cb of
        CLine a b, CLine c d -> pure <$> lineNeConj env pts a b c d
        CCircle a _, CCircle c _ -> pure <$> neConj env pts a c
        _, _ -> pure []
      choice <- choiceConj env pts o.id x ca cb m
      pure ([ memA, memB ] <> distinct <> choice)

  curveOf i = do
    o <- note "internal: an unknown object" (Array.find (\x -> x.id == i) n.objects)
    case o.def of
      DefLine a b -> pure (CLine ("u" <> show a) ("u" <> show b))
      DefCircle c q -> pure (CCircle ("u" <> show c) ("u" <> show q))
      _ -> throwError "internal: a meet of a point"

  -- A ≠ B
  neConj env pts a b = do
    g <- posDist env pts a b
    pure (obj [ Tuple "kind" (str "ne"), Tuple "a" (str a), Tuple "b" (str b), Tuple "g" (int g) ])

  -- 0 < dist2N a b
  posDist env pts a b = do
    ua <- getPt pts a
    ub <- getPt pts b
    v <- pm env (dist2 ua.value ub.value)
    signGoal env { fn: "dist2N", args: [ a, b ], neg: false, rel: FPos } (dist2N ua.upt ub.upt)
      { v, fq: ua.upt.d * ub.upt.d, sos: Just [ dxN ua.upt ub.upt, dyN ua.upt ub.upt ] }

  memConj env pts x c = case c of
    CLine a b -> do
      nd <- posDist env pts a b
      ux <- getPt pts x
      ua <- getPt pts a
      ub <- getPt pts b
      g <- zeroGoal { fn: "crossN", args: [ a, b, x ], neg: false, rel: FZero } (crossN ua.upt ub.upt ux.upt)
      pure (obj [ Tuple "kind" (str "mem"), Tuple "p" (str x), Tuple "curve" (curveJson c), Tuple "g" (ints [ nd, g ]) ])
    CCircle o a -> do
      ux <- getPt pts x
      uo <- getPt pts o
      ua <- getPt pts a
      g <- zeroGoal { fn: "circN", args: [ o, a, x ], neg: false, rel: FZero } (circN uo.upt ua.upt ux.upt)
      pure (obj [ Tuple "kind" (str "mem"), Tuple "p" (str x), Tuple "curve" (curveJson c), Tuple "g" (ints [ g ]) ])

  lineNeConj env pts a b c d = do
    ua <- getPt pts a
    ub <- getPt pts b
    uc <- getPt pts c
    ud <- getPt pts d
    v <- pm env (crossDir ua.value ub.value uc.value ud.value)
    g <- nonZeroGoal env { fn: "crossDirN", args: [ a, b, c, d ], neg: false, rel: FNonZero } (crossDirN ua.upt ub.upt uc.upt ud.upt)
      { v, f: ua.upt.d * ub.upt.d * uc.upt.d * ud.upt.d }
    pure (obj [ Tuple "kind" (str "lineNe"), Tuple "l1" (strs [ a, b ]), Tuple "l2" (strs [ c, d ]), Tuple "g" (int g) ])

  -- which of the intersection points
  choiceConj env pts id x ca cb m = case m.choice of
    SCUnique -> pure []
    SCPair -> case Array.find (\o -> o.id == id - 1) n.objects of
      Just { def: DefMeet { choice: SCPair } } -> pure <$> neConj env pts ("u" <> show (id - 1)) x
      _ -> pure []
    SCExcept ref -> do
      r <- refName id ref
      memA <- memConj env pts r ca
      memB <- memConj env pts r cb
      ne <- neConj env pts r x
      pure [ memA, memB, ne ]
    _ -> do
      let
        y = "o" <> show id
      memA <- memConj env pts y ca
      memB <- memConj env pts y cb
      cond <- condOf
      pure [ obj [ Tuple "kind" (str "other"), Tuple "p" (str x), Tuple "y" (str y), Tuple "curves" (fromArray [ curveJson ca, curveJson cb ]), Tuple "mem" (fromArray [ memA, memB ]), Tuple "cond" cond ] ]
    where
    y' = "o" <> show id
    condOf = case m.choice of
      SCLexFirst -> lexCond env pts x y'
      SCLexSecond -> lexCond env pts y' x
      SCNearest ref -> do
        r <- refName id ref
        -- dist X R < dist Y R: cmpN y r x r > 0
        g <- cmpGoal env pts y' r x r FPos false
        pure (obj [ Tuple "kind" (str "nearest"), Tuple "r" (str r), Tuple "g" (ints [ g ]) ])
      SCFarthest ref -> do
        r <- refName id ref
        g <- cmpGoal env pts x r y' r FPos false
        pure (obj [ Tuple "kind" (str "farthest"), Tuple "r" (str r), Tuple "g" (ints [ g ]) ])
      SCLeft a b -> sideCond env pts ("u" <> show a) ("u" <> show b) x y'
      SCRight a b -> sideCond env pts ("u" <> show b) ("u" <> show a) x y'
      _ -> throwError "internal: choice"

  refName id = case _ of
    SRefObj i -> pure ("u" <> show i)
    SRefLit _ -> pure ("r" <> show id)

  -- LexLt X Y
  lexCond env pts x y = do
    ux <- getPt pts x
    uy <- getPt pts y
    vx <- pm env (PF.sub uy.value.x ux.value.x)
    if PF.isZero vx then do
      g0 <- zeroGoal { fn: "dxN", args: [ x, y ], neg: false, rel: FZero } (dxN ux.upt uy.upt)
      vy <- pm env (PF.sub uy.value.y ux.value.y)
      g1 <- signGoal env { fn: "lexYN", args: [ x, y ], neg: false, rel: FPos } (lexYN ux.upt uy.upt) { v: vy, fq: ux.upt.d * uy.upt.d, sos: Nothing }
      pure (obj [ Tuple "kind" (str "lexY"), Tuple "a" (str x), Tuple "b" (str y), Tuple "g" (ints [ g0, g1 ]) ])
    else do
      g <- signGoal env { fn: "lexXN", args: [ x, y ], neg: false, rel: FPos } (lexXN ux.upt uy.upt) { v: vx, fq: ux.upt.d * uy.upt.d, sos: Nothing }
      pure (obj [ Tuple "kind" (str "lexX"), Tuple "a" (str x), Tuple "b" (str y), Tuple "g" (ints [ g ]) ])

  -- IsLeft P Q X ∧ ¬IsLeft P Q Y
  sideCond env pts p q x y = do
    g1 <- leftGoal env pts p q x false FPos
    g2 <- leftGoal env pts p q y true FNonNeg
    pure (obj [ Tuple "kind" (str "left"), Tuple "line" (strs [ p, q ]), Tuple "g" (ints [ g1, g2 ]) ])

  leftGoal env pts a b x neg rel = do
    ua <- getPt pts a
    ub <- getPt pts b
    ux <- getPt pts x
    v0 <- pm env (cross ua.value ub.value ux.value)
    let
      v = if neg then PF.neg v0 else v0
      g = leftN ua.upt ub.upt ux.upt
    signGoal env { fn: "leftN", args: [ a, b, x ], neg, rel } (if neg then negate g else g)
      { v, fq: ua.upt.d * ub.upt.d * ux.upt.d, sos: Nothing }

  -- cmpN a b c d (> 0, ≥ 0, ≠ 0, = 0)
  cmpGoal env pts a b c d rel neg = do
    ua <- getPt pts a
    ub <- getPt pts b
    uc <- getPt pts c
    ud <- getPt pts d
    let
      g0 = cmpN ua.upt ub.upt uc.upt ud.upt
      g = if neg then negate g0 else g0
      fq = ua.upt.d * ub.upt.d * uc.upt.d * ud.upt.d
      goal = { fn: "cmpN", args: [ a, b, c, d ], neg, rel }
    case rel of
      FZero -> zeroGoal goal g
      _ -> do
        v0 <- pm env (dist2 ua.value ub.value >>= \x -> dist2 uc.value ud.value >>= PF.sub x)
        let
          v = if neg then PF.neg v0 else v0
        case rel of
          FNonZero -> nonZeroGoal env goal g { v, f: fq * fq }
          _ -> signGoal env goal g { v, fq, sos: Nothing }

  claimConjuncts env pts (Tuple ci c) = case c.pred of
    On p x -> do
      curve <- curveOf x
      pure <$> memConj env pts (u p) curve
    PointEq a b -> do
      ua <- getPt pts (u a)
      ub <- getPt pts (u b)
      g0 <- zeroGoal { fn: "dxN", args: [ u a, u b ], neg: false, rel: FZero } (dxN ua.upt ub.upt)
      g1 <- zeroGoal { fn: "dyN", args: [ u a, u b ], neg: false, rel: FZero } (dyN ua.upt ub.upt)
      pure [ obj [ Tuple "kind" (str "eq"), Tuple "a" (str (u a)), Tuple "b" (str (u b)), Tuple "g" (ints [ g0, g1 ]) ] ]
    PointNe a b -> pure <$> neConj env pts (u a) (u b)
    DistCmp cmp a b d f -> do
      let
        k = [ u a, u b, u d, u f ]
        conj g = obj [ Tuple "kind" (str "dist"), Tuple "cmp" (str (cmpName cmp)), Tuple "args" (strs k), Tuple "g" (int g) ]
      g <- case cmp of
        CEq -> cmpGoal env pts (u a) (u b) (u d) (u f) FZero false
        CNe -> cmpGoal env pts (u a) (u b) (u d) (u f) FNonZero false
        -- dist A B < dist C D: cmpN c d a b > 0
        CLt -> cmpGoal env pts (u d) (u f) (u a) (u b) FPos false
        CLe -> cmpGoal env pts (u d) (u f) (u a) (u b) FNonNeg false
        CGt -> cmpGoal env pts (u a) (u b) (u d) (u f) FPos false
        CGe -> cmpGoal env pts (u a) (u b) (u d) (u f) FNonNeg false
      pure [ conj g ]
    Collinear ps -> collinearConj env pts (map u ps)
    Concyclic ps -> concyclicConj env pts ci (map u ps)
    Parallel l m -> do
      cl <- curveOf l
      cm <- curveOf m
      case cl, cm of
        CLine a b, CLine c d -> do
          g1 <- posDist env pts a b
          g2 <- posDist env pts c d
          ua <- getPt pts a
          ub <- getPt pts b
          uc <- getPt pts c
          ud <- getPt pts d
          g3 <- zeroGoal { fn: "crossDirN", args: [ a, b, c, d ], neg: false, rel: FZero } (crossDirN ua.upt ub.upt uc.upt ud.upt)
          pure [ obj [ Tuple "kind" (str "parallel"), Tuple "l1" (strs [ a, b ]), Tuple "l2" (strs [ c, d ]), Tuple "g" (ints [ g1, g2, g3 ]) ] ]
        _, _ -> throwError "parallel: expected two lines"
    Perpendicular l m -> do
      cl <- curveOf l
      cm <- curveOf m
      case cl, cm of
        CLine a b, CLine c d -> do
          ua <- getPt pts a
          ub <- getPt pts b
          uc <- getPt pts c
          ud <- getPt pts d
          g <- zeroGoal { fn: "dotDirN", args: [ a, b, c, d ], neg: false, rel: FZero } (dotDirN ua.upt ub.upt uc.upt ud.upt)
          pure [ obj [ Tuple "kind" (str "perp"), Tuple "l1" (strs [ a, b ]), Tuple "l2" (strs [ c, d ]), Tuple "g" (ints [ g ]) ] ]
        _, _ -> throwError "perpendicular: expected two lines"
    AngleEq a b cc d e f -> do
      let
        pa = u a
        pb = u b
        pc = u cc
        pd = u d
        pe = u e
        pf = u f
      gba <- posDist env pts pb pa
      gbc <- posDist env pts pb pc
      ged <- posDist env pts pe pd
      gef <- posDist env pts pe pf
      ua <- getPt pts pa
      ub <- getPt pts pb
      uc <- getPt pts pc
      ud <- getPt pts pd
      ue <- getPt pts pe
      uf <- getPt pts pf
      d1 <- pm env (dot ub.value ua.value uc.value)
      d2 <- pm env (dot ue.value ud.value uf.value)
      sgn <- if PF.isZero d1 && PF.isZero d2 then pure "zero" else do
        s1 <- pm env (PF.sign d1)
        pure if s1 == PF.Pos then "pos" else "neg"
      let
        neg = sgn == "neg"
        dsg x y z vv = do
          let
            gpoly = dotSignN x.upt y.upt z.upt
          case sgn of
            "zero" -> zeroGoal { fn: "dotSignN", args: [ x.name, y.name, z.name ], neg: false, rel: FZero } gpoly
            _ -> signGoal env { fn: "dotSignN", args: [ x.name, y.name, z.name ], neg, rel: FPos } (if neg then negate gpoly else gpoly)
              { v: if neg then PF.neg vv else vv, fq: x.upt.d * y.upt.d * z.upt.d, sos: Nothing }
      s1 <- dsg ub ua uc d1
      s2 <- dsg ue ud uf d2
      gz <- zeroGoal { fn: "angleN", args: [ pa, pb, pc, pd, pe, pf ], neg: false, rel: FZero } (angleN ua.upt ub.upt uc.upt ud.upt ue.upt uf.upt)
      pure [ obj [ Tuple "kind" (str "angle"), Tuple "args" (strs [ pa, pb, pc, pd, pe, pf ]), Tuple "g" (ints [ gba, gbc, ged, gef, s1, s2, gz ]), Tuple "sign" (str sgn) ] ]
    Ccw a b cc -> do
      g <- leftGoal env pts (u a) (u b) (u cc) false FPos
      pure [ obj [ Tuple "kind" (str "ccw"), Tuple "args" (strs [ u a, u b, u cc ]), Tuple "g" (ints [ g ]) ] ]
    Between a b cc -> do
      g1 <- posDist env pts (u a) (u cc)
      ua <- getPt pts (u a)
      ub <- getPt pts (u b)
      uc <- getPt pts (u cc)
      g2 <- zeroGoal { fn: "crossN", args: [ u a, u cc, u b ], neg: false, rel: FZero } (crossN ua.upt uc.upt ub.upt)
      v <- pm env (dot ub.value ua.value uc.value)
      g3 <- signGoal env { fn: "dotSignN", args: [ u b, u a, u cc ], neg: true, rel: FPos } (negate (dotSignN ub.upt ua.upt uc.upt))
        { v: PF.neg v, fq: ub.upt.d * ua.upt.d * uc.upt.d, sos: Nothing }
      pure [ obj [ Tuple "kind" (str "between"), Tuple "args" (strs [ u a, u b, u cc ]), Tuple "g" (ints [ g1, g2, g3 ]) ] ]
    where
    u i = "u" <> show i

  -- Collinear: three points by their cross product; more on the line of a
  -- distinct pair
  collinearConj env pts ps = case ps of
    [ a, b, c ] -> do
      ua <- getPt pts a
      ub <- getPt pts b
      uc <- getPt pts c
      g <- zeroGoal { fn: "crossN", args: [ a, b, c ], neg: false, rel: FZero } (crossN ua.upt ub.upt uc.upt)
      pure [ obj [ Tuple "kind" (str "collinear3"), Tuple "args" (strs ps), Tuple "g" (ints [ g ]) ] ]
    _ -> do
      let
        candidates = Array.concatMap (\i -> map (\j -> Tuple i j) ((i + 1) .. (Array.length ps - 1))) (0 .. (Array.length ps - 2))
      -- the first pair whose distinctness can be derived
      pairE <- firstOk candidates \(Tuple i j) -> do
        a <- note "" (Array.index ps i)
        b <- note "" (Array.index ps j)
        g <- posDist env pts a b
        pure { a, b, g }
      pr <- case pairE of
        Just x -> pure x
        Nothing -> throwError "collinear: no two of the points are distinct on the whole domain"
      ua <- getPt pts pr.a
      ub <- getPt pts pr.b
      gs <- traverse
        ( \p -> do
            up <- getPt pts p
            zeroGoal { fn: "crossN", args: [ pr.a, pr.b, p ], neg: false, rel: FZero } (crossN ua.upt ub.upt up.upt)
        )
        ps
      pure [ obj [ Tuple "kind" (str "collinear"), Tuple "args" (strs ps), Tuple "base" (strs [ pr.a, pr.b ]), Tuple "nd" (int pr.g), Tuple "g" (ints gs) ] ]

  firstOk :: forall a b. Array a -> (a -> EM b) -> EM (Maybe b)
  firstOk xs f = case Array.uncons xs of
    Nothing -> pure Nothing
    Just { head, tail } -> do
      st <- get
      let
        Tuple r st' = runState (runExceptT (f head)) st
      case r of
        Right b -> put st' *> pure (Just b)
        Left _ -> firstOk tail f

  -- Concyclic: the circumcentre of the first three is equally far from all
  concyclicConj env pts ci ps = do
    p0 <- note "concyclic: points" (Array.index ps 0)
    p1 <- note "concyclic: points" (Array.index ps 1)
    p2 <- note "concyclic: points" (Array.index ps 2)
    a <- getPt pts p0
    b <- getPt pts p1
    c <- getPt pts p2
    zc <- pm env (circumcentre a.value b.value c.value)
    let
      zName = "z" <> show ci
      uz = uptOf env zc
    dz <- denominator uz
    gd <- addGoal { fn: "d", args: [ zName ], neg: false, rel: FNonZero } uz.d dz
    let
      pts' = pts { byName = Map.insert zName { name: zName, upt: uz, value: zc, def: uptJson uz } pts.byName }
    gs <- traverse
      ( \p -> do
          up <- getPt pts' p
          zeroGoal { fn: "circN", args: [ zName, p0, p ], neg: false, rel: FZero } (circN uz a.upt up.upt)
      )
      ps
    pure [ obj [ Tuple "kind" (str "concyclic"), Tuple "args" (strs ps), Tuple "centre" (str zName), Tuple "centreDef" (uptJson uz), Tuple "dgoal" (int gd), Tuple "g" (ints gs) ] ]

  getPt pts name = note ("internal: unknown point " <> name) (Map.lookup name pts.byName)

  -----------------------------------------------------------------------------
  -- Goals

  goalIndex g = do
    st <- get
    note ("internal: missing goal " <> g.fn) (Array.findIndex (\x -> sameGoal x.goal g) st.goals)

  -- records a goal with its derivation; the position in the goal list
  addGoal g poly d = do
    st <- get
    case Array.findIndex (\x -> sameGoal x.goal g) st.goals of
      Just k -> pure k
      Nothing -> do
        i <- liftC (C.addExact { p: poly, rel: g.rel } d)
        modify_ \s -> s { goals = Array.snoc s.goals { goal: g, index: i } }
        pure (Array.length st.goals)

  sameGoal a b = a.fn == b.fn && a.args == b.args && a.neg == b.neg && a.rel == b.rel

  -- G = 0: an identity modulo the square roots
  zeroGoal g poly = addGoal g poly (Derived { p: poly, rel: FZero } (ByLin { mult: unitTerm, terms: [] }))

  -- G ≠ 0 with G = v · f in the tower
  nonZeroGoal env g poly sem = do
    st <- get
    let
      ctx = st.ctx
      fv = flatten env sem.v
    zv <- note ("could not show " <> g.fn <> " " <> joinWith " " g.args <> " ≠ 0 (value " <> Sym.pcText dbgNames sem.v <> ")") (towerNonZero env ctx sem.v)
    zf <- note "a factor could not be shown nonzero" (certNonZero ctx sem.f)
    k <- note "internal: nonzero ratio" (ratioOf (signPoly env sem.v * sem.f) ((factOf zv).p * (factOf zf).p))
    addGoal g poly (Derived { p: poly, rel: FNonZero } (ByLinNZ { m: fv.d * fv.d, c: k, refs: [ zv, zf ] }))

  -- G > 0 or ≥ 0 with G = v · fq² in the tower
  signGoal env g poly sem = do
    st <- get
    let
      ctx = st.ctx
      fv = flatten env sem.v
      qv = sqPart ctx fv.d
      qf = sqPart ctx sem.fq
      viaSign = towerSign env ctx sem.v >>= \t ->
        if t.sigma /= one then Nothing
        else if g.rel == FPos && not (t.rel == FPos && qf.strict) then Nothing
        else if g.rel == FNonNeg && not qv.strict then Nothing
        else Just (Derived { p: poly, rel: g.rel } (ByLin { mult: termWith one [] qv.sqrefs qv.sq, terms: [ termWith one [ t.deriv ] qf.sqrefs qf.sq ] }))
      -- a sum of squares that is nonzero
      viaSos = case sem.sos of
        Just sqs | g.rel == FPos -> do
          zv <- towerNonZero env ctx sem.v
          let
            zq = case P.constValue sem.fq of
              Just _ -> []
              Nothing -> Array.catMaybes [ certNonZero ctx sem.fq ]
            refs = [ zv ] <> zq <> zq
          when (P.constValue sem.fq == Nothing && Array.null zq) Nothing
          k <- ratioOf (signPoly env sem.v * sem.fq * sem.fq) (foldl (\acc d -> acc * (factOf d).p) one refs)
          let
            nn = byTerms { p: poly, rel: FNonNeg } (map (\q -> termWith one [] [] q) sqs)
            nz = Derived { p: poly, rel: FNonZero } (ByLinNZ { m: fv.d * fv.d, c: k, refs })
          pure (Derived { p: poly, rel: FPos } (ByStrict nn nz))
        _ -> Nothing
      -- ≥ 0 of a value that is zero: an identity
      viaZero =
        if g.rel == FNonNeg && PF.isZero sem.v then Just (Derived { p: poly, rel: FNonNeg } (ByLin { mult: unitTerm, terms: [] }))
        else Nothing
    d <- note ("could not show " <> g.fn <> " " <> joinWith " " g.args <> " " <> (if g.rel == FPos then "> 0" else "≥ 0"))
      (case viaZero of
        Just x -> Just x
        Nothing -> case viaSign of
          Just x -> Just x
          Nothing -> viaSos)
    addGoal g poly d

  -----------------------------------------------------------------------------

  paramVar _ i = fromMaybe ("x" <> show i) (Array.index (Array.mapMaybe (\j -> leanVar <$> Array.index n.vars j) (Array.filter (\j -> not (Map.member j n.substMap)) (0 .. (Array.length n.vars - 1)))) i)

  -- a coordinate n / m of a given point with denominator D: m · k = c · D
  coordJson env var d v = case PF.deconstruct v of
    PF.PBase f ->
      let
        nm = P.rename (RF.num f) env.ren
        dm = P.rename (RF.den f) env.ren
        k0 = fromMaybe zero (P.divExact d dm)
        l = foldl Q.lcmInt one (map (Q.denominator <<< _.coeff) (P.terms k0))
        nt = Sym.polyLean var nm
        dt = Sym.polyLean var dm
      in
        if dm == one then
          obj [ Tuple "expr" (str ("(" <> nt <> " : ℝ)")), Tuple "num" (str ("(" <> nt <> " : ℝ)")), Tuple "den" (str "1"), Tuple "k" (C.polyJson d), Tuple "c" (str "1") ]
        else
          obj
            [ Tuple "expr" (str ("((" <> nt <> ") / (" <> dt <> ") : ℝ)"))
            , Tuple "num" (str ("(" <> nt <> " : ℝ)"))
            , Tuple "den" (str ("(" <> dt <> " : ℝ)"))
            , Tuple "k" (C.polyJson (P.scale l k0))
            , Tuple "c" (str (ratLit l))
            ]
    PF.PQuad _ _ _ -> jsonNull

  dbgNames = { text: \i -> "x" <> show i, typst: \i -> "x" <> show i }

  pm :: forall a. Env -> PF.PM a -> EM a
  pm env m = liftE (PF.runPM env.oracle m)

data Curve = CLine String String | CCircle String String

curveJson :: Curve -> Json
curveJson = case _ of
  CLine a b -> obj [ Tuple "line" (strs [ a, b ]) ]
  CCircle o a -> obj [ Tuple "circle" (strs [ o, a ]) ]

uptJson :: UPt -> Json
uptJson u = obj [ Tuple "x" (C.polyJson u.x), Tuple "y" (C.polyJson u.y), Tuple "d" (C.polyJson u.d) ]

cmpName :: Cmp -> String
cmpName = case _ of
  CEq -> "=="
  CNe -> "!="
  CLt -> "<"
  CLe -> "<="
  CGt -> ">"
  CGe -> ">="

relLean :: Rel -> String
relLean = case _ of
  RPos -> ">"
  RNeg -> "<"
  RNonZero -> "≠"
  RNonNeg -> "≥"
  RNonPos -> "≤"

relKind :: Rel -> String
relKind = case _ of
  RPos -> "pos"
  RNeg -> "neg"
  RNonZero -> "nonzero"
  RNonNeg -> "nonneg"
  RNonPos -> "nonpos"

ratLit :: Rational -> String
ratLit q =
  if Q.isInteger q then Q.numeratorString q
  else "(" <> Q.numeratorString q <> " / " <> Q.denominatorString q <> ")"

-- | A hypothesis in readable form, with the scale c of its fact (the fact is
-- | c · x for the readable x, or c · (v − k) for `v ≠ k`).
condJson :: forall r. (Int -> String) -> Array Int -> { poly :: Poly, rel :: Rel, text :: String | r } -> Json
condJson var renArr h = condJsonL var { poly: P.rename h.poly renArr, rel: h.rel, text: h.text }

-- | `condJson` for a polynomial in Lean variables.
condJsonL :: (Int -> String) -> { poly :: Poly, rel :: Rel, text :: String } -> Json
condJsonL var h =
  let
    p = h.poly
    kf = knownFact { poly: p, rel: h.rel }
    fp = (factOf kf.deriv).p
    stored = P.normalise fp
    -- the stored fact is λ · σ · p with λ > 0
    lam = recip (if Q.signum stored.c < 0 then negate stored.c else stored.c)
    e = Sym.polyLean var p
    linear = case P.vars p, h.rel of
      [ v ], RNonZero | P.degIn p v == 1 ->
        let
          cs = P.coeffsIn p v
          a = fromMaybe zero (Array.index cs 1 >>= P.constValue)
          b = fromMaybe zero (Array.index cs 0 >>= P.constValue)
        in
          Just { v: var v, k: negate b / a, a }
      _, _ -> Nothing
  in
    case linear of
      Just l -> obj
        [ Tuple "stmt" (str (l.v <> " ≠ " <> ratLit l.k))
        , Tuple "text" (str h.text)
        , Tuple "kind" (str "ne")
        , Tuple "c" (str (ratLit (lam * l.a)))
        , Tuple "v" (str l.v)
        , Tuple "k" (str (ratLit l.k))
        , Tuple "fact" (C.factJson { p: P.scale lam fp, rel: (factOf kf.deriv).rel })
        ]
      Nothing -> obj
        [ Tuple "stmt" (str (e <> " " <> relLean h.rel <> " 0"))
        , Tuple "text" (str h.text)
        , Tuple "kind" (str (relKind h.rel))
        , Tuple "c" (str (ratLit lam))
        , Tuple "expr" (str e)
        , Tuple "fact" (C.factJson { p: P.scale lam fp, rel: (factOf kf.deriv).rel })
        ]

-- | A constant coordinate of a point with denominator D (m = 1, k = D).
constCoord :: Poly -> Rational -> Json
constCoord d q = obj
  [ Tuple "expr" (str (ratReal q))
  , Tuple "num" (str (ratReal q))
  , Tuple "den" (str "1")
  , Tuple "k" (C.polyJson d)
  , Tuple "c" (str "1")
  ]

ratReal :: Rational -> String
ratReal q = "(" <> (if Q.isInteger q then Q.numeratorString q else Q.numeratorString q <> " / " <> Q.denominatorString q) <> " : ℝ)"

-- | A Lean name for a variable.
leanVar :: VarInfo -> String
leanVar v =
  let
    t = v.text
  in
    if String.length t > 0 && all ok (String.toCodePointArray t) then t else "«" <> t <> "»"
  where
  ok c = let s = String.singleton c in String.contains (String.Pattern s) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_'"

-------------------------------------------------------------------------------
-- Geometry in the tower (as the prover computes it)

dist2 :: SPoint -> SPoint -> PF.PM PC
dist2 p q = PF.run (sq (PF.ex p.x - PF.ex q.x) + sq (PF.ex p.y - PF.ex q.y))
  where
  sq a = a * a

-- | (q − p) × (r − p)
cross :: SPoint -> SPoint -> SPoint -> PF.PM PC
cross p q r = PF.run ((PF.ex q.x - PF.ex p.x) * (PF.ex r.y - PF.ex p.y) - (PF.ex q.y - PF.ex p.y) * (PF.ex r.x - PF.ex p.x))

-- | (q − p) · (r − p)
dot :: SPoint -> SPoint -> SPoint -> PF.PM PC
dot p q r = PF.run ((PF.ex q.x - PF.ex p.x) * (PF.ex r.x - PF.ex p.x) + (PF.ex q.y - PF.ex p.y) * (PF.ex r.y - PF.ex p.y))

-- | (b − a) × (d − c)
crossDir :: SPoint -> SPoint -> SPoint -> SPoint -> PF.PM PC
crossDir a b c d = PF.run ((PF.ex b.x - PF.ex a.x) * (PF.ex d.y - PF.ex c.y) - (PF.ex b.y - PF.ex a.y) * (PF.ex d.x - PF.ex c.x))

circumcentre :: SPoint -> SPoint -> SPoint -> PF.PM SPoint
circumcentre a b c = do
  let
    e = PF.ex
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

-------------------------------------------------------------------------------
-- Coverage: one theorem per `cases` statement

-- | The split nodes, with a Lean name (`cover`, `case_two.cover`).
splitsOf :: PNode -> Array { name :: String, label :: String, node :: PNode }
splitsOf = go [] []
  where
  go names labels node@(PNode n) = case n.split of
    Nothing -> []
    Just sp ->
      [ { name: (if Array.null names then "" else "case_" <> joinWith "_" names <> ".") <> "cover"
        , label: joinWith " › " labels
        , node
        }
      ]
        <> Array.concatMap
          ( \b ->
              let
                many = Array.length b.cases > 1
              in
                Array.concat
                  ( Array.mapWithIndex
                      ( \i c -> case c.node of
                          Just nd -> go
                            (Array.snoc names (labelText b.label <> (if many then "_" <> solutionName i c.text else "")))
                            (Array.snoc labels (labelText b.label <> (if c.text == "" then "" else " (" <> c.text <> ")")))
                            nd
                          Nothing -> []
                      )
                      b.cases
                  )
          )
          sp.branches

-- | A condition of a case: a polynomial fact, and its readable form.
type CaseCond = { fact :: C.SFact, json :: Json }

-- | The decision tree.
data CTree
  = CSplit Poly CTree CTree CTree
  | CChain (Array C.LStep) CTree
  | CCase Int (Array C.LStep) (Array Int)
  | CAbsurd (Array C.LStep) Int Int

-- | The facts of a region of the tree: signs (as conditions for the sign
-- | engine) and zeros (with their index in the chain).
type Region = { conds :: Array S.Cond, zeros :: Array { p :: Poly, idx :: Int } }

coverJson :: String -> String -> PNode -> Either String Json
coverJson leanName label (PNode n) = do
  sp <- case n.split of
    Just x -> Right x
    Nothing -> Left "internal: not a split"
  let
    keep = Array.filter (\i -> not (Map.member i n.substMap)) (0 .. (Array.length n.vars - 1))
    nparams = Array.length keep
    renArr = map (\i -> fromMaybe (-1) (Array.elemIndex i keep)) (0 .. (Array.length n.vars - 1))
    paramNames = Array.mapMaybe (\i -> leanVar <$> Array.index n.vars i) keep
    var i = fromMaybe ("x" <> show i) (Array.index paramNames i)
    renH h = { poly: P.rename h.poly renArr, rel: h.rel, text: h.text }
    -- the cases: the conditions of the branches (an inequality, or a
    -- solution of the branch's equations with its conditions)
    disjuncts = Array.concatMap
      ( \b -> case b.cover of
          Nothing -> []
          Just cv
            | not (Array.null cv.ineq) ->
                if cv.refuted || Array.null (Array.mapMaybe _.node b.cases) then []
                else [ { label: labelText b.label, conds: map (hypCond var <<< renH) (Array.filter (\h -> P.constValue h.poly == Nothing) cv.ineq) } ]
            | otherwise -> Array.catMaybes
                ( Array.zipWith
                    ( \o c -> case c.node of
                        Just _ | o.possible ->
                          let
                            fresh = Array.filter (\(Tuple v _) -> not (Map.member v n.substMap)) (Map.toUnfoldable o.subst)
                            eqs = Array.concatMap (\(Tuple v f) -> substConds var renArr v f) fresh
                            side = map (\h -> renH h { poly = substPoly o.subst h.poly }) o.hyps
                          in
                            Just
                              { label: labelText b.label <> " (" <> o.text <> ")"
                              , conds: eqs <> map (hypCond var) (Array.filter (\h -> P.constValue h.poly == Nothing) side)
                              }
                        _ -> Nothing
                    )
                    cv.options
                    b.cases
                )
      )
      sp.branches
    -- the polynomials to split on: the conditions of the cases
    queue0 = Array.nubByEq (\a b -> (P.normalise a).p == (P.normalise b).p)
      ( Array.filter (\p -> P.constValue p == Nothing)
          ( Array.concatMap
              ( \b -> case b.cover of
                  Just cv -> map (\h -> P.rename h.poly renArr) cv.ineq
                    <> map (\p -> P.rename p renArr) cv.eqs
                  Nothing -> []
              )
              sp.branches
              <> Array.concatMap (\d -> map (\c -> c.fact.p) d.conds) disjuncts
          )
      )
    proverConds = map (\h -> { poly: P.rename h.poly renArr, rel: h.rel }) n.hyps
    st0 = { chain: C.emptyChain nparams, goals: [], ctx: { conds: proverConds, samples: [] } }
    -- in one variable, the rational roots too (as linear factors)
    withRoots = Array.concatMap
      ( \p -> [ p ] <> case P.vars p of
          [ v ] -> case P.toUni p v >>= P.ratRoots of
            Just rs -> map (\r0 -> P.var v - P.constant r0) rs
            Nothing -> []
          _ -> []
      )
      queue0
    queue = Array.nubByEq (\a b -> (P.normalise a).p == (P.normalise b).p) (map primitive withRoots)
    Tuple r _ = runState (runExceptT (coverBody nparams n disjuncts queue renArr)) st0
  { rootSteps, tree } <- r
  pure
    ( obj
        [ Tuple "name" (str leanName)
        , Tuple "label" (str label)
        , Tuple "split" (str sp.text)
        , Tuple "params" (strs paramNames)
        , Tuple "hyps" (fromArray (map (condJson var renArr) n.theorem))
        , Tuple "facts" (fromArray (map (\h -> C.factJson (storedFact (factOf (knownFact { poly: P.rename h.poly renArr, rel: h.rel }).deriv))) n.theorem))
        , Tuple "nparams" (int nparams)
        , Tuple "root" (fromArray (map C.stepJson rootSteps))
        , Tuple "tree" (treeJson tree)
        , Tuple "cases" (fromArray (map (\d -> obj [ Tuple "label" (str d.label), Tuple "conds" (fromArray (map _.json d.conds)) ]) disjuncts))
        , Tuple "D" (fromArray (map (\d -> fromArray (map (C.factJson <<< _.fact) d.conds)) disjuncts))
        ]
    )
  where
  primitive p = P.scale (recip (P.normalise p).c) p

-- | The stored form of a fact (primitive integer, its sign kept), as
-- | `Proof.Cert` stores hypotheses.
storedFact :: Fact' -> C.SFact
storedFact f =
  let
    nc = (P.normalise f.p).c
    a = if Q.signum nc < 0 then negate nc else nc
  in
    { p: P.scale (recip a) f.p, rel: f.rel }

type Fact' = { p :: Poly, rel :: FRel }

-- | A hypothesis (in Lean variables) as a condition of a case.
hypCond :: (Int -> String) -> { poly :: Poly, rel :: Rel, text :: String } -> CaseCond
hypCond var h =
  { fact: storedFact (factOf (knownFact { poly: h.poly, rel: h.rel }).deriv), json: condJsonL var h }

-- | `v = value` as conditions: `m ≠ 0` (a non-constant denominator) and
-- | `v · m − n = 0`.
substConds :: (Int -> String) -> Array Int -> Int -> RF.RatFun -> Array CaseCond
substConds var renArr v f =
  let
    nm = P.rename (RF.num f) renArr
    dm = P.rename (RF.den f) renArr
    lv = fromMaybe 0 (Array.index renArr v)
    eqp = P.var lv * dm - nm
    st = storedFact { p: eqp, rel: FZero }
    -- the stored fact is c · (v · m − n)
    c = ratioOf st.p eqp
    vt = var lv
  in
    case P.constValue dm of
      Just _ ->
        [ { fact: st
          , json: obj
              [ Tuple "stmt" (str (vt <> " = " <> Sym.polyLean var nm))
              , Tuple "kind" (str "eq")
              , Tuple "c" (str (ratLit (fromMaybe one c)))
              , Tuple "v" (str vt)
              , Tuple "k" (str (Sym.polyLean var nm))
              ]
          }
        ]
      Nothing ->
        let
          nzSt = storedFact { p: dm, rel: FNonZero }
          cm = ratioOf nzSt.p dm
          mt = Sym.polyLean var dm
          nt = Sym.polyLean var nm
        in
          [ { fact: nzSt
            , json: obj [ Tuple "stmt" (str (mt <> " ≠ 0")), Tuple "kind" (str "nonzero"), Tuple "c" (str (ratLit (fromMaybe one cm))), Tuple "expr" (str mt) ]
            }
          , { fact: st
            , json: obj
                [ Tuple "stmt" (str (vt <> " = (" <> nt <> ") / (" <> mt <> ")"))
                , Tuple "kind" (str "eqdiv")
                , Tuple "c" (str (ratLit (fromMaybe one c)))
                , Tuple "v" (str vt)
                , Tuple "n" (str ("(" <> nt <> ")"))
                , Tuple "m" (str ("(" <> mt <> ")"))
                ]
            }
          ]

treeJson :: CTree -> Json
treeJson = case _ of
  CSplit p a b c -> obj [ Tuple "split" (C.polyJson p), Tuple "neg" (treeJson a), Tuple "zero" (treeJson b), Tuple "pos" (treeJson c) ]
  CChain steps t -> obj [ Tuple "chain" (fromArray (map C.stepJson steps)), Tuple "next" (treeJson t) ]
  CCase k steps idx -> obj [ Tuple "case" (int k), Tuple "steps" (fromArray (map C.stepJson steps)), Tuple "idx" (ints idx) ]
  CAbsurd steps i j -> obj [ Tuple "absurd" (ints [ i, j ]), Tuple "steps" (fromArray (map C.stepJson steps)) ]

coverBody
  :: forall r
   . Int
  -> { hyps :: Array Hyp', theorem :: Array Hyp' | r }
  -> Array { label :: String, conds :: Array CaseCond }
  -> Array Poly
  -> Array Int
  -> EM { rootSteps :: Array C.LStep, tree :: CTree }
coverBody _ n disjuncts queue renArr = do
  let
    renH h = { poly: P.rename h.poly renArr, rel: h.rel }
    isKept h = Array.any (\t -> t.poly == h.poly && t.rel == h.rel) n.theorem
    indexed = Array.mapWithIndex Tuple n.hyps
  liftC (C.addHyps (map (\h -> factOf (knownFact (renH h)).deriv) n.theorem))
  -- the dropped hypotheses, as in the leaves
  for_ (Array.reverse indexed) \(Tuple i h) -> unless (isKept h) do
    let
      ctx =
        { conds: map (renH <<< (\(Tuple _ x) -> x)) (Array.filter (\(Tuple j x) -> j > i || (j < i && isKept x)) indexed)
        , samples: []
        }
      c = renH h
      d = case c.rel of
        RNonZero -> certNonZero ctx c.poly
        _ -> certSign ctx c.poly >>= \p -> case c.rel, p.answer of
          RPos, APos _ -> Just p.deriv
          RNeg, ANeg _ -> Just p.deriv
          RNonNeg, APos _ -> Just p.deriv
          RNonNeg, ANonNeg _ -> Just p.deriv
          RNonPos, ANeg _ -> Just p.deriv
          RNonPos, ANonPos _ -> Just p.deriv
          _, _ -> Nothing
    dd <- note ("the hypothesis '" <> h.text <> "' (dropped in the PDF) could not be derived") d
    liftC (void (C.addDeriv dd))
  st <- get
  let
    rootSteps = st.chain.steps
  modify_ \s -> s { chain = s.chain { steps = [] } }
  tree <- build (Array.length queue + 1) queue { conds: [], zeros: [] }
  pure { rootSteps, tree }
  where
  build fuel q region = do
    st <- get
    let
      ctx = { conds: st.ctx.conds <> region.conds, samples: [] }
    -- a case whose conditions follow
    caseLeaf <- firstJust (Array.mapWithIndex Tuple disjuncts) \(Tuple k d) ->
      case traverse (\c -> derivTarget ctx region c.fact) d.conds of
        Nothing -> pure Nothing
        Just ds -> do
          before <- get
          r <- tryEM do
            idx <- traverse (\(Tuple c dv) -> liftC (C.addExact c.fact dv)) (Array.zip d.conds ds)
            after <- get
            pure (CCase k after.chain.steps idx)
          put before
          pure r
    case caseLeaf of
      Just t -> pure t
      Nothing -> do
        ab <- absurdLeaf ctx region
        case ab of
          Just t -> pure t
          Nothing -> case Array.uncons q of
            Nothing -> throwError
              ( "coverage: a configuration satisfies none of the cases (or it could not be decided), where "
                  <> joinWith ", " (map (\c -> P.key c.poly <> " " <> relLean c.rel <> " 0") region.conds <> map (\z -> P.key z.p <> " = 0") region.zeros)
              )
            Just { head: f, tail }
              | fuel <= 0 -> throwError "coverage: the decision tree is too deep"
              | Array.any (\z -> isJust (ratioOf f z.p)) region.zeros -> build (fuel - 1) tail region
              | Just t <- known ctx f -> do
                  -- the sign is known: derive it as a fact of the region
                  before <- get
                  _ <- liftC (C.addDeriv t.deriv)
                  after <- get
                  let
                    steps = after.chain.steps
                  modify_ \s -> s { chain = s.chain { steps = [] } }
                  rest <- build (fuel - 1) tail region { conds = Array.snoc region.conds { poly: f, rel: if t.sigma == one then RPos else RNeg } }
                  put before
                  pure (CChain steps rest)
              | otherwise -> do
                  neg <- sub (negate f) FPos (\_ -> region { conds = Array.snoc region.conds { poly: f, rel: RNeg } }) (build (fuel - 1) tail)
                  zer <- sub f FZero (\i -> region { zeros = Array.snoc region.zeros { p: f, idx: i } }) (build (fuel - 1) tail)
                  pos <- sub f FPos (\_ -> region { conds = Array.snoc region.conds { poly: f, rel: RPos } }) (build (fuel - 1) tail)
                  pure (CSplit f neg zer pos)
  -- a subtree with one more fact; the chain is restored afterwards
  sub p rel mkRegion k = do
    before <- get
    i <- liftC (C.addFact { p, rel })
    modify_ \s -> s { chain = s.chain { steps = [] } }
    t <- k (mkRegion i)
    put before
    pure t
  -- the sign of f is already known: no split needed
  known :: Context -> Poly -> Maybe { sigma :: Rational, deriv :: Deriv }
  known ctx f = certSign ctx f >>= \p -> case p.answer of
    APos _ -> Just { sigma: one, deriv: p.deriv }
    ANeg _ -> Just { sigma: negate one, deriv: p.deriv }
    _ -> Nothing
  absurdLeaf ctx region = do
    st <- get
    let
      facts = Array.mapWithIndex Tuple st.chain.facts
      -- the contradiction targets: the opposite of each fact
      targets = Array.mapMaybe
        ( \(Tuple i f) -> case f.rel of
            FPos -> Just { i, t: { p: negate f.p, rel: FNonNeg }, first: true }
            FNonNeg -> Just { i, t: { p: negate f.p, rel: FPos }, first: false }
            FNonZero -> Just { i, t: { p: f.p, rel: FZero }, first: false }
            FZero -> Just { i, t: { p: f.p, rel: FNonZero }, first: true }
        )
        (Array.reverse facts)
    firstJust targets \x -> do
      let
        region' = region { zeros = Array.filter (\z -> z.idx /= x.i) region.zeros }
      case derivTarget ctx region' x.t of
        Nothing -> pure Nothing
        Just dv -> do
          before <- get
          r <- tryEM do
            j <- liftC (C.addExact x.t dv)
            after <- get
            -- contra i j: K[i] = 0 or > 0, K[j] ≠ 0 or ≥ −K[i]
            pure case x.t.rel of
              FNonNeg -> CAbsurd after.chain.steps x.i j
              FPos -> CAbsurd after.chain.steps j x.i
              FZero -> CAbsurd after.chain.steps j x.i
              FNonZero -> CAbsurd after.chain.steps x.i j
          put before
          pure r
  firstJust :: forall a b. Array a -> (a -> EM (Maybe b)) -> EM (Maybe b)
  firstJust xs f = case Array.uncons xs of
    Nothing -> pure Nothing
    Just { head, tail } -> f head >>= case _ of
      Just b -> pure (Just b)
      Nothing -> firstJust tail f
  tryEM :: forall a. EM a -> EM (Maybe a)
  tryEM m = do
    st <- get
    let
      Tuple r st' = runState (runExceptT m) st
    case r of
      Right a -> put st' *> pure (Just a)
      Left _ -> pure Nothing

type Hyp' = { poly :: Poly, rel :: Rel, text :: String, origin :: HypOrigin, line :: Int, step :: Int }

-- | A derivation of the fact `t` (exactly its polynomial) in a region: the
-- | zeros of the region (linear ones, with a constant coefficient) are
-- | substituted, and the sign of the rest comes from the sign engine.
derivTarget :: Context -> Region -> C.SFact -> Maybe Deriv
derivTarget ctx region t =
  let
    -- a multiple of a zero of the region is zero; else substitute
    red = case Array.findMap (\z -> (\k -> { k, z: z.p }) <$> ratioOf t.p z.p) region.zeros of
      Just m -> { r: zero, eqs: [ { c: P.constant m.k, z: m.z } ] }
      Nothing -> reduceZeros region.zeros t.p
    eqs = map (\x -> { c: x.c, ref: Known { p: x.z, rel: FZero } }) red.eqs
    r = red.r
  in
    if P.isZero r then case t.rel of
      FZero -> Just (Derived t (BySum { mult: unitTerm, terms: [], eqs }))
      FNonNeg -> Just (Derived t (BySum { mult: unitTerm, terms: [], eqs }))
      _ -> Nothing
    else case t.rel of
      FZero -> Nothing
      FNonZero -> do
        dz <- certNonZero ctx r
        k <- ratioOf r (factOf dz).p
        pure (Derived t (ByNonZero { m: one, c: k, refs: [ dz ], eqs }))
      _ -> do
        pr <- certSign ctx r
        ok <- case t.rel, pr.answer of
          FPos, APos _ -> Just true
          FNonNeg, APos _ -> Just true
          FNonNeg, ANonNeg _ -> Just true
          _, _ -> Nothing
        when (not ok) Nothing
        pure
          if Array.null eqs && (factOf pr.deriv).p == t.p then pr.deriv
          else Derived t (BySum { mult: unitTerm, terms: [ termWith one [ pr.deriv ] [] one ], eqs })

-- | p = r + Σ cᵢ zᵢ, substituting the zeros zᵢ that are linear in a variable
-- | with a constant coefficient.
reduceZeros :: Array { p :: Poly, idx :: Int } -> Poly -> { r :: Poly, eqs :: Array { c :: Poly, z :: Poly } }
reduceZeros zs p0 = foldl step { r: p0, eqs: [] } zs
  where
  step acc z = case Array.find (\v -> P.degIn z.p v == 1 && isJust (Array.index (P.coeffsIn z.p v) 1 >>= P.constValue)) (P.vars z.p) of
    Nothing -> acc
    Just v ->
      if P.degIn acc.r v == 0 then acc
      else
        let
          cs = P.coeffsIn z.p v
          a = fromMaybe zero (Array.index cs 1)
          b = fromMaybe zero (Array.index cs 0)
          sub = P.substFrac acc.r v (negate b) a
          r' = P.scale (recip (Q.pow (fromMaybe one (P.constValue a)) sub.degree)) sub.poly
        in
          case P.divExact (acc.r - r') z.p of
            Just q -> { r: r', eqs: Array.snoc acc.eqs { c: q, z: z.p } }
            Nothing -> acc
