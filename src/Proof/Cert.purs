-- | Sign certificates, as checked by Lean (`StraightedgeCompass.Cert`).
-- |
-- | A *derivation* proves a fact (a polynomial `> 0`, `≥ 0`, `≠ 0` or `= 0`)
-- | from known facts by a polynomial identity with rational coefficients:
-- |
-- | - `BySum`: `m · p = Σ terms + Σ e · z`, each term (and the multiplier `m`)
-- |   a product `c · Π fᵢ · Π gⱼ² · q²` of known facts and a square;
-- | - `ByNonZero`: `p · m = c · Π fᵢ + Σ e · z` with nonzero facts `fᵢ`;
-- | - `ByStrict`: `p ≥ 0` and `p ≠ 0`;
-- | - `ByLin`, `ByLinNZ`: like `BySum` and `ByNonZero`, but the identity only
-- |   holds modulo the relations `d · s² = n` of the square roots `s`; the
-- |   chain finds the multiples of those relations by reduction.
-- |
-- | A *chain* collects the facts of one theorem in order, from its
-- | hypotheses, with integer certificates, deduplicated, as Lean checks
-- | them. Every identity is checked here before it is emitted.
module Proof.Cert
  ( FRel(..)
  , Fact
  , Deriv(..)
  , Rule(..)
  , DTerm
  , DEq
  , factOf
  , unitTerm
  , termWith
  , byTerms
  , SFact
  , LCert(..)
  , LTerm
  , LEq
  , LStep(..)
  , Chain
  , ChainM
  , SqrtInfo
  , emptyChain
  , runChain
  , addHyps
  , addFact
  , addSqrt
  , addDeriv
  , addExact
  , reduce
  , termPoly
  , chainJson
  , stepJson
  , polyJson
  , factJson
  , relName
  ) where

import Prelude

import Constructible.Poly (Poly)
import Constructible.Poly as P
import Constructible.Rational (Rational)
import Constructible.Rational as Q
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, get, modify_, runState)
import Data.Argonaut.Core (Json, fromArray, fromNumber, fromObject, fromString)
import Data.Array ((..))
import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (foldl, for_)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object

-------------------------------------------------------------------------------
-- Derivations

data FRel = FPos | FNonNeg | FNonZero | FZero

derive instance Eq FRel

type Fact = { p :: Poly, rel :: FRel }

-- | A product `c · Π refs · Π sqrefs² · sq²`.
type DTerm = { c :: Rational, refs :: Array Deriv, sqrefs :: Array Deriv, sq :: Poly }

-- | `c · z` with `z` a known fact `= 0`.
type DEq = { c :: Poly, ref :: Deriv }

data Deriv
  = Known Fact
  | Derived Fact Rule

data Rule
  = BySum { mult :: DTerm, terms :: Array DTerm, eqs :: Array DEq }
  | ByNonZero { m :: Poly, c :: Rational, refs :: Array Deriv, eqs :: Array DEq }
  | ByStrict Deriv Deriv
  | ByLin { mult :: DTerm, terms :: Array DTerm }
  | ByLinNZ { m :: Poly, c :: Rational, refs :: Array Deriv }

factOf :: Deriv -> Fact
factOf = case _ of
  Known f -> f
  Derived f _ -> f

unitTerm :: DTerm
unitTerm = { c: one, refs: [], sqrefs: [], sq: one }

termWith :: Rational -> Array Deriv -> Array Deriv -> Poly -> DTerm
termWith c refs sqrefs sq = { c, refs, sqrefs, sq }

-- | `p = Σ terms`.
byTerms :: Fact -> Array DTerm -> Deriv
byTerms f terms = Derived f (BySum { mult: unitTerm, terms, eqs: [] })

-- | The polynomial of a term, in the polynomials of its facts.
termPoly :: DTerm -> Poly
termPoly t =
  let
    prod = foldl (\acc d -> acc * (factOf d).p) one
    sq = prod t.sqrefs
  in
    P.constant t.c * prod t.refs * sq * sq * t.sq * t.sq

-------------------------------------------------------------------------------
-- Reduction modulo the square roots

-- | A square root `s = √(n / d)` in the variable `v`, with `d > 0`.
type SqrtInfo = { v :: Int, n :: Poly, d :: Poly }

-- | `M · g = r + Σ cᵢ · (dᵢ sᵢ² − nᵢ)` with `r` of degree < 2 in every `sᵢ`
-- | and `M` the product of the `dᵢ` listed in `mult` (with repetitions).
-- | The roots are reduced from the last, whose relation may involve the
-- | earlier ones.
reduce :: Array SqrtInfo -> Poly -> { r :: Poly, mult :: Array Int, cof :: Array { i :: Int, c :: Poly } }
reduce sqrts g0 = foldl step { r: g0, mult: [], cof: [] } (Array.reverse (Array.mapWithIndex Tuple sqrts))
  where
  step acc (Tuple i s) =
    let
      cs = P.coeffsIn acc.r s.v
      e = Array.length cs - 1
      q = e / 2
    in
      if e < 2 then acc
      else
        let
          sv = P.var s.v
          dv2 = s.d * sv * sv
          parts = Array.mapWithIndex
            ( \j c ->
                let
                  h = j / 2
                  lowPart = (if Int.odd j then sv else one) * P.pow s.d (q - h)
                  geo = foldl (\a k -> a + P.pow dv2 k * P.pow s.n (h - 1 - k)) zero (if h > 0 then 0 .. (h - 1) else [])
                in
                  { red: c * lowPart * P.pow s.n h, cof: c * lowPart * geo }
            )
            cs
          dq = P.pow s.d q
        in
          { r: foldl (\a x -> a + x.red) zero parts
          , mult: acc.mult <> Array.replicate q i
          , cof: Array.snoc (map (\x -> x { c = x.c * dq }) acc.cof) { i, c: foldl (\a x -> a + x.cof) zero parts }
          }

-------------------------------------------------------------------------------
-- Chains

-- | A stored fact: a primitive integer polynomial (its sign kept).
type SFact = { p :: Poly, rel :: FRel }

type LTerm = { c :: Rational, refs :: Array Int, sqrefs :: Array Int, sq :: Poly }

type LEq = { c :: Poly, ref :: Int }

data LCert
  = LSum LTerm (Array LTerm) (Array LEq)
  | LNonZero Poly Rational (Array Int) (Array LEq)
  | LStrict Int Int

data LStep
  = LFact SFact LCert
  | LSqrt { n :: Poly, d :: Poly, rad :: Int, den :: Int }

type Chain =
  { nparams :: Int
  , nhyps :: Int
  , facts :: Array SFact
  , byKey :: Map String (Array Int)
  , steps :: Array LStep
  , sqrts :: Array SqrtInfo
  }

type ChainM = ExceptT String (State Chain)

emptyChain :: Int -> Chain
emptyChain nparams = { nparams, nhyps: 0, facts: [], byKey: Map.empty, steps: [], sqrts: [] }

runChain :: forall a. Chain -> ChainM a -> Tuple (Either String a) Chain
runChain c m = runState (runExceptT m) c

-- | p = c · q with q primitive and a positive leading coefficient.
normal :: Poly -> { c :: Rational, q :: Poly }
normal p
  | P.isZero p = { c: one, q: zero }
  | otherwise = let n = P.normalise p in { c: n.c, q: n.p }

-- | The stored form of a polynomial: λ · p, primitive, with p's sign.
store :: Poly -> { p :: Poly, lam :: Rational }
store p =
  let
    n = normal p
    a = if Q.signum n.c < 0 then negate n.c else n.c
  in
    { p: P.scale (recip a) p, lam: recip a }

keyOf :: Poly -> String
keyOf p = P.key (normal p).q

-- | A stored fact that serves as `f`: stored = λ · f.p.
findUsable :: Chain -> Fact -> Maybe { idx :: Int, lam :: Rational }
findUsable ch f = Array.findMap ok (fromMaybe [] (Map.lookup (keyOf f.p) ch.byKey))
  where
  nf = normal f.p
  ok i = do
    s <- Array.index ch.facts i
    let
      ns = normal s.p
      lam = if P.isZero f.p then one else ns.c / nf.c
      pos = Q.signum lam > 0
      good = case f.rel of
        FPos -> s.rel == FPos && pos
        FNonNeg -> (s.rel == FPos || s.rel == FNonNeg) && pos
        FNonZero -> s.rel == FPos || s.rel == FNonZero
        FZero -> s.rel == FZero
    if good then Just { idx: i, lam } else Nothing

push :: SFact -> Maybe LStep -> ChainM Int
push f step = do
  ch <- get
  let
    i = Array.length ch.facts
  modify_ \c -> c
    { facts = Array.snoc c.facts f
    , byKey = Map.insertWith (<>) (keyOf f.p) [ i ] c.byKey
    , steps = case step of
        Just s -> Array.snoc c.steps s
        Nothing -> c.steps
    }
  pure i

-- | The hypotheses, first in the chain (no step: Lean is given them).
addHyps :: Array Fact -> ChainM Unit
addHyps hs = do
  for_ hs \h -> push { p: (store h.p).p, rel: h.rel } Nothing
  modify_ \c -> c { nhyps = Array.length hs }

-- | A fact that Lean adds itself (a case of a split), exactly as given.
addFact :: SFact -> ChainM Int
addFact f = push f Nothing

-- | A square root step: the facts `n · d ≥ 0` (or > 0) and `d > 0` must be
-- | derived; adds `d s² − n = 0` and `s ≥ 0` (or > 0), as Lean does.
addSqrt :: SqrtInfo -> Deriv -> Deriv -> ChainM Unit
addSqrt s rad den = do
  r <- addDeriv rad
  d <- addDeriv den
  ch <- get
  radFact <- note' "internal: radicand fact" (Array.index ch.facts r.idx)
  denFact <- note' "internal: denominator fact" (Array.index ch.facts d.idx)
  -- Lean compares the facts with n · d and d exactly (as polynomials)
  rIdx <- exact r.idx radFact (s.n * s.d)
  when (denFact.rel /= FPos) (throwError "internal: the denominator fact is not d > 0")
  dIdx <- exact d.idx denFact s.d
  let
    sv = P.var s.v
    z = s.d * sv * sv - s.n
  modify_ \c -> c { steps = Array.snoc c.steps (LSqrt { n: s.n, d: s.d, rad: rIdx, den: dIdx }) }
  _ <- push { p: z, rel: FZero } Nothing
  _ <- push { p: sv, rel: if radFact.rel == FPos then FPos else FNonNeg } Nothing
  modify_ \c -> c { sqrts = Array.snoc c.sqrts s }

-- | A fact with exactly the polynomial p: the stored one, or p = k · stored.
exact :: Int -> SFact -> Poly -> ChainM Int
exact i f p
  | f.p == p = pure i
  | otherwise = case ratio p f.p of
      Just k | Q.signum k > 0 && Q.isInteger k ->
        push { p, rel: f.rel } (Just (LFact { p, rel: f.rel } (LSum { c: one, refs: [], sqrefs: [], sq: one } [ { c: k, refs: [ i ], sqrefs: [], sq: one } ] [])))
      _ -> throwError "internal: a square root fact has the wrong polynomial"
  where
  ratio a b =
    let
      na = normal a
      nb = normal b
    in
      if not (P.isZero b) && na.q == nb.q then Just (na.c / nb.c) else Nothing

-- | Adds a derivation of the goal f (its fact may be a stronger one, or one
-- | about −f for f ≠ 0); then a fact with exactly the goal's polynomial
-- | (Lean compares goals with the chain's facts as polynomials).
addExact :: Fact -> Deriv -> ChainM Int
addExact f d = do
  r <- addDeriv d
  ch <- get
  s <- note' "internal: a fact" (Array.index ch.facts r.idx)
  if s.p == f.p then pure r.idx
  else do
    -- f = k · stored
    k <- case normal f.p, normal s.p of
      nf, ns | nf.q == ns.q && not (P.isZero s.p) -> pure (nf.c / ns.c)
      _, _ -> throwError ("internal: the goal " <> P.key f.p <> " is not a multiple of its fact")
    let
      cert = case f.rel of
        FZero -> LSum unitL [] [ { c: P.constant k, ref: r.idx } ]
        FNonZero -> LNonZero one k [ r.idx ] []
        _ -> LSum unitL [ { c: k, refs: [ r.idx ], sqrefs: [], sq: one } ] []
    unless (Q.isInteger k) (throwError "internal: a goal is not an integer multiple of its fact")
    when ((f.rel == FPos || f.rel == FNonNeg) && Q.signum k <= 0) (throwError "internal: a goal has the wrong sign")
    push f (Just (LFact f cert))
  where
  unitL = { c: one, refs: [], sqrefs: [], sq: one }

note' :: forall a. String -> Maybe a -> ChainM a
note' msg = case _ of
  Just x -> pure x
  Nothing -> throwError msg

-- | Adds a derivation (and everything it uses) to the chain: the index of
-- | its fact, and λ with stored = λ · fact.
addDeriv :: Deriv -> ChainM { idx :: Int, lam :: Rational }
addDeriv d = do
  ch <- get
  let
    f = factOf d
  case findUsable ch f of
    Just r -> pure r
    Nothing -> case d of
      Known _ -> throwError ("internal: the known fact " <> P.key f.p <> " " <> relName f.rel <> " is not in the chain")
      Derived _ rule -> do
        let
          st = store f.p
          goal = { p: st.p, rel: f.rel }
        cert <- integral goal st.lam rule
        i <- push goal (Just (LFact goal cert))
        pure { idx: i, lam: st.lam }

-- | Lean's version of a rule, in stored polynomials with integers.
integral :: SFact -> Rational -> Rule -> ChainM LCert
integral goal lamG = case _ of
  BySum r -> do
    mt <- termOf r.mult
    ts <- traverse termOf r.terms
    es <- traverse eqOf r.eqs
    let
      lhs = mt.k / lamG
      dens = [ Q.denominator lhs ] <> map (Q.denominator <<< _.k) ts
        <> Array.concatMap (\x -> map (Q.denominator <<< _.coeff) (P.terms x.c)) es
      l = foldl Q.lcmInt one dens
      -- with no terms the identity may be negated (the multiplier only needs ≠ 0)
      sgn = if Q.signum lhs < 0 && Array.null ts then negate one else one
      lt t k = { c: l * sgn * k, refs: t.refs, sqrefs: t.sqrefs, sq: t.sq }
      mult = lt mt lhs
      terms = map (\t -> lt t t.k) ts
      eqs = map (\x -> { c: P.scale (l * sgn) x.c, ref: x.ref }) es
    when (Q.signum mult.c <= 0 || Array.any (\t -> Q.signum t.c <= 0) terms) do
      throwError "internal: a certificate coefficient is not positive"
    ch <- get
    let
      tp t = P.constant t.c * prodS ch t.refs * sqS ch t.sqrefs * t.sq * t.sq
      left = tp mult * goal.p
      right = foldl (\a t -> a + tp t) zero terms + foldl (\a x -> a + x.c * factP ch x.ref) zero eqs
    unless (left == right) (throwError ("internal: a certificate identity fails for " <> P.key goal.p))
    pure (LSum mult terms eqs)
  ByNonZero r -> do
    rs <- traverse addDeriv r.refs
    es <- traverse eqOf r.eqs
    let
      lamR = foldl (\a x -> a * x.lam) one rs
      c0 = r.c * lamG / lamR
      -- (stored / λ) · m = c · Π (storedᵢ / λᵢ) + …, times λ
      m0 = r.m
      es0 = map (\x -> x { c = P.scale lamG x.c }) es
      dens = [ Q.denominator c0 ] <> map (Q.denominator <<< _.coeff) (P.terms m0)
        <> Array.concatMap (\x -> map (Q.denominator <<< _.coeff) (P.terms x.c)) es0
      l = foldl Q.lcmInt one dens
      m = P.scale l m0
      c = l * c0
      refs = map _.idx rs
      eqs = map (\x -> { c: P.scale l x.c, ref: x.ref }) es0
    ch <- get
    let
      left = goal.p * m
      right = P.constant c * prodS ch refs + foldl (\a x -> a + x.c * factP ch x.ref) zero eqs
    unless (left == right && not (Q.isZero c)) (throwError ("internal: a nonzero certificate fails for " <> P.key goal.p))
    pure (LNonZero m c refs eqs)
  ByStrict dn dz -> do
    i <- addDeriv dn
    j <- addDeriv dz
    ch <- get
    let
      pi = factP ch i.idx
      pj = factP ch j.idx
    unless (pi == goal.p) (throwError "internal: strict: the nonnegative fact differs")
    j' <-
      if pj == goal.p then pure j.idx
      else if pj == negate goal.p then push { p: goal.p, rel: FNonZero } (Just (LFact { p: goal.p, rel: FNonZero } (LNonZero one (negate one) [ j.idx ] [])))
      else throwError "internal: strict: the nonzero fact differs"
    pure (LStrict i.idx j')
  ByLin r -> do
    ch <- get
    let
      f = P.scale (recip lamG) goal.p
      delta = termPoly r.mult * f - foldl (\a t -> a + termPoly t) zero r.terms
      red = reduce ch.sqrts delta
    unless (P.isZero red.r) (throwError ("internal: not an identity modulo the square roots (" <> show (P.size red.r) <> " terms remain)"))
    let
      ds = map (\i -> Known { p: (fromMaybe' i ch.sqrts).d, rel: FPos }) red.mult
      zs = map (\x -> { c: x.c, ref: Known { p: zOf (fromMaybe' x.i ch.sqrts), rel: FZero } }) red.cof
    integral goal lamG (BySum { mult: r.mult { refs = r.mult.refs <> ds }, terms: map (\t -> t { refs = t.refs <> ds }) r.terms, eqs: zs })
  ByLinNZ r -> do
    ch <- get
    let
      f = P.scale (recip lamG) goal.p
      delta = f * r.m - P.constant r.c * foldl (\a d -> a * (factOf d).p) one r.refs
      red = reduce ch.sqrts delta
    unless (P.isZero red.r) (throwError "internal: not an identity modulo the square roots (nonzero)")
    let
      dsPolys = map (\i -> (fromMaybe' i ch.sqrts).d) red.mult
      ds = map (\p -> Known { p, rel: FPos }) dsPolys
      mM = foldl (*) one dsPolys
      zs = map (\x -> { c: x.c, ref: Known { p: zOf (fromMaybe' x.i ch.sqrts), rel: FZero } }) red.cof
    integral goal lamG (ByNonZero { m: mM * r.m, c: r.c, refs: r.refs <> ds, eqs: zs })
  where
  fromMaybe' i xs = case Array.index xs i of
    Just x -> x
    Nothing -> { v: 0, n: zero, d: one }
  zOf s = s.d * P.var s.v * P.var s.v - s.n
  termOf t = do
    rs <- traverse addDeriv t.refs
    qs <- traverse addDeriv t.sqrefs
    let
      nsq = normal t.sq
      lamR = foldl (\a x -> a * x.lam) one rs
      lamQ = foldl (\a x -> a * x.lam * x.lam) one qs
      k = t.c * nsq.c * nsq.c / (lamR * lamQ)
    pure { k, refs: map _.idx rs, sqrefs: map _.idx qs, sq: nsq.q }
  eqOf e = do
    z <- addDeriv e.ref
    pure { c: P.scale (recip z.lam) e.c, ref: z.idx }
  factP ch i = case Array.index ch.facts i of
    Just s -> s.p
    Nothing -> zero
  prodS ch = foldl (\a i -> a * factP ch i) one
  sqS ch is = let p = prodS ch is in p * p

-------------------------------------------------------------------------------
-- JSON for Lean

relName :: FRel -> String
relName = case _ of
  FPos -> "pos"
  FNonNeg -> "nonneg"
  FNonZero -> "nonzero"
  FZero -> "zero"

obj :: Array (Tuple String Json) -> Json
obj = fromObject <<< Object.fromFoldable

int :: Int -> Json
int = fromNumber <<< Int.toNumber

-- | A polynomial with integer coefficients: `[[exponents, "coeff"], …]`, in
-- | Lean's canonical order (`Mono.cmp`: exponents compared from variable 0,
-- | ascending), so that Lean's merges and normal forms are linear.
polyJson :: Poly -> Json
polyJson p = fromArray
  ( map
      (\t -> fromArray [ fromArray (map int t.exps), fromString (Q.numeratorString t.coeff) ])
      (Array.sortBy (\a b -> leanOrder a.exps b.exps) (map (\t -> t { exps = trimmed t.exps }) (P.terms p)))
  )
  where
  trimmed es = Array.reverse (Array.dropWhile (_ == 0) (Array.reverse es))

-- | `Mono.cmp`: lexicographic from variable 0, missing exponents are zero.
leanOrder :: Array Int -> Array Int -> Ordering
leanOrder a b = go 0
  where
  n = max (Array.length a) (Array.length b)
  at xs i = fromMaybe 0 (Array.index xs i)
  go i
    | i >= n = EQ
    | otherwise = case compare (at a i) (at b i) of
        EQ -> go (i + 1)
        o -> o

factJson :: SFact -> Json
factJson f = obj [ Tuple "p" (polyJson f.p), Tuple "rel" (fromString (relName f.rel)) ]

termJson :: LTerm -> Json
termJson t = obj
  [ Tuple "c" (fromString (Q.numeratorString t.c))
  , Tuple "refs" (fromArray (map int t.refs))
  , Tuple "sqrefs" (fromArray (map int t.sqrefs))
  , Tuple "sq" (polyJson t.sq)
  ]

eqJson :: LEq -> Json
eqJson e = obj [ Tuple "c" (polyJson e.c), Tuple "ref" (int e.ref) ]

stepJson :: LStep -> Json
stepJson = case _ of
  LFact f c -> obj [ Tuple "fact" (factJson f), Tuple "cert" (certJson c) ]
  LSqrt s -> obj
    [ Tuple "sqrt" (obj [ Tuple "n" (polyJson s.n), Tuple "d" (polyJson s.d), Tuple "rad" (int s.rad), Tuple "den" (int s.den) ]) ]
  where
  certJson = case _ of
    LSum m ts es -> obj [ Tuple "sum" (obj [ Tuple "mult" (termJson m), Tuple "terms" (fromArray (map termJson ts)), Tuple "eqs" (fromArray (map eqJson es)) ]) ]
    LNonZero m c refs es -> obj
      [ Tuple "nonzero"
          (obj [ Tuple "m" (polyJson m), Tuple "c" (fromString (Q.numeratorString c)), Tuple "refs" (fromArray (map int refs)), Tuple "eqs" (fromArray (map eqJson es)) ])
      ]
    LStrict i j -> obj [ Tuple "strict" (fromArray [ int i, int j ]) ]

-- | The hypotheses, the square roots and the steps.
chainJson :: Chain -> Json
chainJson ch = obj
  [ Tuple "nparams" (int ch.nparams)
  , Tuple "hyps" (fromArray (map factJson (Array.take ch.nhyps ch.facts)))
  , Tuple "sqrts" (fromArray (map (\s -> fromArray [ polyJson s.n, polyJson s.d ]) ch.sqrts))
  , Tuple "steps" (fromArray (map stepJson ch.steps))
  ]
