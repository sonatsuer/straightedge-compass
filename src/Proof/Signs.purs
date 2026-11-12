-- | The sign engine: what is known about the sign of a polynomial on the
-- | set of parameters where the hypotheses hold.
-- |
-- | The rules are sound but incomplete, and each one comes with a
-- | certificate that Lean checks (see `Proof.Cert`):
-- |
-- | 1. constants;
-- | 2. a hypothesis that is a constant multiple of the polynomial;
-- | 3. dividing out hypotheses of known sign; the rest is a constant times a
-- |    square, a sum of squares of linear forms, or a sum of monomials of
-- |    known sign;
-- | 4. a hypothesis that the polynomial divides, with a cofactor of known
-- |    sign; a hypothesis with the same leading term and a rest of known
-- |    sign; for ≠ 0, a hypothesis it divides, or a positive square.
-- |
-- | In one variable, an exact cell decomposition of ℝ finds witnesses of
-- | both signs when there are any (it proves nothing). Otherwise exact
-- | evaluation at sample points of the domain either finds both signs (two
-- | witnesses) or gives evidence for an error message.
module Proof.Signs
  ( Rel(..)
  , Cond
  , Valuation
  , Context
  , Answer(..)
  , Proved
  , signOf
  , signOfRat
  , certSign
  , certNonZero
  , knownFact
  , isNonZero
  , relHolds
  , condHolds
  , findPoints
  , toSgn
  , oracleFor
  , oracleWith
  ) where

import Prelude

import Constructible.ParamField (Oracle, Sgn(..))
import Constructible.Poly (Poly)
import Constructible.Poly as P
import Constructible.RatFun (RatFun)
import Constructible.RatFun as RF
import Constructible.Rational (Rational)
import Constructible.Rational as Q
import Data.Array ((..))
import Data.Array as Array
import Data.Foldable (all, any, foldl)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Tuple (Tuple(..))
import Proof.Cert (Deriv(..), FRel(..), Rule(..), byTerms, factOf, termWith)

-- | The relation of a hypothesis polynomial to zero.
data Rel = RPos | RNeg | RNonZero | RNonNeg | RNonPos

derive instance Eq Rel

type Cond = { poly :: Poly, rel :: Rel }

-- | Values of the variables, by index.
type Valuation = Array (Maybe Rational)

type Context = { conds :: Array Cond, samples :: Array Valuation }

-- | The answers carry a justification for the reader.
data Answer
  = APos String
  | ANeg String
  | AZero
  | ANonNeg String
  | ANonPos String
  | ANonZero String -- ^ never zero (the sign may change)
  | AVaries Valuation Valuation -- ^ a point where it is positive, one where negative
  | AUnknown String

-- | A proved answer about f, with the derivation of its fact: f > 0
-- | (`APos`), −f > 0 (`ANeg`), f ≥ 0, −f ≥ 0, f ≠ 0 or f = 0.
type Proved = { answer :: Answer, deriv :: Deriv }

toSgn :: Answer -> Sgn
toSgn = case _ of
  APos _ -> Pos
  ANeg _ -> Neg
  AZero -> Zero
  ANonNeg _ -> NonNeg
  ANonPos _ -> NonPos
  _ -> Unknown

-- | The oracle for towers over ℚ(x) on this context. `showPoint` names a
-- | configuration and `showFun` a rational function, for error messages.
oracleWith :: (Valuation -> String) -> (RatFun -> String) -> Context -> Oracle
oracleWith showPoint showFun ctx =
  { sign: \f -> toSgn (signOfRat ctx f)
  , explain: \f -> case signOfRat ctx f of
      AVaries p n ->
        "the sign of " <> showFun f <> " is not determined: it is positive at " <> showPoint p
          <> " and negative at "
          <> showPoint n
      AUnknown why -> "the sign of " <> showFun f <> " could not be decided (" <> why <> ")"
      ANonNeg _ -> showFun f <> " is only known to be ≥ 0"
      ANonPos _ -> showFun f <> " is only known to be ≤ 0"
      _ -> "internal: sign of " <> showFun f
  }

oracleFor :: Context -> Oracle
oracleFor = oracleWith show RF.key

relHolds :: Rel -> Int -> Boolean
relHolds rel s = case rel of
  RPos -> s > 0
  RNeg -> s < 0
  RNonZero -> s /= 0
  RNonNeg -> s >= 0
  RNonPos -> s <= 0

condHolds :: Valuation -> Cond -> Boolean
condHolds vals c = case P.evalAt c.poly vals of
  Just v -> relHolds c.rel (Q.signum v)
  Nothing -> false

-- | The sign of a rational function, from the signs of its numerator and
-- | denominator (which is nonzero on the domain).
signOfRat :: Context -> RatFun -> Answer
signOfRat ctx f
  | RF.den f == one = signOf ctx (RF.num f)
  | otherwise =
      let
        sn = signOf ctx (RF.num f)
        sd = case signOf ctx (RF.den f) of
          ANonNeg w -> APos w
          ANonPos w -> ANeg w
          a -> a
      in
        case svOf sn, svOf sd of
          SUnknown, _ -> signOf ctx (RF.num f * RF.den f)
          _, SUnknown -> signOf ctx (RF.num f * RF.den f)
          a, b -> answerOf (times a b) (reasonOf sn <> "; the denominator: " <> reasonOf sd)
      where
      reasonOf = case _ of
        APos w -> w
        ANeg w -> w
        ANonNeg w -> w
        ANonPos w -> w
        _ -> ""

signOf :: Context -> Poly -> Answer
signOf ctx f = case certSign ctx f of
  Just p -> p.answer
  Nothing -> case univariate ctx f of
    Just a -> a
    Nothing -> evidence ctx f

-- | f is nonzero everywhere on the domain (by a certificate).
isNonZero :: Context -> Poly -> Boolean
isNonZero ctx f = isJust (certNonZero ctx f)

-------------------------------------------------------------------------------
-- Certified signs

-- | How deep the rules may call each other.
depthLimit :: Int
depthLimit = 3

-- | A hypothesis as a fact: σ · poly with its relation, σ = ±1.
knownFact :: Cond -> { sigma :: Rational, deriv :: Deriv }
knownFact c = case c.rel of
  RPos -> { sigma: one, deriv: Known { p: c.poly, rel: FPos } }
  RNeg -> { sigma: negate one, deriv: Known { p: negate c.poly, rel: FPos } }
  RNonNeg -> { sigma: one, deriv: Known { p: c.poly, rel: FNonNeg } }
  RNonPos -> { sigma: negate one, deriv: Known { p: negate c.poly, rel: FNonNeg } }
  RNonZero -> { sigma: one, deriv: Known { p: c.poly, rel: FNonZero } }

-- | The proved answer for σ · f with the relation of the fact (σ = ±1).
answerFor :: Rational -> FRel -> String -> Answer
answerFor sigma rel why = case rel, Q.signum sigma > 0 of
  FPos, true -> APos why
  FPos, false -> ANeg why
  FNonNeg, true -> ANonNeg why
  FNonNeg, false -> ANonPos why
  FNonZero, _ -> ANonZero why
  FZero, _ -> AZero

-- | The sign of f with a certificate.
certSign :: Context -> Poly -> Maybe Proved
certSign = certSignAt depthLimit

certSignAt :: Int -> Context -> Poly -> Maybe Proved
certSignAt depth ctx f
  | P.isZero f = Just { answer: AZero, deriv: byTerms { p: f, rel: FZero } [] }
  | Just c <- P.constValue f = Just (constant f c)
  | otherwise = prove depth ctx f

constant :: Poly -> Rational -> Proved
constant f c
  | Q.signum c > 0 = { answer: APos "a positive constant", deriv: byTerms { p: f, rel: FPos } [ termWith c [] [] one ] }
  | otherwise = { answer: ANeg "a negative constant", deriv: byTerms { p: negate f, rel: FPos } [ termWith (negate c) [] [] one ] }

-- | A derivation that f ≠ 0: a fact f ≠ 0, or ±f > 0.
certNonZero :: Context -> Poly -> Maybe Deriv
certNonZero = nonZeroAt depthLimit

nonZeroAt :: Int -> Context -> Poly -> Maybe Deriv
nonZeroAt depth ctx g
  | Just c <- P.constValue g =
      if Q.isZero c then Nothing
      else Just (Derived { p: g, rel: FNonZero } (ByNonZero { m: one, c, refs: [], eqs: [] }))
  | depth <= 0 = Nothing
  | otherwise = case certSignAt (depth - 1) ctx g of
      Just { answer: APos _, deriv } -> Just deriv
      Just { answer: ANeg _, deriv } -> Just deriv
      Just { answer: ANonZero _, deriv } -> Just deriv
      _ -> Array.findMap (\rule -> rule unit)
        [ \_ -> Array.findMap multiple ctx.conds
        , \_ -> divided
        , \_ -> Array.findMap dividing ctx.conds
        , \_ -> Array.findMap common ctx.conds
        , \_ -> square
        ]
  where
  nonzeroRel c = c.rel /= RNonNeg && c.rel /= RNonPos
  nz q = { p: q, rel: FNonZero }
  -- g = k · c with c ≠ 0
  multiple c
    | c.rel == RNonZero = multipleOf g c.poly <#> \k ->
        Derived (nz g) (ByNonZero { m: one, c: k, refs: [ (knownFact c).deriv ], eqs: [] })
    | otherwise = Nothing
  -- g = Π cᵢ^kᵢ · h with the cᵢ ≠ 0 and h ≠ 0
  divided =
    let
      usable = Array.filter (\c -> nonzeroRel c && P.constValue c.poly == Nothing) ctx.conds
      step acc c =
        let
          go k q = case P.divExact q c.poly of
            Just q' | k < 64 -> go (k + 1) q'
            _ -> Tuple k q
          Tuple k left = go 0 acc.h
        in
          if k == 0 then acc else { h: left, refs: acc.refs <> Array.replicate k (knownFact c).deriv }
      r = foldl step { h: g, refs: [] } usable
    in
      if Array.null r.refs then Nothing
      else do
        dh <- case P.constValue r.h of
          Just c | not (Q.isZero c) -> Just []
          _ -> pure <$> nonZeroAt (depth - 1) ctx r.h
        let
          refs = r.refs <> dh
        k <- multipleOf g (foldl (\acc d -> acc * (factOf d).p) one refs)
        pure (Derived (nz g) (ByNonZero { m: one, c: k, refs, eqs: [] }))
  -- c = g · h with c ≠ 0
  dividing c
    | nonzeroRel c && P.constValue c.poly == Nothing = dividingBy g c
    | otherwise = Nothing
  -- g = h · k with h a common factor of g and of c ≠ 0, and k ≠ 0
  common c
    | nonzeroRel c && P.constValue c.poly == Nothing = do
        let
          h = P.gcd g c.poly
        when (P.constValue h /= Nothing) Nothing
        k <- P.divExact g h
        when (P.constValue k /= Nothing) Nothing
        dh <- dividingBy h c
        dk <- nonZeroAt (depth - 1) ctx k
        kk <- multipleOf g ((factOf dh).p * (factOf dk).p)
        pure (Derived (nz g) (ByNonZero { m: one, c: kk, refs: [ dh, dk ], eqs: [] }))
    | otherwise = Nothing
  -- h ≠ 0 because it divides c ≠ 0
  dividingBy h c = do
    q <- P.divExact c.poly h
    let
      kf = knownFact c
    pure (Derived (nz h) (ByNonZero { m: P.scale kf.sigma q, c: one, refs: [ kf.deriv ], eqs: [] }))
  -- g² > 0
  square = case certSignAt (depth - 1) ctx (g * g) of
    Just { answer: APos _, deriv } -> Just (Derived (nz g) (ByNonZero { m: g, c: one, refs: [ deriv ], eqs: [] }))
    _ -> Nothing

prove :: Int -> Context -> Poly -> Maybe Proved
prove depth ctx f =
  case direct ctx f of
    Just a@{ answer: APos _ } -> Just a
    Just a@{ answer: ANeg _ } -> Just a
    d ->
      let
        candidates =
          [ d
          , rest depth ctx f
          , factored depth ctx f
          ]
        found = best candidates
        more = if isJust (strictOf found) then [] else [ dividedBy depth ctx f, leading depth ctx f ]
      in
        strengthen (best (candidates <> more))
  where
  best as = case Array.find strict (Array.catMaybes as) of
    Just a -> Just a
    Nothing -> Array.head (Array.catMaybes as)
  strictOf m = m >>= \a -> if strict a then Just a else Nothing
  strict a = case a.answer of
    APos _ -> true
    ANeg _ -> true
    _ -> false
  -- ≥ 0 and ≠ 0 (a hypothesis) give > 0
  strengthen = map \a -> case a.answer, nonZeroHyp of
    ANonNeg w, Just z -> { answer: APos (w <> ", and nonzero by hypothesis"), deriv: Derived { p: f, rel: FPos } (ByStrict a.deriv z) }
    ANonPos w, Just z -> { answer: ANeg (w <> ", and nonzero by hypothesis"), deriv: Derived { p: negate f, rel: FPos } (ByStrict a.deriv z) }
    _, _ -> a
  nonZeroHyp = Array.findMap
    (\c -> if c.rel == RNonZero && isJust (multipleOf f c.poly) then Just (knownFact c).deriv else Nothing)
    ctx.conds

-- | f = c · g with c a nonzero constant.
multipleOf :: Poly -> Poly -> Maybe Rational
multipleOf f g =
  let
    nf = P.normalise f
    ng = P.normalise g
  in
    if not (P.isZero g) && not (P.isZero f) && nf.p == ng.p then Just (nf.c / ng.c) else Nothing

-- | A hypothesis that is a constant multiple of f (a strict one first).
direct :: Context -> Poly -> Maybe Proved
direct ctx f =
  let
    all' = Array.mapMaybe match ctx.conds
    strictA p = case p.answer of
      APos _ -> true
      ANeg _ -> true
      _ -> false
  in
    case Array.find strictA all' of
      Just p -> Just p
      Nothing -> Array.head all'
  where
  match c
    | c.rel == RNonZero = Nothing
    | otherwise = multipleOf f c.poly >>= \k ->
        let
          kf = knownFact c
          kappa = k * kf.sigma -- f = κ · fact
          rel = (factOf kf.deriv).rel
          sigma = if Q.signum kappa > 0 then one else negate one
        in
          Just
            { answer: answerFor sigma rel "by hypothesis"
            , deriv: byTerms { p: P.scale sigma f, rel } [ termWith (kappa * sigma) [ kf.deriv ] [] one ]
            }

-------------------------------------------------------------------------------
-- Signs of products

data SV = SPos | SNeg | SNonNeg | SNonPos | SUnknown

derive instance Eq SV

times :: SV -> SV -> SV
times a b = case a, b of
  SUnknown, _ -> SUnknown
  _, SUnknown -> SUnknown
  SPos, x -> x
  x, SPos -> x
  SNeg, x -> flipSV x
  x, SNeg -> flipSV x
  SNonNeg, SNonNeg -> SNonNeg
  SNonPos, SNonPos -> SNonNeg
  _, _ -> SNonPos

flipSV :: SV -> SV
flipSV = case _ of
  SPos -> SNeg
  SNeg -> SPos
  SNonNeg -> SNonPos
  SNonPos -> SNonNeg
  SUnknown -> SUnknown

answerOf :: SV -> String -> Answer
answerOf s why = case s of
  SPos -> APos why
  SNeg -> ANeg why
  SNonNeg -> ANonNeg why
  SNonPos -> ANonPos why
  SUnknown -> AUnknown why

svOf :: Answer -> SV
svOf = case _ of
  APos _ -> SPos
  ANeg _ -> SNeg
  ANonNeg _ -> SNonNeg
  ANonPos _ -> SNonPos
  _ -> SUnknown

-- | The sign σ (±1) and relation of a proved answer.
signRel :: Answer -> Maybe { sigma :: Rational, rel :: FRel }
signRel = case _ of
  APos _ -> Just { sigma: one, rel: FPos }
  ANeg _ -> Just { sigma: negate one, rel: FPos }
  ANonNeg _ -> Just { sigma: one, rel: FNonNeg }
  ANonPos _ -> Just { sigma: negate one, rel: FNonNeg }
  _ -> Nothing

reason :: Answer -> String
reason = case _ of
  APos w -> w
  ANeg w -> w
  ANonNeg w -> w
  ANonPos w -> w
  AUnknown w -> w
  ANonZero w -> w
  _ -> ""

-- | Divides out the hypotheses, then reads off the sign of the rest.
factored :: Int -> Context -> Poly -> Maybe Proved
factored depth ctx f0 =
  let
    usable = Array.filter (\c -> P.constValue c.poly == Nothing && c.rel /= RNonZero) ctx.conds
    step acc c =
      let
        go k g = case P.divExact g c.poly of
          Just q | k < 64 -> go (k + 1) q
          _ -> Tuple k g
        Tuple k left = go 0 acc.f
        kf = knownFact c
      in
        if k == 0 then acc
        else
          { f: left
          , sigma: acc.sigma * Q.pow kf.sigma k
          , refs: acc.refs <> Array.replicate k kf.deriv
          , strict: acc.strict && (factOf kf.deriv).rel == FPos
          }
    divided = foldl step { f: f0, sigma: one, refs: [], strict: true } usable
  in
    if Array.null divided.refs then Nothing
    else do
      r <- rest depth ctx divided.f
      sr <- signRel r.answer
      let
        sigma = divided.sigma * sr.sigma
        rel = if divided.strict && sr.rel == FPos then FPos else FNonNeg
      -- f = σ · Π facts · (σ_r · rest)
      pure
        { answer: answerFor sigma rel ("a product of hypotheses and " <> reason r.answer)
        , deriv: byTerms { p: P.scale sigma f0, rel } [ termWith one (divided.refs <> [ r.deriv ]) [] one ]
        }

-- | The sign of what is left after dividing out the hypotheses.
rest :: Int -> Context -> Poly -> Maybe Proved
rest depth ctx f
  | Just c <- P.constValue f = if Q.isZero c then Nothing else Just (constant f c)
  | otherwise = Array.findMap (\rule -> rule unit) [ \_ -> square, \_ -> quadratic, \_ -> monomials ]
      where
      lc = P.leadingCoeff f
      sigma = if Q.signum lc > 0 then one else negate one
      -- f = lc · s²
      square = P.sqrt (P.scale (recip lc) f) <#> \s ->
        case nonZeroAt (depth - 1) ctx s of
          Just d ->
            { answer: answerFor sigma FPos (if sigma == one then "a square, nonzero on the domain" else "minus a square, nonzero on the domain")
            , deriv: byTerms { p: P.scale sigma f, rel: FPos } [ termWith (lc * sigma) [] [ d ] one ]
            }
          Nothing ->
            { answer: answerFor sigma FNonNeg (if sigma == one then "a square" else "minus a square")
            , deriv: byTerms { p: P.scale sigma f, rel: FNonNeg } [ termWith (lc * sigma) [] [] s ]
            }
      -- ±f = Σ dᵢ ℓᵢ² (dᵢ > 0); strict with a constant ℓ, or with a
      -- hypothesis ℓ ≠ 0
      quadratic = Array.findMap sos [ one, negate one ]
      sos s =
        let
          g = P.scale s f
          plain = P.ldl g <#> \ds -> { ds, nzVar: Nothing }
          aligned = Array.findMap (alignedWith g) ctx.conds
        in
          case plain of
            Nothing -> Nothing
            Just pl ->
              let
                strictPlain = Array.any (\t -> isJust (P.constValue t.l)) pl.ds
                chosen = if strictPlain then pl else fromMaybe pl aligned
                terms = map
                  ( \t -> case chosen.nzVar of
                      Just hv | t.l == hv.l -> termWith t.d [] [ hv.deriv ] (P.constant hv.scale)
                      _ -> termWith t.d [] [] t.l
                  )
                  chosen.ds
                isStrict = strictPlain || isJust chosen.nzVar
                rel = if isStrict then FPos else FNonNeg
              in
                Just
                  { answer: answerFor s rel
                      ( if isStrict then (if s == one then "a sum of squares with a positive part" else "minus a sum of squares with a positive part")
                        else if s == one then "a positive semidefinite quadratic"
                        else "a negative semidefinite quadratic"
                      )
                  , deriv: byTerms { p: g, rel } terms
                  }
      -- an LDL in which a hypothesis h ≠ 0 (linear) is one of the forms
      alignedWith g c
        | c.rel == RNonNeg || c.rel == RNonPos = Nothing
        | P.totalDeg c.poly /= 1 = Nothing
        | not (all (\v -> Array.elem v (P.vars g)) (P.vars c.poly)) = Nothing
        | otherwise = do
            v <- Array.head (P.vars c.poly)
            let
              cs = P.coeffsIn c.poly v
              a = fromMaybe zero (Array.index cs 1 >>= P.constValue)
              restH = fromMaybe zero (Array.index cs 0)
              fresh = 1 + foldl max 0 (P.vars g)
              others = Array.filter (_ /= v) (P.vars g)
            when (Q.isZero a) Nothing
            -- v = (H − rest) / a
            let
              sub = P.substFrac g v (P.var fresh - restH) (P.constant a)
              g' = P.scale (recip (Q.pow a sub.degree)) sub.poly
            ds <- P.ldlOrder g' (others <> [ fresh ])
            let
              back l = (P.substFrac l fresh c.poly one).poly
              ds' = map (\t -> t { l = back t.l }) ds
            hv <- Array.find (\t -> isJust (multipleOf t.l c.poly)) ds'
            k <- multipleOf hv.l c.poly
            let
              kf = knownFact c
            -- ℓ = k · poly = k σ · fact
            pure { ds: ds', nzVar: Just { l: hv.l, deriv: kf.deriv, scale: k * kf.sigma } }
      -- every monomial has a known sign: even powers, or variables of known sign
      monomials =
        let
          ts = P.terms f
          varFact v = Array.findMap
            ( \c -> if c.rel == RNonZero then Nothing
                else multipleOf (P.var v) c.poly <#> \k ->
                  let
                    kf = knownFact c
                  in
                    { kappa: k * kf.sigma, deriv: kf.deriv, strict: (factOf kf.deriv).rel == FPos }
            )
            ctx.conds
          termOf t =
            let
              vs = Array.mapWithIndex Tuple t.exps
              parts = map
                ( \(Tuple v e) ->
                    if e == 0 then Just { c: one, refs: [], sq: one, strict: true }
                    else case varFact v of
                      Just vf -> Just { c: Q.pow vf.kappa e, refs: Array.replicate e vf.deriv, sq: one, strict: vf.strict }
                      Nothing ->
                        if Int.even e then Just { c: one, refs: [], sq: P.pow (P.var v) (e / 2), strict: false }
                        else Nothing
                )
                vs
            in
              Array.foldM
                (\acc m -> m <#> \x -> { c: acc.c * x.c, refs: acc.refs <> x.refs, sq: acc.sq * x.sq, strict: acc.strict && x.strict })
                { c: t.coeff, refs: [], sq: one, strict: true }
                parts
        in
          do
            parts <- Array.foldM (\acc t -> termOf t <#> \x -> Array.snoc acc x) [] ts
            let
              pos = all (\x -> Q.signum x.c > 0) parts
              neg = all (\x -> Q.signum x.c < 0) parts
            s <- if pos then Just one else if neg then Just (negate one) else Nothing
            let
              isStrict = any _.strict parts
              rel = if isStrict then FPos else FNonNeg
            pure
              { answer: answerFor s rel (if isStrict then "a sum of terms of one sign" else "a sum of terms of one weak sign")
              , deriv: byTerms { p: P.scale s f, rel } (map (\x -> termWith (x.c * s) x.refs [] x.sq) parts)
              }

-- | f divides a hypothesis h = f · g with a cofactor g of known sign:
-- | g · f = h.
dividedBy :: Int -> Context -> Poly -> Maybe Proved
dividedBy depth ctx f
  | depth <= 0 = Nothing
  | otherwise = Array.findMap try ctx.conds
      where
      try c
        | c.rel == RNonZero || P.constValue c.poly /= Nothing = Nothing
        | otherwise = do
            let
              kf = knownFact c
              h = (factOf kf.deriv).p
              hRel = (factOf kf.deriv).rel
            g <- P.divExact h f
            when (P.constValue g /= Nothing) Nothing
            pg <- certSignAt (depth - 1) ctx g
            sg <- signRel pg.answer
            -- σ_g g · (σ_g f) = h, so σ_g f has the relation of h when σ_g g ≥ 0
            -- (> 0 needed for a weak h)
            when (hRel == FNonNeg && sg.rel /= FPos) Nothing
            pure
              { answer: answerFor sg.sigma hRel ("it divides a hypothesis, with a cofactor of known sign")
              , deriv: Derived { p: P.scale sg.sigma f, rel: hRel }
                  (BySum { mult: termWith one [ pg.deriv ] [] one, terms: [ termWith one [ kf.deriv ] [] one ], eqs: [] })
              }

-- | f = c · h + r with h a hypothesis with the same leading term (c > 0) and
-- | r of the same sign.
leading :: Int -> Context -> Poly -> Maybe Proved
leading depth ctx f
  | depth <= 0 = Nothing
  | otherwise = Array.findMap (\s -> Array.findMap (try s) ctx.conds) [ one, negate one ]
      where
      lead p = (\t -> t.exps) <$> Array.head (P.terms p)
      try s c
        | c.rel == RNonZero || P.constValue c.poly /= Nothing = Nothing
        | otherwise = do
            let
              g = P.scale s f
              kf = knownFact c
              h = (factOf kf.deriv).p
              hRel = (factOf kf.deriv).rel
            lg <- lead g
            lh <- lead h
            when (lg /= lh) Nothing
            let
              k = P.leadingCoeff g / P.leadingCoeff h
            when (Q.signum k <= 0) Nothing
            let
              r = g - P.scale k h
            pr <- if P.isZero r then Nothing else certSignAt (depth - 1) ctx r
            sr <- signRel pr.answer
            when (Q.signum sr.sigma < 0) Nothing
            let
              rel = if hRel == FPos || sr.rel == FPos then FPos else FNonNeg
            pure
              { answer: answerFor s rel "a hypothesis plus a term of the same sign"
              , deriv: byTerms { p: g, rel } [ termWith k [ kf.deriv ] [] one, termWith one [ pr.deriv ] [] one ]
              }

-------------------------------------------------------------------------------
-- One variable: witnesses of both signs

univariate :: Context -> Poly -> Maybe Answer
univariate ctx f = case P.vars f of
  [ v ] -> do
    uf <- P.toUni f v
    let
      relevant = Array.filter (\c -> P.vars c.poly == [ v ]) ctx.conds
      unis = Array.mapMaybe (\c -> (\u -> { u, rel: c.rel }) <$> P.toUni c.poly v) relevant
      cs = P.cells (Array.cons uf (map _.u unis))
      inDomain cell = all identity (Array.mapWithIndex (\i c -> relHolds c.rel (fromMaybe 0 (Array.index cell.signs (i + 1)))) unis)
      admissible = Array.filter inDomain cs
      witness s = Array.find (\cell -> not cell.root && Array.index cell.signs 0 == Just s) admissible >>= _.sample
      at x = map (\s -> Array.updateAt v (Just x) s) ctx.samples
    case witness 1, witness (-1) of
      Just p, Just n -> case Array.findMap identity (at p), Array.findMap identity (at n) of
        Just vp, Just vn
          | all (condHolds vp) ctx.conds && all (condHolds vn) ctx.conds -> Just (AVaries vp vn)
        _, _ -> Nothing
      _, _ -> Nothing
  _ -> Nothing

-------------------------------------------------------------------------------
-- Evidence from sample points

-- | Up to n points of the domain: the samples and deterministic random
-- | perturbations of them that satisfy every hypothesis.
findPoints :: Context -> Int -> Array Valuation
findPoints ctx n =
  let
    seeds = Array.filter (\s -> all (condHolds s) ctx.conds) ctx.samples
    base = if Array.null seeds then ctx.samples else seeds
    candidates = Array.concatMap (\s -> map (\i -> perturb i s) (0 .. (max 120 (4 * n)))) base
    good = Array.filter (\p -> all (condHolds p) ctx.conds) candidates
  in
    Array.take n (Array.nub good)
  where
  perturb i s = Array.mapWithIndex
    ( \v x -> x <#> \q ->
        let
          r = lcg (i * 7919 + v * 104729 + 13)
          scale = case i `mod` 3 of
            0 -> Q.fromInts 1 8
            1 -> Q.fromInts 1 2
            _ -> Q.fromInt 2
        in
          if i == 0 then q else q + scale * Q.fromInts ((r `mod` 201) - 100) 100
    )
    s

lcg :: Int -> Int
lcg x = ((x * 1103515245 + 12345) `mod` 2147483647 + 2147483647) `mod` 2147483647

evidence :: Context -> Poly -> Answer
evidence ctx f =
  let
    pts = findPoints ctx 24
    valued = Array.mapMaybe (\p -> (\v -> { p, s: Q.signum v }) <$> P.evalAt f p) pts
    pos = Array.find (\x -> x.s > 0) valued
    neg = Array.find (\x -> x.s < 0) valued
  in
    case pos, neg of
      Just p, Just n -> AVaries p.p n.p
      Just _, Nothing -> AUnknown ("positive at " <> show (Array.length valued) <> " sampled configurations, but not proved")
      Nothing, Just _ -> AUnknown ("negative at " <> show (Array.length valued) <> " sampled configurations, but not proved")
      Nothing, Nothing -> AUnknown "no sampled configuration to test"

