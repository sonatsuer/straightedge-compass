-- | Exact arithmetic with constructible real numbers.
-- |
-- | A PureScript port of `Data.Real.Constructible` from the Haskell library
-- | `constructible` 0.1.2, © 2013 Anders Kaseorg, BSD-3-Clause (see NOTICE).
-- |
-- | A constructible number lives in a tower of quadratic extensions
-- | Q(√r₁)(√r₂)…(√rₙ), where each radicand rᵢ is an element of the field
-- | below it that is not a square there. An element at level n is a pair
-- | (a, b) of elements of level n−1, standing for a + b·√rₙ. Because every
-- | radicand is a non-square, this representation is unique, so equality is
-- | componentwise and the sign is decided exactly by recursion.
-- |
-- | The Haskell original tracks the tower shape at the type level (GADTs and
-- | type families). Here the shape is a runtime invariant: every function
-- | takes the field and an element of that field, and a mismatch is an
-- | internal error (`shapeError`).
module Constructible.Field
  ( Construct
  , Field(..)
  , Elt(..)
  , fromRational
  , fromInt
  , fromInts
  , sqrt
  , sign
  , abs
  , isZero
  , toNumber
  , toRational
  , deconstruct
  , Deconstructed(..)
  , depth
  , fieldOf
  , joinAll
  , showConstruct
  ) where

import Prelude

import Constructible.Rational (Rational)
import Constructible.Rational as Q
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Partial.Unsafe (unsafeCrashWith)
import Unsafe.Reference (unsafeRefEq)

-- | A tower of quadratic extensions of Q. `Sqrt k r` adjoins √r to `k`,
-- | where `r` is a positive element of `k` that is not a square in `k`.
data Field = QF | Sqrt Field Elt

-- | An element of a field. `R q` lives in Q. At a `Sqrt k r` level an element
-- | is `Zero` or `E a b` = a + b·√r with `a`, `b` in `k`, not both zero.
data Elt = R Rational | Zero | E Elt Elt

-- | A constructible real number: an element together with its field.
data Construct = C Field Elt

shapeError :: forall a. String -> a
shapeError fn = unsafeCrashWith ("Constructible.Field." <> fn <> ": element does not match its field")

-------------------------------------------------------------------------------
-- Arithmetic within one field

zeroK :: Field -> Elt
zeroK QF = R zero
zeroK (Sqrt _ _) = Zero

isZeroK :: Field -> Elt -> Boolean
isZeroK QF (R a) = Q.isZero a
isZeroK (Sqrt _ _) Zero = true
isZeroK (Sqrt _ _) (E _ _) = false
isZeroK _ _ = shapeError "isZeroK"

-- | Builds a + b√r over `k`, normalising 0 + 0√r to `Zero`.
sqrtElt :: Field -> Elt -> Elt -> Elt
sqrtElt k a b
  | isZeroK k a && isZeroK k b = Zero
  | otherwise = E a b

-- | Embeds an element of `k` into `k(√r)`.
sqrtLift :: Field -> Elt -> Elt
sqrtLift k a = sqrtElt k a (zeroK k)

fromRationalK :: Field -> Rational -> Elt
fromRationalK QF a = R a
fromRationalK (Sqrt k _) a = sqrtLift k (fromRationalK k a)

addK :: Field -> Elt -> Elt -> Elt
addK QF (R a) (R b) = R (a + b)
addK (Sqrt _ _) Zero a = a
addK (Sqrt _ _) a Zero = a
addK (Sqrt k _) (E a b) (E c d) = sqrtElt k (addK k a c) (addK k b d)
addK _ _ _ = shapeError "addK"

subK :: Field -> Elt -> Elt -> Elt
subK QF (R a) (R b) = R (a - b)
subK f@(Sqrt _ _) Zero a = negateK f a
subK (Sqrt _ _) a Zero = a
subK (Sqrt k _) (E a b) (E c d) = sqrtElt k (subK k a c) (subK k b d)
subK _ _ _ = shapeError "subK"

negateK :: Field -> Elt -> Elt
negateK QF (R a) = R (negate a)
negateK (Sqrt _ _) Zero = Zero
negateK (Sqrt k _) (E a b) = E (negateK k a) (negateK k b)
negateK _ _ = shapeError "negateK"

mulK :: Field -> Elt -> Elt -> Elt
mulK QF (R a) (R b) = R (a * b)
mulK (Sqrt _ _) Zero _ = Zero
mulK (Sqrt _ _) _ Zero = Zero
mulK (Sqrt k r) (E a b) (E c d) =
  E (addK k (mulK k a c) (mulK k r (mulK k b d))) (addK k (mulK k a d) (mulK k b c))
mulK _ _ _ = shapeError "mulK"

sqK :: Field -> Elt -> Elt
sqK QF (R a) = R (a * a)
sqK (Sqrt _ _) Zero = Zero
sqK (Sqrt k r) (E a b) =
  let
    c = mulK k a b
  in
    E (addK k (sqK k a) (mulK k r (sqK k b))) (addK k c c)
sqK _ _ = shapeError "sqK"

recipK :: Field -> Elt -> Elt
recipK QF (R a)
  | Q.isZero a = unsafeCrashWith "Constructible.Field: division by zero"
  | otherwise = R (recip a)
recipK (Sqrt _ _) Zero = unsafeCrashWith "Constructible.Field: division by zero"
recipK (Sqrt k r) (E a b) =
  let
    c = recipK k (subK k (sqK k a) (mulK k r (sqK k b)))
  in
    E (mulK k a c) (mulK k (negateK k b) c)
recipK _ _ = shapeError "recipK"

divK :: Field -> Elt -> Elt -> Elt
divK QF (R a) (R b)
  | Q.isZero b = unsafeCrashWith "Constructible.Field: division by zero"
  | otherwise = R (a / b)
divK k a b = mulK k a (recipK k b)

eqK :: Field -> Elt -> Elt -> Boolean
eqK QF (R a) (R b) = a == b
eqK (Sqrt _ _) Zero Zero = true
eqK (Sqrt k _) (E a b) (E c d) = eqK k a c && eqK k b d
eqK (Sqrt _ _) Zero (E _ _) = false
eqK (Sqrt _ _) (E _ _) Zero = false
eqK _ _ _ = shapeError "eqK"

-- | The sign of an element, decided exactly.
sgnK :: Field -> Elt -> Ordering
sgnK QF (R a) = compare a zero
sgnK (Sqrt _ _) Zero = EQ
sgnK (Sqrt k r) (E a b) = case sgnK k a, sgnK k b of
  o, EQ -> o
  EQ, o -> o
  GT, GT -> GT
  LT, LT -> LT
  -- Mixed signs: a + b√r has the sign of a² − r·b² when a > 0 > b, and the
  -- opposite sign when a < 0 < b.
  GT, LT -> sgnK k (subK k (sqK k a) (mulK k r (sqK k b)))
  LT, GT -> sgnK k (subK k (mulK k r (sqK k b)) (sqK k a))
sgnK _ _ = shapeError "sgnK"

compareK :: Field -> Elt -> Elt -> Ordering
compareK QF (R a) (R b) = compare a b
compareK k a b = sgnK k (subK k a b)

-- | The square root within the field, when it exists there. For a
-- | non-negative argument the result is the non-negative root.
sqrtK :: Field -> Elt -> Maybe Elt
sqrtK QF (R a) = R <$> Q.exactSqrt a
sqrtK (Sqrt _ _) Zero = Just Zero
sqrtK (Sqrt k r) (E a b)
  | isZeroK k b = case sqrtK k a of
      Just c -> Just (sqrtLift k c)
      Nothing -> E (zeroK k) <$> sqrtK k (divK k a r)
  | otherwise = do
      -- A square root of a + b√r can exist only if its norm a² − r·b² is a
      -- square n² in k; the root is then c + (q/c)√r with c² = (a+n)/2, or
      -- (q/d) + d√r with d² = (a+n)/(2r), where q = b/2.
      n <- sqrtK k (subK k (sqK k a) (mulK k r (sqK k b)))
      let
        half = fromRationalK k (Q.fromInts 1 2)
        p = mulK k half (addK k a n)
        q = mulK k half b
      case sqrtK k p of
        Just c -> Just (E c (divK k q c))
        Nothing -> do
          d <- sqrtK k (divK k p r)
          pure (E (divK k q d) d)
sqrtK _ _ = shapeError "sqrtK"

-------------------------------------------------------------------------------
-- Merging towers

-- | A common field together with embeddings of the two operands' fields.
type Join = { field :: Field, f1 :: Elt -> Elt, f2 :: Elt -> Elt }

-- | Structural equality of towers (reference equality as a fast path).
sameField :: Field -> Field -> Boolean
sameField k1 k2
  | unsafeRefEq k1 k2 = true
  | otherwise = case k1, k2 of
      QF, QF -> true
      Sqrt a r, Sqrt b s -> sameField a b && (unsafeRefEq r s || eqK a r s)
      _, _ -> false

fieldDepth :: Field -> Int
fieldDepth QF = 0
fieldDepth (Sqrt k _) = 1 + fieldDepth k

-- | `liftTo n k` embeds elements of the `n`-th ancestor of `k` into `k`.
liftFrom :: Int -> Field -> Elt -> Elt
liftFrom 0 _ a = a
liftFrom _ QF a = a
liftFrom n (Sqrt k _) a = sqrtLift k (liftFrom (n - 1) k a)

ancestor :: Int -> Field -> Field
ancestor 0 k = k
ancestor _ QF = QF
ancestor n (Sqrt k _) = ancestor (n - 1) k

-- | `joinK k1 k2` finds a field containing both, with the two embeddings.
-- |
-- | This is the original algorithm, plus fast paths for the common cases where
-- | one tower is a prefix of the other; those give the same result as the
-- | general recursion, which would rediscover every shared radicand.
joinK :: Field -> Field -> Join
joinK k1 k2
  | sameField k1 k2 = { field: k1, f1: identity, f2: identity }
  | otherwise =
      let
        d1 = fieldDepth k1
        d2 = fieldDepth k2
      in
        if d1 > d2 && sameField (ancestor (d1 - d2) k1) k2 then
          { field: k1, f1: identity, f2: liftFrom (d1 - d2) k1 }
        else if d2 > d1 && sameField (ancestor (d2 - d1) k2) k1 then
          { field: k2, f1: liftFrom (d2 - d1) k2, f2: identity }
        else joinGeneral k1 k2

joinGeneral :: Field -> Field -> Join
joinGeneral QF k = { field: k, f1: fromQ, f2: identity }
  where
  fromQ (R a) = fromRationalK k a
  fromQ _ = shapeError "joinK"
joinGeneral k QF = { field: k, f1: identity, f2: fromQ }
  where
  fromQ (R a) = fromRationalK k a
  fromQ _ = shapeError "joinK"
joinGeneral k1 (Sqrt k2 r) =
  let
    j = joinK k1 k2
    k = j.field
    r' = j.f2 r
  in
    case sqrtK k r' of
      Nothing ->
        let
          f2' Zero = Zero
          f2' (E a b) = E (j.f2 a) (j.f2 b)
          f2' _ = shapeError "joinK"
        in
          { field: Sqrt k r', f1: sqrtLift k <<< j.f1, f2: f2' }
      Just s ->
        let
          f2' Zero = zeroK k
          f2' (E a b) = addK k (j.f2 a) (mulK k (j.f2 b) s)
          f2' _ = shapeError "joinK"
        in
          { field: k, f1: j.f1, f2: f2' }

binary :: (Field -> Elt -> Elt -> Elt) -> Construct -> Construct -> Construct
binary op (C k1 a1) (C k2 a2) =
  let
    j = joinK k1 k2
  in
    demote j.field (op j.field (j.f1 a1) (j.f2 a2))

-- | Drops top tower levels whose √-coefficient is zero, so a number lives in
-- | the smallest prefix of its tower that contains it. (Not in the Haskell
-- | original; without it, e.g. the 1 in (0, 1) obtained via (0, ±√3) keeps
-- | √3 in every later tower.) Values and printing are unchanged.
demote :: Field -> Elt -> Construct
demote QF a = C QF a
demote (Sqrt _ _) Zero = C QF (R zero)
demote f@(Sqrt k _) e@(E a b)
  | isZeroK k b = demote k a
  | otherwise = C f e
demote _ _ = shapeError "demote"

-- | Embeds all numbers into one common tower (used by the Lean export).
joinAll :: Array Construct -> { field :: Field, elts :: Array Elt }
joinAll xs =
  let
    field = foldl (\k (C k' _) -> (joinK k k').field) QF xs
    embed (C k a) = (joinK field k).f2 a
  in
    { field, elts: map embed xs }

-------------------------------------------------------------------------------
-- The number type

instance Eq Construct where
  eq (C k1 a1) (C k2 a2) =
    let
      j = joinK k1 k2
    in
      eqK j.field (j.f1 a1) (j.f2 a2)

instance Ord Construct where
  compare (C k1 a1) (C k2 a2) =
    let
      j = joinK k1 k2
    in
      compareK j.field (j.f1 a1) (j.f2 a2)

instance Semiring Construct where
  add = binary addK
  mul = binary mulK
  zero = C QF (R zero)
  one = C QF (R one)

instance Ring Construct where
  sub = binary subK

instance CommutativeRing Construct

instance DivisionRing Construct where
  recip (C k a) = C k (recipK k a)

-- | A field: `div` is exact division, `mod` is always zero.
instance EuclideanRing Construct where
  degree _ = 1
  div = binary divK
  mod _ _ = zero

instance Show Construct where
  show = showConstruct

fromRational :: Rational -> Construct
fromRational q = C QF (R q)

fromInt :: Int -> Construct
fromInt = fromRational <<< Q.fromInt

fromInts :: Int -> Int -> Construct
fromInts p q = fromRational (Q.fromInts p q)

-- | The non-negative square root; `Nothing` for negative numbers.
sqrt :: Construct -> Maybe Construct
sqrt (C k a)
  | sgnK k a == LT = Nothing
  | otherwise = Just case sqrtK k a of
      Nothing -> C (Sqrt k a) (E (zeroK k) (fromRationalK k one))
      Just b -> demote k b

-- | The sign: `LT` for negative, `EQ` for zero, `GT` for positive.
sign :: Construct -> Ordering
sign (C k a) = sgnK k a

isZero :: Construct -> Boolean
isZero (C k a) = isZeroK k a

abs :: Construct -> Construct
abs x@(C k a) = if sgnK k a == LT then C k (negateK k a) else x

-- | Tower depth of the field the number lives in.
depth :: Construct -> Int
depth (C k _) = fieldDepth k

fieldOf :: Construct -> Field
fieldOf (C k _) = k

data Deconstructed
  = Rat Rational
  -- | a + b·√r with b ≠ 0 and r > 0.
  | Quad Construct Construct Construct

-- | Either a rational, or a + b√r over a simpler field. Two equal irrational
-- | numbers may deconstruct differently (it depends on the tower).
deconstruct :: Construct -> Deconstructed
deconstruct (C k0 x0) = go k0 x0
  where
  go QF (R a) = Rat a
  go (Sqrt _ _) Zero = Rat zero
  go (Sqrt k r) (E a b)
    | isZeroK k b = go k a
    | otherwise = Quad (C k a) (C k b) (C k r)
  go _ _ = shapeError "deconstruct"

toRational :: Construct -> Maybe Rational
toRational x = case deconstruct x of
  Rat q -> Just q
  Quad _ _ _ -> Nothing

-------------------------------------------------------------------------------
-- Floating-point approximation

-- | A floating-point approximation. Addition of numbers with different signs
-- | is avoided by quadratic conjugation, which keeps the result accurate.
toNumber :: Construct -> Number
toNumber (C k a) = fromRatioK k a (fromRationalK k one)

fromRatioK :: Field -> Elt -> Elt -> Number
fromRatioK QF (R a) (R b) = Q.toNumber (a / b)
fromRatioK (Sqrt _ _) Zero _ = 0.0
fromRatioK (Sqrt _ _) _ Zero = unsafeCrashWith "Constructible.Field: division by zero"
fromRatioK (Sqrt k r) (E a0 b0) (E a1 b1) =
  let
    e = fromRatioK k
    one' = fromRationalK k one
    s _ = Number.sqrt (e r one')
    -- (a0 + b0√r)/(a1 + b1√r) = (a + b√r)/n1
    a = subK k (mulK k a0 a1) (mulK k r (mulK k b0 b1))
    b = subK k (mulK k b0 a1) (mulK k a0 b1)
    n0 _ = subK k (sqK k a0) (mulK k r (sqK k b0))
    n1 _ = subK k (sqK k a1) (mulK k r (sqK k b1))
    x1 _ = e a (n1 unit) + e b (n1 unit) * s unit
    -- = n0/(a − b√r), whose terms have equal signs when a and b do not
    x2 _ = 1.0 / (e a (n0 unit) - e b (n0 unit) * s unit)
  in
    case sgnK k a, sgnK k b of
      _, EQ -> e a (n1 unit)
      EQ, _ -> e b (n1 unit) * s unit
      GT, GT -> x1 unit
      LT, LT -> x1 unit
      _, _ -> x2 unit
fromRatioK _ _ _ = shapeError "fromRatioK"

-------------------------------------------------------------------------------
-- Rendering (same format as the Haskell Show instance)

type Shows = Int -> String

paren :: Boolean -> String -> String
paren true s = "(" <> s <> ")"
paren false s = s

negateS :: Shows -> Shows
negateS s d = paren (d > 6) ("-" <> s 7)

plusS :: Shows -> Shows -> Shows
plusS s1 s2 d = paren (d > 6) (s1 6 <> " + " <> s2 7)

minusS :: Shows -> Shows -> Shows
minusS s1 s2 d = paren (d > 6) (s1 6 <> " - " <> s2 7)

timesS :: Shows -> Shows -> Shows
timesS s1 s2 d = paren (d > 7) (s1 7 <> "*" <> s2 8)

overS :: Shows -> Shows -> Shows
overS s1 s2 d = paren (d > 7) (s1 7 <> "/" <> s2 8)

sqrtS :: Shows -> Shows
sqrtS s d = paren (d > 9) ("sqrt " <> s 10)

-- | An integer-valued rational. Haskell's showsPrec for Integer parenthesises
-- | negatives above precedence 6.
integerS :: Rational -> Shows
integerS n d = paren (d > 6 && Q.signum n < 0) (Q.numeratorString n)

showsK :: Field -> Elt -> Shows
showsK QF (R x) d
  | Q.isInteger x = integerS x d
  | Q.signum x < 0 =
      negateS (overS (integerS (Q.numerator (negate x))) (integerS (Q.denominator x))) d
  | otherwise = overS (integerS (Q.numerator x)) (integerS (Q.denominator x)) d
showsK (Sqrt _ _) Zero _ = "0"
showsK (Sqrt k r) (E a b) d = case sgnK k b of
  EQ -> showsK k a d
  GT
    | isZeroK k a -> mulSqrtS k b r d
    | otherwise -> plusS (showsK k a) (mulSqrtS k b r) d
  LT
    | isZeroK k a -> negateS (mulSqrtS k (negateK k b) r) d
    | otherwise -> minusS (showsK k a) (mulSqrtS k (negateK k b) r) d
showsK _ _ _ = shapeError "showsK"

mulSqrtS :: Field -> Elt -> Elt -> Shows
mulSqrtS k b r
  | eqK k b (fromRationalK k one) = sqrtS (showsK k r)
  | otherwise = timesS (showsK k b) (sqrtS (showsK k r))

-- | The Haskell library's textual form, e.g. `1/2 + 1/2*sqrt 5`.
showConstruct :: Construct -> String
showConstruct (C k a) = showsK k a 0
