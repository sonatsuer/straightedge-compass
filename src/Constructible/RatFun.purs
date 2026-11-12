-- | Rational functions ℚ(x₀, x₁, …) as reduced fractions of polynomials.
-- |
-- | Invariant: gcd(num, den) = 1 and `den` is primitive, integral, with a
-- | positive leading coefficient, so equal functions have equal fractions.
module Constructible.RatFun
  ( RatFun
  , fromPoly
  , fromRational
  , fromInt
  , var
  , num
  , den
  , isZero
  , toRational
  , toPoly
  , sqrt
  , evalAt
  , substFrac
  , vars
  , key
  ) where

import Prelude

import Constructible.Poly (Poly, Var)
import Constructible.Poly as P
import Constructible.Rational (Rational)
import Constructible.Rational as Q
import Data.Array as Array
import Data.Maybe (Maybe(..))

newtype RatFun = RatFun { num :: Poly, den :: Poly }

-- | Reduces num/den (den ≠ 0) to the canonical form.
make :: Poly -> Poly -> RatFun
make n d
  | P.isZero n = RatFun { num: zero, den: one }
  | otherwise =
      case P.constValue d of
        Just c -> RatFun { num: P.scale (recip c) n, den: one }
        Nothing ->
          let
            g = P.gcd n d
            n1 = if g == one then n else unsafeDiv n g
            d1 = if g == one then d else unsafeDiv d g
            nd = P.normalise d1
          in
            RatFun { num: P.scale (recip nd.c) n1, den: nd.p }
      where
      unsafeDiv a b = case P.divExact a b of
        Just q -> q
        Nothing -> a

fromPoly :: Poly -> RatFun
fromPoly p = RatFun { num: p, den: one }

fromRational :: Rational -> RatFun
fromRational = fromPoly <<< P.constant

fromInt :: Int -> RatFun
fromInt = fromRational <<< Q.fromInt

var :: Var -> RatFun
var = fromPoly <<< P.var

num :: RatFun -> Poly
num (RatFun f) = f.num

den :: RatFun -> Poly
den (RatFun f) = f.den

isZero :: RatFun -> Boolean
isZero (RatFun f) = P.isZero f.num

toRational :: RatFun -> Maybe Rational
toRational (RatFun f) = if f.den == one then P.constValue f.num else Nothing

-- | The function as a polynomial, when its denominator is 1.
toPoly :: RatFun -> Maybe Poly
toPoly (RatFun f) = if f.den == one then Just f.num else Nothing

-- | A square root in ℚ(x), if there is one: √(n/d) = √(n·d)/d. The root has a
-- | positive leading coefficient; which sign is the non-negative root on a
-- | given domain is for the caller to decide.
sqrt :: RatFun -> Maybe RatFun
sqrt (RatFun f) = (\s -> make s f.den) <$> P.sqrt (f.num * f.den)

-- | The value at a point, unless the denominator vanishes there.
evalAt :: RatFun -> Array (Maybe Rational) -> Maybe Rational
evalAt (RatFun f) vals = do
  n <- P.evalAt f.num vals
  d <- P.evalAt f.den vals
  if Q.isZero d then Nothing else Just (n / d)

-- | Substitutes N/D for the variable v.
substFrac :: RatFun -> Var -> Poly -> Poly -> RatFun
substFrac (RatFun f) v n d =
  let
    a = P.substFrac f.num v n d
    b = P.substFrac f.den v n d
    -- a.poly / D^ka divided by b.poly / D^kb
    k = a.degree - b.degree
  in
    if k >= 0 then make a.poly (b.poly * powP d k)
    else make (a.poly * powP d (negate k)) b.poly
  where
  powP p e = Array.foldl (\acc _ -> acc * p) one (Array.replicate e unit)

vars :: RatFun -> Array Var
vars (RatFun f) = Array.nub (P.vars f.num <> P.vars f.den)

key :: RatFun -> String
key (RatFun f) = P.key f.num <> "/" <> P.key f.den

instance Eq RatFun where
  eq (RatFun a) (RatFun b) = a.num == b.num && a.den == b.den

instance Show RatFun where
  show = key

instance Semiring RatFun where
  add (RatFun a) (RatFun b)
    | a.den == b.den = make (a.num + b.num) a.den
    | otherwise = make (a.num * b.den + b.num * a.den) (a.den * b.den)
  mul (RatFun a) (RatFun b) = make (a.num * b.num) (a.den * b.den)
  zero = fromPoly zero
  one = fromPoly one

instance Ring RatFun where
  sub a b = a + negate' b
    where
    negate' (RatFun f) = RatFun { num: zero - f.num, den: f.den }

instance CommutativeRing RatFun

instance DivisionRing RatFun where
  recip (RatFun f) = make f.den f.num

instance EuclideanRing RatFun where
  degree _ = 1
  div a b = a * recip b
  mod _ _ = zero
