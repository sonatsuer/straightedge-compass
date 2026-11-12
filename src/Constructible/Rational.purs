-- | Exact rational numbers, backed by the JavaScript `BigInt` type.
module Constructible.Rational
  ( Rational
  , fromInt
  , fromInts
  , parse
  , exactSqrt
  , floor
  , isInteger
  , isZero
  , signum
  , pow
  , numerator
  , denominator
  , numeratorString
  , denominatorString
  , toNumber
  , toPair
  , extractSquares
  , gcdInt
  , lcmInt
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)

foreign import data Rational :: Type

foreign import zeroR :: Rational
foreign import oneR :: Rational
foreign import fromInt :: Int -> Rational
foreign import fromIntsImpl :: Int -> Int -> Rational
foreign import parseImpl :: String -> Nullable Rational
foreign import addR :: Rational -> Rational -> Rational
foreign import subR :: Rational -> Rational -> Rational
foreign import mulR :: Rational -> Rational -> Rational
foreign import negateR :: Rational -> Rational
foreign import recipR :: Rational -> Rational
foreign import divR :: Rational -> Rational -> Rational
foreign import eqR :: Rational -> Rational -> Boolean
foreign import compareImpl :: Rational -> Rational -> Int
foreign import signum :: Rational -> Int
foreign import isZero :: Rational -> Boolean
foreign import isInteger :: Rational -> Boolean
foreign import exactSqrtImpl :: Rational -> Nullable Rational
foreign import floor :: Rational -> Rational
foreign import toNumber :: Rational -> Number
foreign import numerator :: Rational -> Rational
foreign import denominator :: Rational -> Rational
foreign import numeratorString :: Rational -> String
foreign import denominatorString :: Rational -> String
foreign import toString :: Rational -> String
foreign import powImpl :: Rational -> Int -> Rational
foreign import toPair :: Rational -> { num :: String, den :: String }

-- | For a positive integer n: n = out² · rest, where rest has no square
-- | factor among the primes below 10⁴.
foreign import extractSquares :: Rational -> { out :: Rational, rest :: Rational }

-- | The gcd of two integers (as rationals with denominator 1).
foreign import gcdInt :: Rational -> Rational -> Rational

-- | The lcm of two positive integers.
lcmInt :: Rational -> Rational -> Rational
lcmInt a b = a * b / gcdInt a b

-- | `fromInts p q` is p/q. Throws on q = 0.
fromInts :: Int -> Int -> Rational
fromInts = fromIntsImpl

-- | Parses `p`, `-p` or `p/q`.
parse :: String -> Maybe Rational
parse = toMaybe <<< parseImpl

-- | The square root, when it is rational.
exactSqrt :: Rational -> Maybe Rational
exactSqrt = toMaybe <<< exactSqrtImpl

-- | Integer power; negative exponents take the reciprocal.
pow :: Rational -> Int -> Rational
pow = powImpl

instance Eq Rational where
  eq = eqR

instance Ord Rational where
  compare a b = case compareImpl a b of
    -1 -> LT
    0 -> EQ
    _ -> GT

instance Show Rational where
  show = toString

instance Semiring Rational where
  add = addR
  mul = mulR
  zero = zeroR
  one = oneR

instance Ring Rational where
  sub = subR

instance CommutativeRing Rational

instance DivisionRing Rational where
  recip = recipR

instance EuclideanRing Rational where
  degree _ = 1
  div = divR
  mod _ _ = zeroR
