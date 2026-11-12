-- | Sparse multivariate polynomials with rational coefficients, in the
-- | variables 0, 1, 2, … (JavaScript `BigInt` underneath).
-- |
-- | Monomials are ordered lexicographically with the highest variable most
-- | significant; the leading coefficient is the coefficient of the largest
-- | monomial. `gcd` returns a primitive integral polynomial with a positive
-- | leading coefficient.
module Constructible.Poly
  ( Poly
  , Var
  , Term
  , constant
  , var
  , scale
  , pow
  , fromInt
  , isZero
  , constValue
  , vars
  , degIn
  , totalDeg
  , coeffsIn
  , fromCoeffsIn
  , terms
  , leadingCoeff
  , size
  , substRats
  , substFrac
  , evalAt
  , deriv
  , divExact
  , gcd
  , normalise
  , sqrt
  , QuadSign(..)
  , quadSign
  , quadMin
  , ldl
  , ldlOrder
  , rename
  , Uni
  , toUni
  , Cell
  , cells
  , ratRoots
  , key
  ) where

import Prelude

import Constructible.Rational (Rational)
import Constructible.Rational as Q
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)

foreign import data Poly :: Type

type Var = Int

-- | A term: the exponents by variable, and the coefficient.
type Term = { exps :: Array Int, coeff :: Rational }

foreign import zeroP :: Poly
foreign import constP :: Rational -> Poly
foreign import varP :: Int -> Poly
foreign import addP :: Poly -> Poly -> Poly
foreign import subP :: Poly -> Poly -> Poly
foreign import negP :: Poly -> Poly
foreign import mulP :: Poly -> Poly -> Poly
foreign import scaleP :: Rational -> Poly -> Poly
foreign import powP :: Poly -> Int -> Poly
foreign import eqP :: Poly -> Poly -> Boolean
foreign import isZeroP :: Poly -> Boolean
foreign import constValueImpl :: Poly -> Nullable Rational
foreign import varsP :: Poly -> Array Int
foreign import degInP :: Poly -> Int -> Int
foreign import totalDegP :: Poly -> Int
foreign import coeffsInP :: Poly -> Int -> Array Poly
foreign import fromCoeffsInP :: Array Poly -> Int -> Poly
foreign import termsP :: Poly -> Array Term
foreign import leadingCoeffP :: Poly -> Rational
foreign import sizeP :: Poly -> Int
foreign import substRatsP :: Poly -> Array (Nullable Rational) -> Poly
foreign import substFracP :: Poly -> Int -> Poly -> Poly -> { poly :: Poly, degree :: Int }
foreign import derivP :: Poly -> Int -> Poly
foreign import divExactImpl :: Poly -> Poly -> Nullable Poly
foreign import gcdP :: Poly -> Poly -> Poly
foreign import normaliseP :: Poly -> { c :: Rational, p :: Poly }
foreign import sqrtImpl :: Poly -> Nullable Poly
foreign import quadSignImpl :: Poly -> Nullable String
foreign import keyP :: Poly -> String
foreign import quadMinImpl :: Poly -> Nullable { vars :: Array Int, point :: Array Rational, value :: Rational }
foreign import ldlImpl :: Poly -> Nullable (Array { d :: Rational, l :: Poly })
foreign import ldlOrderImpl :: Poly -> Array Int -> Nullable (Array { d :: Rational, l :: Poly })
foreign import renameP :: Poly -> Array Int -> Poly
foreign import toUniImpl :: Poly -> Int -> Nullable Uni
foreign import cellsP :: Array Uni -> Array CellRaw
foreign import ratRootsImpl :: Uni -> Nullable (Array Rational)
foreign import nullR :: Nullable Rational
foreign import someR :: Rational -> Nullable Rational

-- | A univariate polynomial (coefficients, lowest degree first).
foreign import data Uni :: Type

type CellRaw = { kind :: String, sample :: Nullable Rational, signs :: Array Int }

-- | A cell of ℝ for a family of univariate polynomials: a common root or an
-- | open interval between roots, with the sign of every polynomial on it.
-- | `sample` is a rational point of the cell (absent for an irrational root).
type Cell = { root :: Boolean, sample :: Maybe Rational, signs :: Array Int }

instance Eq Poly where
  eq = eqP

instance Show Poly where
  show = keyP

instance Semiring Poly where
  add = addP
  mul = mulP
  zero = zeroP
  one = constP one

instance Ring Poly where
  sub = subP

instance CommutativeRing Poly

constant :: Rational -> Poly
constant = constP

var :: Var -> Poly
var = varP

scale :: Rational -> Poly -> Poly
scale = scaleP

pow :: Poly -> Int -> Poly
pow = powP

fromInt :: Int -> Poly
fromInt = constP <<< Q.fromInt

isZero :: Poly -> Boolean
isZero = isZeroP

-- | The value of a constant polynomial.
constValue :: Poly -> Maybe Rational
constValue = toMaybe <<< constValueImpl

vars :: Poly -> Array Var
vars = varsP

degIn :: Poly -> Var -> Int
degIn = degInP

totalDeg :: Poly -> Int
totalDeg = totalDegP

-- | The coefficients as a polynomial in one variable (index = degree).
coeffsIn :: Poly -> Var -> Array Poly
coeffsIn = coeffsInP

fromCoeffsIn :: Array Poly -> Var -> Poly
fromCoeffsIn = fromCoeffsInP

-- | Terms, largest monomial first.
terms :: Poly -> Array Term
terms = termsP

leadingCoeff :: Poly -> Rational
leadingCoeff = leadingCoeffP

size :: Poly -> Int
size = sizeP

-- | Substitutes rationals for the variables that have a value.
substRats :: Poly -> Array (Maybe Rational) -> Poly
substRats p vals = substRatsP p
  ( map
      ( case _ of
          Just r -> someR r
          Nothing -> nullR
      )
      vals
  )

-- | `D^k · p(v := N/D)` with k the degree of p in v.
substFrac :: Poly -> Var -> Poly -> Poly -> { poly :: Poly, degree :: Int }
substFrac = substFracP

-- | The value at a point (every variable of p must have a value).
evalAt :: Poly -> Array (Maybe Rational) -> Maybe Rational
evalAt p vals = constValue (substRats p vals)

deriv :: Poly -> Var -> Poly
deriv = derivP

divExact :: Poly -> Poly -> Maybe Poly
divExact a b = toMaybe (divExactImpl a b)

gcd :: Poly -> Poly -> Poly
gcd = gcdP

-- | p = c · q with q primitive, integral, with a positive leading coefficient.
normalise :: Poly -> { c :: Rational, p :: Poly }
normalise = normaliseP

-- | The square root with a positive leading coefficient, if p is a square.
sqrt :: Poly -> Maybe Poly
sqrt = toMaybe <<< sqrtImpl

-- | The sign of a polynomial of total degree at most 2 on all of ℝⁿ.
data QuadSign = PosDef | PosSemi | NegDef | NegSemi | Indefinite

derive instance Eq QuadSign

quadSign :: Poly -> Maybe QuadSign
quadSign p = toMaybe (quadSignImpl p) >>= case _ of
  "pd" -> Just PosDef
  "psd" -> Just PosSemi
  "nd" -> Just NegDef
  "nsd" -> Just NegSemi
  "indef" -> Just Indefinite
  _ -> Nothing

-- | For a quadratic with a nonsingular quadratic part: the point where its
-- | gradient vanishes (its only zero, if it is semidefinite), and the value
-- | there.
quadMin :: Poly -> Maybe { vars :: Array Var, point :: Array Rational, value :: Rational }
quadMin = toMaybe <<< quadMinImpl

-- | A positive semidefinite quadratic as `Σ dᵢ ℓᵢ²` with `dᵢ > 0` and
-- | linear forms `ℓᵢ`; Nothing if it is not one.
ldl :: Poly -> Maybe (Array { d :: Rational, l :: Poly })
ldl = toMaybe <<< ldlImpl

-- | `ldl` with the variables in the given order (then the constant): the
-- | last variable's linear form is that variable plus a constant.
ldlOrder :: Poly -> Array Var -> Maybe (Array { d :: Rational, l :: Poly })
ldlOrder p order = toMaybe (ldlOrderImpl p order)

-- | Renames the variables: variable i becomes `new[i]` (every variable of p
-- | needs a new name).
rename :: Poly -> Array Var -> Poly
rename = renameP

-- | The polynomial in its only variable `v` (Nothing if others occur).
toUni :: Poly -> Var -> Maybe Uni
toUni p v = toMaybe (toUniImpl p v)

cells :: Array Uni -> Array Cell
cells ps = map (\c -> { root: c.kind == "root", sample: toMaybe c.sample, signs: c.signs }) (cellsP ps)

-- | The rational roots (Nothing when the leading coefficient is too large
-- | to enumerate its divisors).
ratRoots :: Uni -> Maybe (Array Rational)
ratRoots u = toMaybe (ratRootsImpl u)

-- | A canonical string for the polynomial.
key :: Poly -> String
key = keyP
