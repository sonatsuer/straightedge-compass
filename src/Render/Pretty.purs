-- | Human-readable rendering of exact numbers: Unicode text and Typst math.
-- |
-- | A number a + b·√r is shown recursively. Rational radicands are put in
-- | the form f·√s with s a square-free integer where small prime factors
-- | allow (√(3/4) is shown as ½·√3). Exact forms are always given in full;
-- | a decimal approximation can be added next to irrational numbers.
module Render.Pretty
  ( unicode
  , typst
  , decimal
  , decimalText
  , decimalTypst
  , isRational
  , withApprox
  , typstWithApprox
  , Style
  , Radicals
  , noRadicals
  , nameRadicals
  , typstNamed
  , typstNamedWithApprox
  , typstWidth
  ) where

import Prelude

import Constructible.Field (Construct, Deconstructed(..))
import Constructible.Field as C
import Constructible.Rational (Rational)
import Constructible.Rational as Q
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String

foreign import toPrecision :: Int -> Number -> String

-- | A decimal approximation with 10 significant digits.
decimal :: Construct -> String
decimal x = toPrecision 10 (C.toNumber x)

type Style =
  { rat :: Rational -> String
  , sqrt :: String -> String
  , times :: String
  , minus :: String
  , group :: String -> String
  , root :: String -> String -- the square root of a compound radicand
  }

unicodeStyle :: Style
unicodeStyle =
  { rat: \q -> minusSign (show q)
  , sqrt: \s -> "√" <> s
  , times: "·"
  , minus: "−"
  , group: \s -> "(" <> s <> ")"
  , root: \s -> "√(" <> s <> ")"
  }
  where
  minusSign s = case String.stripPrefix (String.Pattern "-") s of
    Just rest -> "−" <> rest
    Nothing -> s

typstStyle :: Style
typstStyle =
  { rat: \q ->
      if Q.isInteger q then Q.numeratorString q
      else
        let
          n = Q.numerator q
        in
          (if Q.signum n < 0 then "-" else "")
            <> "frac("
            <> Q.numeratorString (abs' n)
            <> ", "
            <> Q.denominatorString q
            <> ")"
  , sqrt: \s -> "sqrt(" <> s <> ")"
  , times: " "
  , minus: "-"
  , group: \s -> "(" <> s <> ")"
  , root: \s -> "sqrt(" <> s <> ")"
  }
  where
  abs' n = if Q.signum n < 0 then negate n else n

-- | Unicode text, e.g. `−1/16 + 1/16·√17 + …`.
unicode :: Construct -> String
unicode = render unicodeStyle

-- | Typst math (without the surrounding `$`).
typst :: Construct -> String
typst = render typstStyle

isRational :: Construct -> Boolean
isRational x = C.toRational x /= Nothing

-- | A decimal approximation for text, with the minus sign `−`.
decimalText :: Construct -> String
decimalText x = String.replaceAll (String.Pattern "-") (String.Replacement "−") (decimal x)

-- | The exact form, followed by `≈ decimal` when the number is irrational.
withApprox :: Construct -> String
withApprox x
  | isRational x = unicode x
  | otherwise = unicode x <> " ≈ " <> decimalText x

-- | A decimal as Typst math (`1.5 times 10^(-7)` rather than `1.5e-7`).
decimalTypst :: Construct -> String
decimalTypst x = case String.split (String.Pattern "e") (decimal x) of
  [ mant, ex ] -> mant <> " times 10^(" <> (fromMaybe' (String.stripPrefix (String.Pattern "+") ex) ex) <> ")"
  _ -> decimal x
  where
  fromMaybe' m d = case m of
    Just v -> v
    Nothing -> d

-- | Typst math: the exact form, then `approx decimal` when irrational.
typstWithApprox :: Construct -> String
typstWithApprox x
  | isRational x = typst x
  | otherwise = typst x <> " approx " <> decimalTypst x

-------------------------------------------------------------------------------
-- Named radicals
--
-- Typst breaks an inline equation only at its top-level operators, never
-- inside `sqrt(…)` or parentheses. The nested radicals of a deep tower (the
-- 17-gon) are wider than a line, so a compound radicand longer than
-- `radicandLimit` gets a name ρ_k, defined once where it first appears.
-- Names are keyed by the radicand's Typst text, rendered with the names
-- of the radicands inside it.

type Radicals =
  { names :: Map String String -- radicand → name
  , defs :: Array { name :: String, radicand :: String }
  }

noRadicals :: Radicals
noRadicals = { names: Map.empty, defs: [] }

-- | The approximate printed width of Typst math: the characters left after
-- | removing spaces and function names.
typstWidth :: String -> Int
typstWidth =
  String.length
    <<< String.replaceAll (String.Pattern " ") (String.Replacement "")
    <<< String.replaceAll (String.Pattern "frac(") (String.Replacement "")
    <<< String.replaceAll (String.Pattern "sqrt(") (String.Replacement "")
    <<< String.replaceAll (String.Pattern "rho_(") (String.Replacement "ρ")

radicandLimit :: Int
radicandLimit = 28

namedStyle :: Map String String -> Style
namedStyle names = typstStyle { root = \s -> fromMaybe ("sqrt(" <> s <> ")") (Map.lookup s names) }

-- | Name the long radicands of a number (innermost first) that have no
-- | name yet.
nameRadicals :: Radicals -> Construct -> Radicals
nameRadicals rs x = case C.deconstruct x of
  Rat _ -> rs
  Quad a b r ->
    let
      inner = nameRadicals (nameRadicals (nameRadicals rs a) b) r
    in
      case C.toRational r of
        Just _ -> inner
        Nothing ->
          let
            text = render (namedStyle inner.names) r
            name = "rho_(" <> show (Array.length inner.defs + 1) <> ")"
          in
            if Map.member text inner.names || typstWidth text <= radicandLimit then inner
            else
              { names: Map.insert text name inner.names
              , defs: Array.snoc inner.defs { name, radicand: text }
              }

-- | Typst math with the given names for radicands.
typstNamed :: Map String String -> Construct -> String
typstNamed names = render (namedStyle names)

typstNamedWithApprox :: Map String String -> Construct -> String
typstNamedWithApprox names x
  | isRational x = typstNamed names x
  | otherwise = typstNamed names x <> " approx " <> decimalTypst x

-------------------------------------------------------------------------------

data Term = Term
  { negative :: Boolean
  , text :: String -- magnitude, already rendered
  , atomic :: Boolean -- can be multiplied without parentheses
  }

render :: Style -> Construct -> String
render st x = joinTerms st (terms st x)

joinTerms :: Style -> Array Term -> String
joinTerms st ts = case Array.uncons ts of
  Nothing -> "0"
  Just { head: Term h, tail } ->
    (if h.negative then st.minus else "") <> h.text
      <> foldTerms tail
  where
  foldTerms rest = case Array.uncons rest of
    Nothing -> ""
    Just { head: Term t, tail } ->
      (if t.negative then " " <> st.minus <> " " else " + ") <> t.text <> foldTerms tail

-- | The terms of a + b√r: those of a, then b·√r (as one term).
terms :: Style -> Construct -> Array Term
terms st x = case C.deconstruct x of
  Rat q
    | Q.isZero q -> []
    | otherwise -> [ ratTerm q ]
  Quad a b r ->
    terms st a <> [ radicalTerm st b r ]
  where
  ratTerm q =
    let
      neg = Q.signum q < 0
      m = if neg then negate q else q
    in
      Term { negative: neg, text: st.rat m, atomic: Q.isInteger m }

-- | b·√r, normalising a rational radicand and pulling the sign out of b.
radicalTerm :: Style -> Construct -> Construct -> Term
radicalTerm st b r = case C.toRational r of
  Just q ->
    let
      s = squareFree q
      coeff = b * C.fromRational s.factor
    in
      scaled coeff (st.sqrt (Q.numeratorString s.free))
  Nothing ->
    scaled b (st.root (render st r))
  where
  scaled coeff root =
    let
      neg = C.sign coeff == LT
      mag = if neg then negate coeff else coeff
    in
      if mag == one then Term { negative: neg, text: root, atomic: true }
      else case terms st mag of
        [ Term t ] | t.atomic || C.toRational mag /= Nothing ->
          Term { negative: neg, text: t.text <> st.times <> root, atomic: false }
        ts -> Term { negative: neg, text: st.group (joinTerms st ts) <> st.times <> root, atomic: false }

-- | √q = factor · √free, with `free` a positive integer that has no square
-- | factor among the primes tried.
squareFree :: Rational -> { factor :: Rational, free :: Rational }
squareFree q =
  let
    -- √(n/d) = √(n·d)/d
    nd = Q.numerator q * Q.denominator q
    split = Q.extractSquares nd
  in
    { factor: split.out / Q.denominator q, free: split.rest }

