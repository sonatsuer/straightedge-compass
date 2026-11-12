-- | Printing polynomials, rational functions and towers over ℚ(x), with
-- | variable names: Unicode text for the page and messages, Typst math for
-- | the PDF.
module Render.Sym
  ( Names
  , polyText
  , ratText
  , pcText
  , polyTypst
  , ratTypst
  , pcTypst
  , polyLean
  , ratLean
  , relText
  , valuationText
  ) where

import Prelude

import Constructible.ParamField (PC, PDecon(..), deconstruct, neg, toRatFun)
import Constructible.Poly (Poly, Term)
import Constructible.Poly as P
import Constructible.RatFun (RatFun)
import Constructible.RatFun as RF
import Constructible.Rational (Rational)
import Constructible.Rational as Q
import Data.Array as Array
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String as String
import Proof.Signs (Rel(..), Valuation)

-- | Display names of the variables, by index (Unicode, Typst).
type Names = { text :: Int -> String, typst :: Int -> String }

type Style =
  { var :: Int -> String
  , pow :: String -> Int -> String
  , times :: String
  , minus :: String
  , rat :: Rational -> String
  , frac :: String -> String -> String
  , sqrt :: String -> String
  }

textStyle :: Names -> Style
textStyle names =
  { var: names.text
  , pow: \v e -> v <> superscript e
  , times: ""
  , minus: "−"
  , rat: \q -> String.replaceAll (String.Pattern "-") (String.Replacement "−") (show q)
  , frac: \n d -> paren n <> "/" <> paren d
  , sqrt: \s -> "√(" <> s <> ")"
  }
  where
  paren s = if String.contains (String.Pattern " ") s then "(" <> s <> ")" else s

typstStyle :: Names -> Style
typstStyle names =
  { var: names.typst
  , pow: \v e -> v <> "^" <> show e
  , times: " "
  , minus: "-"
  , rat: \q ->
      if Q.isInteger q then Q.numeratorString q
      else "frac(" <> Q.numeratorString q <> ", " <> Q.denominatorString q <> ")"
  , frac: \n d -> "frac(" <> n <> ", " <> d <> ")"
  , sqrt: \s -> "sqrt(" <> s <> ")"
  }

-- | Lean 4 syntax over ℝ, with the given variable names.
leanStyle :: (Int -> String) -> Style
leanStyle var =
  { var
  , pow: \v e -> v <> " ^ " <> show e
  , times: " * "
  , minus: "-"
  , rat: \q ->
      if Q.isInteger q then Q.numeratorString q
      else "(" <> Q.numeratorString q <> " / " <> Q.denominatorString q <> ")"
  , frac: \n d -> "(" <> n <> ") / (" <> d <> ")"
  , sqrt: \x -> "√(" <> x <> ")"
  }

polyLean :: (Int -> String) -> Poly -> String
polyLean var = polyWith (leanStyle var)

ratLean :: (Int -> String) -> RatFun -> String
ratLean var = ratWith (leanStyle var)

superscript :: Int -> String
superscript e = joinWith "" (map digit (String.split (String.Pattern "") (show e)))
  where
  digit d = case d of
    "0" -> "⁰"
    "1" -> "¹"
    "2" -> "²"
    "3" -> "³"
    "4" -> "⁴"
    "5" -> "⁵"
    "6" -> "⁶"
    "7" -> "⁷"
    "8" -> "⁸"
    "9" -> "⁹"
    other -> other

-- | Terms by total degree (highest first), then as Poly orders them.
ordered :: Poly -> Array Term
ordered p = Array.sortBy (\a b -> compare (deg b) (deg a)) (P.terms p)
  where
  deg t = sum t.exps

monomial :: Style -> Array Int -> String
monomial st exps = joinWith st.times (Array.catMaybes (Array.mapWithIndex factor exps))
  where
  factor v e
    | e == 0 = Nothing
    | e == 1 = Just (st.var v)
    | otherwise = Just (st.pow (st.var v) e)

polyWith :: Style -> Poly -> String
polyWith st p = case Array.uncons (ordered p) of
  Nothing -> "0"
  Just { head, tail } -> term true head <> joinWith "" (map (term false) tail)
  where
  term first t =
    let
      neg = Q.signum t.coeff < 0
      c = if neg then negate t.coeff else t.coeff
      m = monomial st t.exps
      body
        | m == "" = st.rat c
        | c == one = m
        | otherwise = st.rat c <> (if st.times == "" then "" else st.times) <> m
      sep
        | first = if neg then st.minus else ""
        | otherwise = if neg then " " <> st.minus <> " " else " + "
    in
      sep <> body

ratWith :: Style -> RatFun -> String
ratWith st f = case RF.toPoly f of
  Just p -> polyWith st p
  Nothing -> st.frac (polyWith st (RF.num f)) (polyWith st (RF.den f))

pcWith :: Style -> PC -> String
pcWith st x = case deconstruct x of
  PBase f -> ratWith st f
  PQuad a b r ->
    let
      root = st.sqrt (pcWith st r)
      negative = case toRatFun b >>= RF.toRational of
        Just q -> Q.signum q < 0
        Nothing -> false
      bs = pcWith st (if negative then neg b else b)
      coeff = if bs == "1" then "" else if isAtomic bs then bs <> st.times else "(" <> bs <> ")" <> st.times
      as = pcWith st a
    in
      if as == "0" then (if negative then st.minus else "") <> coeff <> root
      else as <> (if negative then " " <> st.minus <> " " else " + ") <> coeff <> root
  where
  isAtomic str = not (String.contains (String.Pattern " ") str)

polyText :: Names -> Poly -> String
polyText n = polyWith (textStyle n)

ratText :: Names -> RatFun -> String
ratText n = ratWith (textStyle n)

pcText :: Names -> PC -> String
pcText n = pcWith (textStyle n)

polyTypst :: Names -> Poly -> String
polyTypst n = polyWith (typstStyle n)

ratTypst :: Names -> RatFun -> String
ratTypst n = ratWith (typstStyle n)

pcTypst :: Names -> PC -> String
pcTypst n = pcWith (typstStyle n)

relText :: Rel -> String
relText = case _ of
  RPos -> "> 0"
  RNeg -> "< 0"
  RNonZero -> "≠ 0"
  RNonNeg -> "≥ 0"
  RNonPos -> "≤ 0"

-- | `r = 1/2, t_P = 3`.
valuationText :: Names -> Valuation -> String
valuationText n vals =
  joinWith ", "
    ( Array.catMaybes
        ( Array.mapWithIndex
            (\i v -> v <#> \q -> n.text i <> " = " <> String.replaceAll (String.Pattern "-") (String.Replacement "−") (show q))
            vals
        )
    )
