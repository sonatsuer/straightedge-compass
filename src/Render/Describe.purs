-- | Descriptions of objects with exact values: Unicode text for the page and
-- | the console, Typst math for the PDF. Irrational values are followed by a
-- | decimal approximation; the exact form is always given in full.
module Render.Describe
  ( describeObj
  , pointText
  , lineText
  , circleText
  , describeTypst
  , pointTypst
  , lineTypst
  , circleTypst
  , objValues
  ) where

import Prelude

import Constructible.Field (Construct)
import Constructible.Field as C
import Data.Foldable (all)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Geometry (Circle, Line, Point)
import Geometry as G
import Proof.Object (Obj(..))
import Render.Pretty as Pretty

-------------------------------------------------------------------------------
-- Unicode

pointText :: Point -> String
pointText p =
  "(" <> Pretty.unicode p.x <> ", " <> Pretty.unicode p.y <> ")"
    <> approxSuffix [ p.x, p.y ] ("(" <> Pretty.decimalText p.x <> ", " <> Pretty.decimalText p.y <> ")")

-- | ` ≈ text` when one of the values is irrational.
approxSuffix :: Array Construct -> String -> String
approxSuffix xs text = if all Pretty.isRational xs then "" else " ≈ " <> text

-- | The coefficients of a line as `x = c` or `y = m·x + c`.
lineForm :: Line -> { vertical :: Boolean, m :: Construct, c :: Construct }
lineForm l =
  let
    k = G.lineCoefficients l
  in
    if C.isZero k.b then { vertical: true, m: zero, c: negate k.c / k.a }
    else { vertical: false, m: negate k.a / k.b, c: negate k.c / k.b }

-- | An equation with the numbers rendered by `num`, signs handled here. A
-- | compound slope is put in parentheses before `x`.
equation
  :: { num :: Construct -> String, times :: String, minus :: String }
  -> { vertical :: Boolean, m :: Construct, c :: Construct }
  -> String
equation r f
  | f.vertical = "x = " <> r.num f.c
  | C.isZero f.m = "y = " <> r.num f.c
  | otherwise =
      "y = " <> slope <> "x" <> constant
      where
      slope
        | f.m == one = ""
        | f.m == negate one = r.minus
        | Pretty.isRational f.m = r.num f.m <> r.times
        | otherwise = "(" <> r.num f.m <> ")" <> r.times
      constant
        | C.isZero f.c = ""
        | C.sign f.c == LT = " " <> r.minus <> " " <> r.num (negate f.c)
        | otherwise = " + " <> r.num f.c

lineText :: Line -> String
lineText l =
  let
    f = lineForm l
  in
    equation { num: Pretty.unicode, times: "·", minus: "−" } f
      <> approxSuffix [ f.m, f.c ] (equation { num: Pretty.decimalText, times: "·", minus: "−" } f)

radius :: Circle -> Construct
radius c = case C.sqrt (G.circleRadius2 c) of
  Just r -> r
  Nothing -> zero

circleText :: Circle -> String
circleText c =
  let
    o = G.circleCenter c
    r = radius c
  in
    "centre (" <> Pretty.unicode o.x <> ", " <> Pretty.unicode o.y <> "), radius " <> Pretty.unicode r
      <> approxSuffix [ o.x, o.y, r ]
        ( "centre (" <> Pretty.decimalText o.x <> ", " <> Pretty.decimalText o.y <> "), radius "
            <> Pretty.decimalText r
        )

describeObj :: Obj -> String
describeObj = case _ of
  OPoint p -> pointText p
  OLine l -> lineText l
  OCircle c -> circleText c

-------------------------------------------------------------------------------
-- Typst markup (with `$…$`). Radicands named in `names` are written by name
-- (see `Pretty.nameRadicals`). A pair of long coordinates is split into
-- `x = …`, `y = …` so that the line can break between them.

-- | The numbers an object description shows, for naming radicands.
objValues :: Obj -> Array Construct
objValues = case _ of
  OPoint p -> [ p.x, p.y ]
  OLine l -> let f = lineForm l in [ f.m, f.c ]
  OCircle c -> let o = G.circleCenter c in [ o.x, o.y, radius c ]

-- | ` approx text` when one of the values is irrational.
approxTypst :: Array Construct -> String -> String
approxTypst xs text = if all Pretty.isRational xs then "" else " approx " <> text

pairWidth :: Int
pairWidth = 60

-- | A pair of coordinates, with its decimal approximation.
pairTypst :: Map String String -> Construct -> Construct -> String
pairTypst names x y =
  let
    xs = Pretty.typstNamed names x
    ys = Pretty.typstNamed names y
    approx = approxTypst [ x, y ] ("(" <> Pretty.decimalTypst x <> ", " <> Pretty.decimalTypst y <> ")")
  in
    if Pretty.typstWidth xs + Pretty.typstWidth ys <= pairWidth then "$(" <> xs <> ", " <> ys <> ")" <> approx <> "$"
    else "$x = " <> xs <> "$, $y = " <> ys <> "$" <> (if approx == "" then "" else ", $(x, y)" <> approx <> "$")

pointTypst :: Map String String -> Point -> String
pointTypst names p = pairTypst names p.x p.y

lineTypst :: Map String String -> Line -> String
lineTypst names l =
  let
    f = lineForm l
  in
    "$" <> equation { num: Pretty.typstNamed names, times: " ", minus: "-" } f
      <> approxTypst [ f.m, f.c ] ("(" <> equation { num: Pretty.decimalTypst, times: " ", minus: "-" } f <> ")")
      <> "$"

circleTypst :: Map String String -> Circle -> String
circleTypst names c =
  let
    o = G.circleCenter c
  in
    "centre " <> pairTypst names o.x o.y <> ", radius $" <> Pretty.typstNamedWithApprox names (radius c) <> "$"

describeTypst :: Map String String -> Obj -> String
describeTypst names = case _ of
  OPoint p -> pointTypst names p
  OLine l -> lineTypst names l
  OCircle c -> circleTypst names c
