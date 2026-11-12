-- | The built-in constructions, written in the script language itself, so
-- | that each one reduces to straightedge-and-compass primitives.
module Proof.Prelude (source, macros) where

import Prelude

import Data.Either (Either(..))
import Partial.Unsafe (unsafeCrashWith)
import Proof.Parser (parseScript)
import Proof.Syntax (Macro)

source :: String
source =
  """
-- The perpendicular bisector of AB (A ≠ B).
def perpBisector(A, B) -> l:
  ca = circle A B
  cb = circle B A
  P, Q = meet ca cb
  l = line P Q
end

-- The midpoint of AB (Euclid I.10).
def midpoint(A, B) -> M:
  l = perpBisector(A, B)
  ab = line A B
  M = meet l ab
end

-- The circle with diameter AB.
def circleDiameter(A, B) -> c:
  M = midpoint(A, B)
  c = circle M A
end

-- The perpendicular to the line AB at its point P, P ≠ A (Euclid I.11).
def perpAt(A, B, P) -> l:
  ab = line A B
  assert on P ab
  c = circle P A
  X = meet ab c except A
  l = perpBisector(A, X)
end

-- The perpendicular from P to the line AB, P not on AB (Euclid I.12).
def perpFrom(A, B, P) -> l:
  ca = circle A P
  cb = circle B P
  Q = meet ca cb except P
  l = line P Q
end

-- The parallel to AB through P, P not on AB: with M the midpoint of PB and
-- Q the reflection of A in M, APQB is a parallelogram.
def parallelThrough(A, B, P) -> l:
  M = midpoint(P, B)
  am = line A M
  c = circle M A
  Q = meet am c except A
  l = line P Q
end

-- The bisector of the angle AOB (Euclid I.9).
def angleBisector(A, O, B) -> l:
  c = circle O A
  ob = line O B
  B1 = meet ob c nearest B
  l = perpBisector(A, B1)
end

-- The circle with centre O and radius AB, O ≠ A (Euclid I.2: a compass
-- that collapses when lifted can still transfer lengths).
def transfer(O, A, B) -> c:
  c1 = circle O A
  c2 = circle A O
  D = meet c1 c2 left O A
  da = line D A
  cab = circle A B
  E = meet da cab farthest D
  cde = circle D E
  dO = line D O
  F = meet dO cde nearest O
  c = circle O F
end
"""

macros :: Array Macro
macros = case parseScript source of
  Right s -> s.macros
  Left e -> unsafeCrashWith ("Proof.Prelude: line " <> show e.line <> ": " <> e.message)
