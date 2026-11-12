module Test.Proof (spec) where

import Prelude

import Constructible.Field as C
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (all)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.String (joinWith)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Geometry as G
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Proof.Eval (Trace, evaluateSource, isGeneral, pages)
import Proof.Object (Obj(..))
import Proof.Parser (parseScript)
import Proof.Predicates as P
import Proof.Syntax (Stmt(..))
import Render.Pretty as Pretty
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

run :: String -> Trace
run src = case evaluateSource src of
  Right t -> t
  Left e -> unsafeCrashWith ("parse error at line " <> show e.line <> ": " <> e.message)

-- | "ok", the evaluation error with its line, or the parse error position.
outcome :: String -> String
outcome src = case evaluateSource src of
  Left e -> "parse error at " <> show e.line <> ":" <> show e.column
  Right t -> case t.error of
    Nothing -> "ok"
    Just e -> "line " <> show e.line <> ": " <> e.message

script :: Array String -> String
script = joinWith "\n"

object :: Trace -> String -> Obj
object t n = case Map.lookup n t.scope >>= \i -> Map.lookup i t.objects of
  Just info -> info.obj
  Nothing -> unsafeCrashWith (n <> " is not defined")

pointOf :: Trace -> String -> G.Point
pointOf t n = case object t n of
  OPoint p -> p
  _ -> unsafeCrashWith (n <> " is not a point")

circleOf :: Trace -> String -> G.Circle
circleOf t n = case object t n of
  OCircle c -> c
  _ -> unsafeCrashWith (n <> " is not a circle")

sqrtC :: Int -> C.Construct
sqrtC k = unsafePartial (fromJust (C.sqrt (C.fromInt k)))

-- | Compares statements by their note text.
shouldEqual' :: Array Stmt -> Array String -> Aff Unit
shouldEqual' stmts expected = map noteText stmts `shouldEqual` expected
  where
  noteText = case _ of
    Note n -> n
    _ -> "(not a note)"

checks :: Array String -> Aff Unit
checks lines = outcome (script lines) `shouldEqual` "ok"

spec :: Spec Unit
spec = describe "Proof scripts" do

  describe "examples" do
    it "Euclid I.1 checks" do
      src <- liftEffect (readTextFile UTF8 "examples/euclid-i-1/euclid-i-1.sc")
      let
        t = run src
        c = pointOf t "C"
      map _.message t.error `shouldEqual` Nothing
      Array.length t.asserts `shouldEqual` 2
      isGeneral t `shouldEqual` true
      [ c.x, c.y ] `shouldEqual` [ C.fromInts 1 2, sqrtC 3 / C.fromInt 2 ]

    it "the 17-gon checks" do
      src <- liftEffect (readTextFile UTF8 "examples/heptadecagon/heptadecagon.sc")
      let
        t = run src
        p1 = pointOf t "P1"
      map _.message t.error `shouldEqual` Nothing
      -- 32 in the script, plus one precondition in each of the 3 perpAt calls
      Array.length (Array.filter (\a -> Array.null a.context) t.asserts) `shouldEqual` 32
      Array.length t.asserts `shouldEqual` 35
      all (\s -> all _.holds s.verdicts) t.steps `shouldEqual` true
      -- cos(2π/17) ≈ 0.9324722294043558
      Pretty.decimal p1.x `shouldEqual` "0.9324722294"

  describe "errors are reported at their line" do
    it "a false assertion" do
      outcome (script [ "given A = (0, 0)", "given B = (1, 0)", "given C = (0, 2)", "assert dist A B == dist A C" ])
        `shouldEqual` "line 4: assertion failed: dist A B == dist A C (|AB| = 1, |AC| = 2)"
    it "an undefined name" do
      outcome (script [ "given A = (0, 0)", "l = line A B" ]) `shouldEqual` "line 2: B is not defined"
    it "a wrong kind of object" do
      outcome (script [ "given A = (0, 0)", "given B = (1, 0)", "l = line A B", "m = line A l" ])
        `shouldEqual` "line 4: l is a line, expected a point"
    it "two intersection points without a selector" do
      outcome (script [ "given A = (0, 0)", "given B = (1, 0)", "c = circle A B", "d = circle B A", "X = meet c d" ])
        `shouldEqual` "line 5: meet c d: there are two intersection points; bind two names or add a selector (except, nearest, farthest, left, right)"
    it "objects that do not meet" do
      outcome (script [ "given A = (0, 0)", "given B = (1, 0)", "given C = (5, 0)", "given D = (6, 0)", "c = circle A B", "d = circle C D", "X, Y = meet c d" ])
        `shouldEqual` "line 7: meet c d: c and d do not meet"
    it "tangent circles meet once" do
      outcome (script [ "given A = (0, 0)", "given B = (1, 0)", "given C = (5, 0)", "c = circle A B", "d = circle C B", "X, Y = meet c d" ])
        `shouldEqual` "line 6: meet c d: there is only one intersection point; bind one name"
    it "redefinition" do
      outcome (script [ "given A = (0, 0)", "given A = (1, 0)" ]) `shouldEqual` "line 2: A is already defined"
    it "a degenerate circle" do
      outcome (script [ "given A = (0, 0)", "c = circle A A" ]) `shouldEqual` "line 2: circle A A: the radius is zero (A = A)"
    it "an error inside a macro is reported at the call" do
      outcome (script [ "given A = (0, 0)", "M = midpoint(A, A)" ])
        `shouldEqual` "line 2: circle A B: the radius is zero (A = B)"
    it "a parse error" do
      outcome (script [ "given A = (0, 0)", "B = line A" ]) `shouldEqual` "parse error at 2:11"
    it "a macro may not shadow a built-in" do
      outcome (script [ "def midpoint(A, B) -> M:", "  M = meet A B", "end" ])
        `shouldEqual` "line 1: midpoint is a built-in construction; choose another name"

  describe "built-in constructions" do
    let
      base = [ "given A = (0, 0)", "given B = (4, 0)", "given P = (1, 3)", "ab = line A B" ]
    it "midpoint" do
      let
        m = pointOf (run (script (base <> [ "M = midpoint(A, B)" ]))) "M"
      [ m.x, m.y ] `shouldEqual` [ C.fromInt 2, zero ]
    it "perpAt and perpFrom" do
      checks $ base <>
        [ "given Q = (3, 0)"
        , "l = perpAt(A, B, Q)"
        , "m = perpFrom(A, B, P)"
        , "assert perpendicular ab l"
        , "assert perpendicular ab m"
        , "assert on Q l"
        , "assert on P m"
        ]
    it "parallelThrough" do
      checks $ base <> [ "l = parallelThrough(A, B, P)", "assert parallel ab l", "assert on P l" ]
    it "angleBisector halves the angle" do
      checks $ base <>
        [ "l = angleBisector(B, A, P)", "c = circle A B", "X = meet l c left A B", "assert angle B A X == angle X A P" ]
    it "transfer copies a length" do
      let
        t = run (script (base <> [ "given O = (-2, 5)", "c = transfer(O, A, P)" ]))
      map _.message t.error `shouldEqual` Nothing
      G.circleRadius2 (circleOf t "c") `shouldEqual` G.dist2 (pointOf t "A") (pointOf t "P")
      G.samePoint (G.circleCenter (circleOf t "c")) (pointOf t "O") `shouldEqual` true
    it "circleDiameter" do
      let
        t = run (script (base <> [ "c = circleDiameter(A, B)", "assert on A c", "assert on B c" ]))
      map _.message t.error `shouldEqual` Nothing
      G.samePoint (G.circleCenter (circleOf t "c")) (G.point (C.fromInt 2) zero) `shouldEqual` true

  describe "parsing" do
    it "comments, notes and macros" do
      case parseScript (script [ "-- a comment", "title \"T\"", "given A = (1/2, -3) -- trailing", "note \"a -- b\"", "def f(X) -> Y:", "  Y = midpoint(X, X)", "end" ]) of
        Left e -> fail e.message
        Right s -> do
          s.title `shouldEqual` Just "T"
          Array.length s.body `shouldEqual` 2
          map _.name s.macros `shouldEqual` [ "f" ]
    it "a def without end" do
      case parseScript (script [ "def f(X) -> Y:", "  Y = line X X" ]) of
        Left e -> e.line `shouldEqual` 1
        Right _ -> fail "expected a parse error"

  describe "snapshots" do
    it "split the steps into pages" do
      let
        t = run (script [ "given A = (0, 0)", "snapshot", "given B = (1, 0)", "c = circle A B", "snapshot", "snapshot", "d = circle B A" ])
      pages t `shouldEqual` [ { first: 0, last: 0 }, { first: 1, last: 2 }, { first: 3, last: 3 } ]
    it "a snapshot before any step or at the end adds no page" do
      let
        t = run (script [ "snapshot", "given A = (0, 0)", "given B = (1, 0)", "snapshot" ])
      pages t `shouldEqual` [ { first: 0, last: 1 } ]
    it "are not allowed inside a construction" do
      outcome (script [ "def f(A) -> B:", "  snapshot", "end" ])
        `shouldEqual` "parse error at 2:1"
    it "the examples have the planned pages" do
      src <- liftEffect (readTextFile UTF8 "examples/heptadecagon/heptadecagon.sc")
      Array.length (pages (run src)) `shouldEqual` 12

  describe "strings" do
    it "keep backslashes that are not escapes (for Typst)" do
      case parseScript (script [ "note \"a \\* b \\\\ c \\\" d\"" ]) of
        Right s -> map _.value s.body `shouldEqual'` [ "a \\* b \\ c \" d" ]
        Left e -> fail e.message

  describe "pretty printing" do
    it "normalises rational radicands" do
      Pretty.unicode (unsafePartial (fromJust (C.sqrt (C.fromInts 3 4)))) `shouldEqual` "1/2·√3"
    it "nested radicals" do
      Pretty.unicode (C.fromInts 1 2 + C.fromInts 1 2 * sqrtC 5) `shouldEqual` "1/2 + 1/2·√5"
      Pretty.unicode (negate (sqrtC 2)) `shouldEqual` "−√2"
    it "exact values with approximations" do
      Pretty.withApprox (C.fromInts 1 2) `shouldEqual` "1/2"
      Pretty.withApprox (sqrtC 2) `shouldEqual` "√2 ≈ 1.414213562"
    it "Typst math" do
      Pretty.typst (C.fromInts 1 2 + C.fromInts 1 2 * sqrtC 5) `shouldEqual` "frac(1, 2) + frac(1, 2) sqrt(5)"
    it "short radicands stay nested in Typst, long ones are named" do
      let
        root x = unsafePartial (fromJust (C.sqrt x))
        r17 = sqrtC 17
        short = root (C.fromInt 10 + C.fromInt 2 * sqrtC 5)
        a = root (C.fromInts 17 8 - C.fromInts 1 8 * r17)
        long = root (C.fromInts 17 16 + C.fromInts 3 16 * r17 - (C.fromInts 3 8 + C.fromInts 1 8 * r17) * a)
        none = Pretty.nameRadicals Pretty.noRadicals short
        named = Pretty.nameRadicals Pretty.noRadicals long
      none.defs `shouldEqual` []
      Pretty.typstNamed none.names short `shouldEqual` "sqrt(10 + 2 sqrt(5))"
      map _.name named.defs `shouldEqual` [ "rho_(1)" ]
      Pretty.typstNamed named.names long `shouldEqual` "rho_(1)"
      Pretty.typstNamed named.names (C.fromInt 2 * long) `shouldEqual` "2 rho_(1)"
    it "equal values in a check are shown once" do
      P.detailText [ { label: "|AB|", value: one }, { label: "|AC|", value: one } ] `shouldEqual` "|AB| = |AC| = 1"
      P.detailText [ { label: "|AB|", value: one }, { label: "|AC|", value: C.fromInt 2 } ] `shouldEqual` "|AB| = 1, |AC| = 2"
