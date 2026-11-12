module Test.Universal (spec) where

import Prelude

import Constructible.Poly as P
import Constructible.Rational as Q
import Data.Argonaut.Core (Json, toArray, toObject)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl, for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.Tuple (fst)
import Effect.Class (liftEffect)
import Export.Universal as Universal
import Foreign.Object as Object
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Partial.Unsafe (unsafeCrashWith)
import Proof.Cert (Deriv(..), FRel(..), Rule(..), byTerms, termWith)
import Proof.Cert as C
import Proof.Parser (parseScript)
import Proof.Prove (PNode(..), hypDisplay, namesOf, prove)
import Proof.Signs (Answer(..), Rel(..), certSign)
import Proof.Syntax (Script)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

script :: String -> Script
script src = case parseScript src of
  Right s -> s
  Left e -> unsafeCrashWith ("line " <> show e.line <> ": " <> e.message)

x :: P.Poly
x = P.var 0

sv :: P.Poly
sv = P.var 1

c :: Int -> P.Poly
c = P.fromInt

-- | The number of entries of a top-level array field.
count :: String -> Json -> Int
count k j = fromMaybe (-1) (toObject j >>= Object.lookup k >>= toArray <#> Array.length)

spec :: Spec Unit
spec = describe "Lean export of universal statements" do
  describe "certificates" do
    it "reduce modulo the square roots" do
      let
        -- s = √((1 − x²) / 2): 2 s² − (1 − x²) = 0
        sq = { v: 1, n: c 1 - x * x, d: c 2 }
        g = sv * sv * sv + x * sv * sv
        red = C.reduce [ sq ] g
        m = foldl (\acc i -> acc * (fromMaybe sq (Array.index [ sq ] i)).d) one red.mult
        z = sq.d * sv * sv - sq.n
        rhs = red.r + foldl (\acc e -> acc + e.c * z) zero red.cof
      (m * g == rhs) `shouldEqual` true
      P.degIn red.r 1 `shouldEqual` 1
    it "come with every sign the engine proves" do
      let
        ctx = { conds: [ { poly: x * x - c 1, rel: RPos } ], samples: [] }
        -- x² − 1 > 0 gives x² > 0, so x ≠ 0
        r = certSign ctx (x * x)
      case r of
        Just { answer: APos _ } -> pure unit
        _ -> fail "x² > 0 not proved"
    it "are checked when they enter a chain" do
      let
        h = { p: c 1 - x * x, rel: FPos }
        -- a wrong identity: 2 (1 − x²) is not (1 − x²)
        bad = Derived { p: c 2 - x * x, rel: FPos } (BySum { mult: C.unitTerm, terms: [ termWith one [ Known h ] [] one ], eqs: [] })
        good = byTerms { p: c 2 - c 2 * x * x, rel: FPos } [ termWith (Q.fromInt 2) [ Known h ] [] one ]
        run d = fst (C.runChain (C.emptyChain 1) (C.addHyps [ h ] *> C.addDeriv d))
      case run bad of
        Left _ -> pure unit
        Right _ -> fail "a wrong certificate was accepted"
      case run good of
        Right _ -> pure unit
        Left e -> fail e
  describe "the export" do
    it "writes every example and test" do
      for_
        [ "examples/thales/thales.sc", "examples/circumcentre/circumcentre.sc", "examples/tangents-from-a-point/tangents-from-a-point.sc"
        , "examples/two-circles/two-circles.sc", "examples/three-points/three-points.sc", "examples/line-and-circle/line-and-circle.sc"
        , "test/lean/u-selectors/u-selectors.sc", "test/lean/u-claims/u-claims.sc", "test/lean/u-assume/u-assume.sc"
        , "test/lean/u-cases/u-cases.sc", "test/lean/u-rational/u-rational.sc", "test/lean/u-cases-pred/u-cases-pred.sc"
        ]
        \file -> do
          src <- liftEffect (readTextFile UTF8 file)
          let
            proof = prove (script src)
          (file <> ": " <> joinWith "; " (map _.message proof.errors)) `shouldEqual` (file <> ": ")
          case Universal.documentJson proof of
            Right _ -> pure unit
            Left e -> fail (file <> ": " <> e)
    it "has one theorem per case and one cover per split" do
      src <- liftEffect (readTextFile UTF8 "examples/tangents-from-a-point/tangents-from-a-point.sc")
      case Universal.documentJson (prove (script src)) of
        Right j -> do
          count "leaves" j `shouldEqual` 4
          count "covers" j `shouldEqual` 1
        Left e -> fail e
  describe "the prover" do
    it "replays an assumed equation after its substitution" do
      src <- liftEffect (readTextFile UTF8 "test/lean/u-assume/u-assume.sc")
      let
        proof = prove (script src)
        PNode root = proof.root
      joinWith "; " (map _.message proof.errors) `shouldEqual` ""
      map _.var root.subst `shouldEqual` [ "p" ]
    it "drops a hypothesis that a substitution makes a true constant" do
      let
        proof = prove (script "given A = (0, 0)\ngiven B = (1, 0)\nparam q ~ 1\ngiven C = (1, q)\ncases ccw A B C\nyes:\n  assert ccw A B C\nno:\n  sample q ~ -1\n  cases side C A B\n  left:\n    assert ccw A B C\n  on:\n    assert collinear A B C\n  right:\n    assert ccw B A C\n  end\nend\n")
      joinWith "; " (map _.message proof.errors) `shouldEqual` ""
      for_ (leafHyps proof.root) \hs -> Array.any (_ == "not ccw A B C: 0 ≤ 0") hs `shouldEqual` false
  where
  leafHyps (PNode n) = case n.split of
    Nothing -> [ map (hypDisplay (namesOf n.vars)) n.theorem ]
    Just sp -> Array.concatMap (\b -> Array.concatMap (\cs -> maybe [] leafHyps cs.node) b.cases) sp.branches
  maybe d f = case _ of
    Just a -> f a
    Nothing -> d
