module Test.Prove (spec) where

import Prelude

import Constructible.Rational as Q
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.String (Pattern(..), contains, joinWith, split)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Partial.Unsafe (unsafeCrashWith)
import Proof.Eval (evaluateAt)
import Proof.Parser (parseScript)
import Proof.Prove (PNode(..), Proof, hypDisplay, namesOf, prove)
import Proof.Syntax (Label(..), SampleValue(..), Script, labelText)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

script :: String -> Script
script src = case parseScript src of
  Right s -> s
  Left e -> unsafeCrashWith ("line " <> show e.line <> ": " <> e.message)

errorsOf :: Proof -> String
errorsOf p = joinWith "; " (map _.message p.errors)

-- | The theorem hypotheses of a leaf, as text.
theorem :: PNode -> Array String
theorem (PNode n) = map (hypDisplay (namesOf n.vars)) n.theorem

tangents :: String
tangents =
  """
given O = (0, 0)
given P = (1, 0)
param r ~ 1/2
given R = (r, 0)
u = circle O R
k = circleDiameter(O, P)
cases meet u k
two T1, T2:
  assert dist P T1 == dist P T2
one T:
  sample r ~ 1
  assert T == P
none:
  sample r ~ 2
end
"""

triangle :: String
triangle =
  """
given A = (0, 0)
given B = (1, 0)
param p ~ 1/3
param q ~ 3/4
given C = (p, q)
"""

spec :: Spec Unit
spec = describe "Universal statements" do
  describe "proofs" do
    it "split into the cases of an intersection" do
      let
        proof = prove (script tangents)
        PNode root = proof.root
      errorsOf proof `shouldEqual` ""
      case root.split of
        Nothing -> fail "expected a split"
        Just sp -> do
          map (labelText <<< _.label) sp.branches `shouldEqual` [ "two", "one", "none", "same" ]
          let
            byLabel l = Array.find (\b -> b.label == l) sp.branches
          case byLabel LTwo >>= \b -> Array.head b.cases >>= _.node of
            Just n -> theorem n `shouldEqual` [ "u and k meet in two points: −r⁴ + r² > 0" ]
            Nothing -> fail "no node for two"
          map _.text (fromMaybe [] (_.cases <$> byLabel LOne)) `shouldEqual` [ "r = −1", "r = 0", "r = 1" ]
          map (\c -> c.impossible /= Nothing) (fromMaybe [] (_.cases <$> byLabel LOne)) `shouldEqual` [ false, true, false ]
          ((_.impossible) <$> byLabel LSame) `shouldEqual` Just (Just "the centres differ")
    it "report a case that is not covered, with an example" do
      let
        proof = prove (script tangents)
        missing = prove
          ( script
              """
given O = (0, 0)
given P = (1, 0)
param r ~ 1/2
given R = (r, 0)
u = circle O R
k = circleDiameter(O, P)
cases meet u k
two T1, T2:
  assert dist P T1 == dist P T2
one T:
  sample r ~ 1
end
"""
          )
      errorsOf proof `shouldEqual` ""
      contains (Pattern "the case none is not covered: for example r = ") (errorsOf missing) `shouldEqual` true
    it "reject a sample outside its case" do
      let
        bad = prove (script (replace "sample r ~ 2" "sample r ~ 1/3" tangents))
      contains (Pattern "the sample is not in this case") (errorsOf bad) `shouldEqual` true
    it "fail on a claim that is not an identity" do
      let
        bad = prove (script (replace "assert dist P T1 == dist P T2" "assert dist P T1 == dist O T1" tangents))
      contains (Pattern "assertion not proved: dist P T1 == dist O T1") (errorsOf bad) `shouldEqual` true
    it "need a split when a selector depends on the configuration" do
      let
        bad = prove (script (triangle <> "c1 = circle B C\nc2 = circle C B\nX = meet c1 c2 nearest A\n"))
      contains (Pattern "depends on the configuration") (errorsOf bad) `shouldEqual` true
    it "find the degenerate cases of a construction" do
      let
        proof = prove (script (triangle <> "mc = perpBisector(A, B)\nma = perpBisector(B, C)\nO = meet mc ma\nmb = perpBisector(C, A)\nassert on O mb\n"))
        PNode root = proof.root
      errorsOf proof `shouldEqual` ""
      theorem proof.root `shouldEqual` [ "mc and ma are not parallel: q ≠ 0" ]
      Array.length root.steps `shouldEqual` 8
    it "prove the examples" do
      for_' [ "line-and-circle", "tangents-from-a-point", "two-circles", "three-points", "thales", "circumcentre", "napoleon", "euler-line", "nine-point-circle" ] \name -> do
        src <- liftEffect (readTextFile UTF8 ("examples/" <> name <> "/" <> name <> ".sc"))
        (name <> ": " <> errorsOf (prove (script src))) `shouldEqual` (name <> ": ")
    it "tell parallel lines from the same line" do
      src <- liftEffect (readTextFile UTF8 "examples/three-points/three-points.sc")
      let
        PNode root = (prove (script src)).root
        branch l = root.split >>= \sp -> Array.find (\b -> b.label == l) sp.branches
        possible l = map _.text (Array.filter (\c -> c.impossible == Nothing) (fromMaybe [] (_.cases <$> branch l)))
        leafOf l = branch l >>= \b -> Array.head b.cases >>= _.node
      -- parallel needs distinct lines: C ≠ A (p ≠ 0) and C ≠ B (p ≠ 1)
      map theorem (leafOf LNone) `shouldEqual` Just [ "mc and ma are distinct: p² − p ≠ 0" ]
      possible LSame `shouldEqual` [ "q = 0, p = 0" ]
    it "rule out a solution that belongs to another case" do
      src <- liftEffect (readTextFile UTF8 "examples/two-circles/two-circles.sc")
      let
        PNode root = (prove (script src)).root
        one = root.split >>= \sp -> Array.find (\b -> b.label == LOne) sp.branches
      map (\c -> Tuple c.text (c.impossible == Nothing)) (fromMaybe [] (_.cases <$> one))
        `shouldEqual` [ Tuple "d = −2" true, Tuple "d = 0" false, Tuple "d = 2" true ]
  describe "configurations" do
    it "follow the branch they lie in" do
      let
        s = script tangents
        at q = (evaluateAt (Map.singleton "r" (SVNum q)) s).path
      map (map (labelText <<< _.label)) [ at (Q.fromInts 1 2), at (Q.fromInt 1), at (Q.fromInt 2) ]
        `shouldEqual` [ [ "two" ], [ "one" ], [ "none" ] ]
  where
  for_' xs f = void (traverse' f xs)
  traverse' f xs = Array.foldM (\_ x -> f x) unit xs
  replace a b str = joinWith b (split (Pattern a) str)
