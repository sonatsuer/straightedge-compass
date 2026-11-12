module Test.Algebra (spec) where

import Prelude

import Constructible.ParamField as PF
import Constructible.Poly as P
import Constructible.RatFun as RF
import Constructible.Rational as Q
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Proof.Signs (Answer(..), Rel(..), oracleFor, signOf)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

x :: P.Poly
x = P.var 0

y :: P.Poly
y = P.var 1

c :: Int -> P.Poly
c = P.fromInt

describeAnswer :: Answer -> String
describeAnswer = case _ of
  APos _ -> "pos"
  ANeg _ -> "neg"
  AZero -> "zero"
  ANonNeg _ -> "nonneg"
  ANonPos _ -> "nonpos"
  AVaries _ _ -> "varies"
  ANonZero _ -> "nonzero"
  AUnknown _ -> "unknown"

spec :: Spec Unit
spec = describe "Algebra over ℚ(x)" do
  describe "polynomials" do
    it "gcd" do
      P.gcd ((x - c 1) * (x + c 2)) ((x - c 1) * (x - c 3)) `shouldEqual` (x - c 1)
      P.gcd ((x + y) * (x - y)) ((x + y) * (x + y)) `shouldEqual` (x + y)
      P.gcd (x * y + c 1) (x + c 1) `shouldEqual` c 1
    it "square roots" do
      let
        s = x - P.scale (Q.fromInt 2) y + c 3
      (P.sqrt (s * s) == Just s || P.sqrt (s * s) == Just (zero - s)) `shouldEqual` true
      P.sqrt (s * s + c 1) `shouldEqual` Nothing
    it "substitution and evaluation" do
      P.evalAt (x * x + y) [ Just (Q.fromInt 3), Just (Q.fromInt 1) ] `shouldEqual` Just (Q.fromInt 10)
      P.evalAt (x * x + y) [ Just (Q.fromInt 3), Nothing ] `shouldEqual` Nothing
  describe "rational functions" do
    it "are reduced" do
      let
        f = RF.fromPoly (x * x - c 1) / RF.fromPoly (x - c 1)
      f `shouldEqual` RF.fromPoly (x + c 1)
      (RF.var 0 / RF.var 0) `shouldEqual` one
    it "square roots up to sign" do
      let
        f = RF.fromPoly (x * x) / RF.fromPoly (y * y)
        r = RF.sqrt f
      (r == Just (RF.var 0 / RF.var 1) || r == Just (negate (RF.var 0 / RF.var 1))) `shouldEqual` true
  describe "the sign engine" do
    let
      r = x
      delta = r * r * (c 1 - r * r)
      sample = [ Just (Q.fromInts 1 2) ]
    it "decides one variable exactly" do
      describeAnswer (signOf { conds: [ { poly: delta, rel: RPos } ], samples: [ sample ] } (c 1 - r * r)) `shouldEqual` "pos"
      describeAnswer (signOf { conds: [], samples: [ sample ] } delta) `shouldEqual` "varies"
      describeAnswer (signOf { conds: [], samples: [ sample ] } (r * r * r * r + c 1)) `shouldEqual` "pos"
    it "uses squares, quadratics and hypotheses" do
      let
        ctx = { conds: [ { poly: y, rel: RPos } ], samples: [ [ Just (Q.fromInts 1 3), Just (Q.fromInts 3 4) ] ] }
        d = (x - c 1) * (x - c 1) + y * y
      describeAnswer (signOf ctx d) `shouldEqual` "pos"
      describeAnswer (signOf { conds: [], samples: ctx.samples } d) `shouldEqual` "nonneg"
      describeAnswer (signOf ctx (y * d)) `shouldEqual` "pos"
      describeAnswer (signOf ctx (zero - P.scale (Q.fromInt 3) (y * y))) `shouldEqual` "neg"
      describeAnswer (signOf ctx (x * y)) `shouldEqual` "varies"
  describe "towers over ℚ(x)" do
    it "chooses the non-negative root of a square from the hypotheses" do
      let
        rr = PF.var 0
        pos = oracleFor { conds: [ { poly: x, rel: RPos } ], samples: [ [ Just (Q.fromInts 1 2) ] ] }
        neg = oracleFor { conds: [ { poly: x, rel: RNeg } ], samples: [ [ Just (Q.fromInts (-1) 2) ] ] }
        free = oracleFor { conds: [], samples: [ [ Just (Q.fromInts 1 2) ] ] }
        root o = PF.runPM o do
          sq <- PF.mul rr rr
          s <- PF.sqrt sq
          PF.eq s rr
      root pos `shouldEqual` Right true
      root neg `shouldEqual` Right false
      case root free of
        Left _ -> pure unit
        Right _ -> fail "√(r²) should need the sign of r"
    it "computes identities with radicals" do
      let
        o = oracleFor { conds: [ { poly: x, rel: RPos } ], samples: [ [ Just (Q.fromInts 1 2) ] ] }
        res = PF.runPM o do
          -- (√(1 − r²))² = 1 − r² and √(r²(1 − r²)) = r·√(1 − r²) for r > 0
          let
            rr = PF.var 0
          one' <- pure (PF.fromInt 1)
          r2 <- PF.mul rr rr
          d <- PF.sub one' r2
          s <- PF.sqrt d
          s2 <- PF.mul s s
          e1 <- PF.eq s2 d
          rd <- PF.mul r2 d
          t <- PF.sqrt rd
          rs <- PF.mul rr s
          e2 <- PF.eq t rs
          pure [ e1, e2 ]
      res `shouldEqual` Right [ true, true ]
