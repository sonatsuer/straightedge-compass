module Test.Main (main) where

import Prelude

import Effect (Effect)
import Test.Algebra as Algebra
import Test.Constructible as Constructible
import Test.Export as Export
import Test.Figure as Figure
import Test.Geometry as Geometry
import Test.Proof as Proof
import Test.Prove as Prove
import Test.Universal as Universal
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Constructible.spec
  Geometry.spec
  Proof.spec
  Export.spec
  Figure.spec
  Algebra.spec
  Prove.spec
  Universal.spec
