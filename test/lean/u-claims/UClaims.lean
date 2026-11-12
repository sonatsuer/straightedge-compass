import StraightedgeCompass.Elab

/-!
# Test: claims with variables

A universal statement: every predicate as a claim with variables, angles with
positive, zero and negative cosines, from `test/lean/u-claims/u-claims.sc`.
-/

construction_proof uClaims from "test/lean/u-claims/u-claims.core.json"

#print axioms uClaims
