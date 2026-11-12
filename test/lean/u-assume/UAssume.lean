import StraightedgeCompass.Elab

/-!
# Test: assumptions with variables

A universal statement: assumptions: inequalities of distances, `!=`, and an
equation solved by substitution, from `test/lean/u-assume/u-assume.sc`.
-/

construction_proof uAssume from "test/lean/u-assume/u-assume.core.json"

#print axioms uAssume
