import StraightedgeCompass.Elab

/-!
# Test: a rational substitution

A universal statement: an equation solved for a variable whose coefficient is
not constant: the given point `C` gets rational coordinates in the remaining
variable, from `test/lean/u-rational/u-rational.sc`.
-/

construction_proof uRational from "test/lean/u-rational/u-rational.core.json"

#print axioms uRational
