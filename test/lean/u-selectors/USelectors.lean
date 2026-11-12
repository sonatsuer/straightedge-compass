import StraightedgeCompass.Elab

/-!
# Test: selectors with variables

A universal statement: every selector with variables (nearest, farthest, except,
left, right, with named and literal points), ordered and unordered pairs, and
free points on a line and a circle, from `test/lean/u-selectors/u-selectors.sc`.
-/

construction_proof uSelectors from "test/lean/u-selectors/u-selectors.core.json"

#print axioms uSelectors
