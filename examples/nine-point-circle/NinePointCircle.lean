import StraightedgeCompass.Elab

/-!
# The nine-point circle

A universal statement, from `examples/nine-point-circle/nine-point-circle.sc`:
one theorem, for all values of the variables that satisfy the hypotheses of the
PDF. Regenerate the input with `npm run core` after changing the script.
-/

construction_proof ninePointCircle from "examples/nine-point-circle/nine-point-circle.core.json"

#print axioms ninePointCircle
