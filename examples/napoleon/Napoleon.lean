import StraightedgeCompass.Elab

/-!
# Napoleon's theorem

A universal statement, from `examples/napoleon/napoleon.sc`: one theorem, for
all values of the variables that satisfy the hypotheses of the PDF. Regenerate
the input with `npm run core` after changing the script.
-/

construction_proof napoleon from "examples/napoleon/napoleon.core.json"

#print axioms napoleon
