import StraightedgeCompass.Elab

/-!
# Thales' theorem

A universal statement, from `examples/thales/thales.sc`: one theorem, for all
values of the variables that satisfy the hypotheses of the PDF. Regenerate the
input with `npm run core` after changing the script.
-/

construction_proof thales from "examples/thales/thales.core.json"

#print axioms thales
