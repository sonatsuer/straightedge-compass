import StraightedgeCompass.Elab

/-!
# Two circles of equal radius

A universal statement, from `examples/two-circles/two-circles.sc`: one theorem
per case, for all values of the variables that satisfy the hypotheses of the
PDF, and a coverage theorem for each `cases` statement: its cases cover every
configuration. Regenerate the input with `npm run core` after changing the
script.
-/

construction_proof twoCircles from "examples/two-circles/two-circles.core.json"

#print axioms twoCircles.case_two
#print axioms twoCircles.case_one_d_eq_neg2
#print axioms twoCircles.case_one_d_eq_2
#print axioms twoCircles.case_none
#print axioms twoCircles.case_same
#print axioms twoCircles.cover
