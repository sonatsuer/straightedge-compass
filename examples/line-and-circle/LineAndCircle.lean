import StraightedgeCompass.Elab

/-!
# A line and a circle

A universal statement, from `examples/line-and-circle/line-and-circle.sc`: one
theorem per case, for all values of the variables that satisfy the hypotheses of
the PDF, and a coverage theorem for each `cases` statement: its cases cover
every configuration. Regenerate the input with `npm run core` after changing the
script.
-/

construction_proof lineAndCircle from "examples/line-and-circle/line-and-circle.core.json"

#print axioms lineAndCircle.case_two
#print axioms lineAndCircle.case_one_h_eq_neg1
#print axioms lineAndCircle.case_one_h_eq_1
#print axioms lineAndCircle.case_none
#print axioms lineAndCircle.cover
