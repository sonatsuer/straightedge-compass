import StraightedgeCompass.Elab

/-!
# Tangents from a point

A universal statement, from
`examples/tangents-from-a-point/tangents-from-a-point.sc`: one theorem per case,
for all values of the variables that satisfy the hypotheses of the PDF, and a
coverage theorem for each `cases` statement: its cases cover every
configuration. Regenerate the input with `npm run core` after changing the
script.
-/

construction_proof tangentsFromAPoint from
  "examples/tangents-from-a-point/tangents-from-a-point.core.json"

#print axioms tangentsFromAPoint.case_two
#print axioms tangentsFromAPoint.case_one_p_eq_neg1
#print axioms tangentsFromAPoint.case_one_p_eq_1
#print axioms tangentsFromAPoint.case_none
#print axioms tangentsFromAPoint.cover
