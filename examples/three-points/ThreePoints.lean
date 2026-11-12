import StraightedgeCompass.Elab

/-!
# Three points and a circle

A universal statement, from `examples/three-points/three-points.sc`: one theorem
per case, for all values of the variables that satisfy the hypotheses of the
PDF, and a coverage theorem for each `cases` statement: its cases cover every
configuration. Regenerate the input with `npm run core` after changing the
script.
-/

construction_proof threePoints from "examples/three-points/three-points.core.json"

#print axioms threePoints.case_one
#print axioms threePoints.case_none
#print axioms threePoints.case_same_q_eq_0_p_eq_0
#print axioms threePoints.cover
