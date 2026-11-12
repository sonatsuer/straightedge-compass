import StraightedgeCompass.Elab

/-!
# Test: cases with variables

A universal statement: every form of `cases` (side, compare, `==`, a predicate),
nested, with equation branches solved by substitution, and the coverage theorem
of each, from `test/lean/u-cases/u-cases.sc`.
-/

construction_proof uCases from "test/lean/u-cases/u-cases.core.json"

#print axioms uCases.case_left_less_yes
#print axioms uCases.case_left_equal_yes
#print axioms uCases.case_left_greater_yes
#print axioms uCases.case_on_equal_no
#print axioms uCases.case_on_different_no
#print axioms uCases.case_right_no
#print axioms uCases.cover
#print axioms uCases.case_left.cover
#print axioms uCases.case_left_less.cover
#print axioms uCases.case_left_equal.cover
#print axioms uCases.case_left_greater.cover
#print axioms uCases.case_on.cover
#print axioms uCases.case_on_equal.cover
#print axioms uCases.case_on_different.cover
#print axioms uCases.case_right.cover
