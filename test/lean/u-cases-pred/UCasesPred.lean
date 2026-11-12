import StraightedgeCompass.Elab

/-!
# Test: cases on predicates

A universal statement: `cases` on a distance comparison (`yes:`, `no:`) and on
an equation (`on C m`, whose `yes:` is solved by substitution), with the
coverage theorem of each, from `test/lean/u-cases-pred/u-cases-pred.sc`.
-/

construction_proof uCasesPred from "test/lean/u-cases-pred/u-cases-pred.core.json"

#print axioms uCasesPred.case_yes_yes
#print axioms uCasesPred.case_yes_no
#print axioms uCasesPred.case_no_yes
#print axioms uCasesPred.case_no_no
#print axioms uCasesPred.cover
#print axioms uCasesPred.case_yes.cover
#print axioms uCasesPred.case_no.cover
