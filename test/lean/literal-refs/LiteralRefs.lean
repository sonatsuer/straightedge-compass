import StraightedgeCompass.Elab

/-!
# Test: literal reference points

Selectors with a literal point (`nearest (0, 1)`, `farthest (0, 1)`,
`except (1, 0)`), from `test/lean/literal-refs/literal-refs.sc`.
-/

construction_proof literalRefs from "test/lean/literal-refs/literal-refs.core.json"

#print axioms literalRefs
