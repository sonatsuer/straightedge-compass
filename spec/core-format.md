# Core export format, version 1

The core export is the file that the web tool and the CLI write for a proof
script, and that the Lean elaborator reads. It is produced by
`src/Export/Core.purs`:

```
node src/cli.mjs core examples/heptadecagon.sc    # writes examples/heptadecagon.core.json
```

It describes the script after evaluation:

- Every macro call is expanded into primitive constructions.
- Every intersection choice is resolved.
- Every point has exact coordinates, as hints.

A consumer must not trust the hints. It re-checks every condition, as
`test/check-core.mjs` and the Lean elaborator do. Lean checks them with ring
operations and signs only.

## Top level

```json
{
  "format": "straightedge-compass-core",
  "version": 1,
  "title": "…" | null,
  "givens": 2,
  "tower": [ElementAtDepth0, ElementAtDepth1, …],
  "objects": [Object, …],
  "asserts": [Assert, …],
  "steps": [Step, …]
}
```

## Numbers

**Rationals** are strings `"p"` or `"p/q"`, with q > 0 and the fraction in
lowest terms. Rationals occur in given points and in literal reference points.

**The tower** is a list of radicands r₁, …, r_D. The field is
K_D = Q(√r₁)(√r₂)…(√r_D). Each radicand r_i is an element of K_{i−1}, the field
below it, and is written at depth i − 1. Every radicand is positive, and its
`den` is `"1"`: the exporter rescales the generators so that radicands are
integer trees. Products of integer trees are then integer trees again. The
exporter also guarantees that each r_i is not a square in K_{i−1}, but a
checker does not need this property for soundness.

**An element** of K_k is written at depth k as

```json
{ "num": Tree, "den": "d" }
```

It stands for Tree / d, where d is a positive integer string. A tree is either
an integer string or a pair:

- An integer string `"n"` is the integer n, embedded at any depth. In
  particular, `"0"` is zero.
- A pair `[a, b]` at depth k ≥ 1 is a + b·√r_k, where a and b are trees at
  depth k − 1.

All coordinates are elements of the full field K_D, written at depth D. With
integer leaves and one denominator per element, a checker can compare elements
by cross-multiplying, without computing a gcd.

## Objects

Objects are listed by id, in creation order. Each object refers only to
objects with smaller ids.

```json
{
  "id": 7,
  "name": "P0'",                    // local name; macro objects have local names
  "path": ["perpBisector(P0', P0)"],  // enclosing macro calls, outermost first
  "step": 4,                        // top-level step that created it
  "named": "yes" | "no",            // bound to a top-level name
  "kind": "point" | "line" | "circle",
  "def": Def,
  "coords": { "x": Element, "y": Element }   // points only (hints)
}
```

`Def` is one of the following:

| `def` | meaning |
|---|---|
| `{ "given": { "x": Rational, "y": Rational } }` | a given point |
| `{ "line": [p, q] }` | the line through the points p ≠ q |
| `{ "circle": [o, p] }` | the circle with centre o through p (o ≠ p) |
| `{ "meet": [a, b], "choice": Choice, "other": Coords \| null }` | a point of a ∩ b |

For a `meet`, a and b are distinct lines or circles. `other` is the second
intersection point when there are two (a hint), and `null` when the point is
the only one.

`Choice` says which intersection point was taken:

| `kind` | condition on the point X and the other point Y |
|---|---|
| `unique` | a ∩ b is the single point X |
| `lexFirst` | X < Y lexicographically (x first, then y) |
| `lexSecond` | Y < X lexicographically |
| `except` + `ref` | Y = ref |
| `nearest` + `ref` | dist(X, ref) < dist(Y, ref) |
| `farthest` + `ref` | dist(X, ref) > dist(Y, ref) |
| `left` + `line: [p, q]` | X is strictly left of p → q, and Y is not |
| `right` + `line: [p, q]` | X is strictly right of p → q, and Y is not |

A `ref` is `{ "object": id }` (a point object) or
`{ "point": { "x": Rational, "y": Rational } }`. "Left of p → q" means that the
cross product (q − p) × (X − p) is positive.

## Assertions

```json
{
  "pred": { "kind": K, "args": [ids…], "cmp": "==" },   // cmp only for "dist"
  "text": "dist A C == dist A B",
  "step": 6,
  "context": [],         // enclosing macro calls; [] for top-level claims
  "centre": { "x": Element, "y": Element }   // concyclic only (hint)
}
```

| `kind` | args | holds when |
|---|---|---|
| `on` | P, x | P lies on the line or circle x |
| `eq`, `ne` | P, Q | P = Q, P ≠ Q |
| `dist` | A, B, C, D | \|AB\| ⋈ \|CD\| with ⋈ = `cmp` ∈ {==, !=, <, <=, >, >=} |
| `collinear` | P₁ … Pₙ (n ≥ 3) | all points on one line |
| `concyclic` | P₁ … Pₙ (n ≥ 4) | all points on one circle |
| `parallel`, `perpendicular` | l, m (lines) | the direction vectors are parallel or orthogonal |
| `angle` | A, B, C, D, E, F | ∠ABC = ∠DEF (unsigned, in [0, π]) |
| `ccw` | A, B, C | A, B, C turn counterclockwise |
| `between` | A, B, C | B lies strictly between A and C |

Each condition reduces to polynomial equations and sign conditions in the
coordinates, as in `test/check-core.mjs`:

- **Distances** are compared through squared distances.
- **Angles** are equal when cos ∠ABC = cos ∠DEF. Write d = (A − B)·(C − B) and
  n = \|A − B\|²\|C − B\|² for the first angle, and d′, n′ for the second. The
  cosines are equal when d and d′ have the same sign and d²·n′ = d′²·n.
- **Concyclic** points come with `centre`, a point in the common tower (a hint,
  like `coords`). The condition is that all points have the same squared
  distance from it. A consumer must check this and must not trust the hint.

## Steps

`steps` mirrors the script's top-level statements:
`{ "line": n, "text": "…", "objects": [ids], "notes": ["…"] }`. Steps are
informative only, used for the theorem's documentation.

## Scripts with variables

Scripts with `param`, variable coordinates or free points (`point on`) prove
universal statements. Their export has another format,
`straightedge-compass-universal`; see `spec/universal-format.md`.

## Compatibility

A consumer must reject files whose `format` or `version` it does not know. A
change that alters the meaning of an existing field increases `version`.
