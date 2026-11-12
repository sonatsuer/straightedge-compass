# Straightedge and Compass

Exact straightedge-and-compass construction proofs, in the browser.

**Live:** <https://sonatsuer.github.io/straightedge-compass/>

You write a construction as a short script. The tool:

- computes every point exactly, in the field of constructible numbers (towers
  of square roots over the rationals), not in floating point;
- checks every claim you assert with exact arithmetic;
- proves universal statements: with variables in the script, it computes over
  rational functions of the variables, finds the hypotheses the construction
  needs, and handles case splits;
- plays the construction back step by step, forwards and backwards, with zoom
  and pan;
- exports the proof as a PDF, with a page wherever you write `snapshot`;
- exports a file from which a [Lean 4](https://lean-lang.org/) command produces
  a kernel-checked theorem in Mathlib's terms, for universal statements too.

The examples are:

- **Constructions:**
  - Euclid I.1 (an equilateral triangle);
  - Euclid I.46 (a square on a segment);
  - the regular hexagon (Euclid IV.15);
  - the regular pentagon by a Carlyle circle;
  - the regular 15-gon (Euclid IV.16, with Carlyle circles);
  - Gauss's regular 17-gon by Carlyle circles.
- **Universal statements:**
  - Thales' theorem;
  - the circumcentre;
  - Napoleon's theorem;
  - the Euler line;
  - the nine-point circle;
  - with case splits: a line and a circle (two points, touching, or apart),
    the tangents from a point (outside, on, or inside the circle), two equal
    circles (all four outcomes of `meet`), and three points and a circle (the
    perpendicular bisectors meet, are parallel, or coincide).

## A first proof

```
title "Euclid I.1: an equilateral triangle on a given segment"

given A = (0, 0)
given B = (1, 0)

c1 = circle A B             -- centre A, through B
c2 = circle B A
C = meet c1 c2 left A B     -- of the two intersection points, the one left of A → B
note "$C$ lies on both circles, so $|C A| = |A B| = |B C|$."

assert dist A C == dist A B
assert dist B C == dist A B
```

## The script language

One statement per line. `--` starts a comment.

| Statement | Meaning |
|---|---|
| `title "…"` | The title of the proof (once, at the top). |
| `given A = (x, y)` | A given point with rational coordinates, such as `(1/2, -3)`. |
| `l = line A B` | The line through two distinct points. |
| `c = circle O A` | The circle with centre `O` through `A`. |
| `X = meet a b` | The intersection of two lines or circles, when it is a single point. |
| `X, Y = meet a b` | Both intersection points, in lexicographic order (x first, then y). |
| `X = meet a b SELECTOR` | One of two intersection points; see below. |
| `M = midpoint(A, B)` | A call of a construction (built-in or your own). |
| `assert PREDICATE` | A claim, checked exactly. |
| `note "…"` | Text attached to the step above (or to the introduction), in Typst markup. |
| `snapshot` | Ends a page of the PDF and of the *by page* playback. |
| `show segment A B` | Draws the segment AB. |
| `show polygon A B C …` | Draws the closed polygon, for example a triangle. |
| `show circle O A` | Draws the circle with centre O through A. |
| `show circle through A B C` | Draws the circle through three points (a circumcircle). |

`show` only draws: it creates no object and no step, and it is not part of the
proof. It attaches to the step above it, like a note, and is drawn from then
on, in a new colour on the page where it appears.

**Selectors** choose one of two intersection points:

- `except P`: the point other than `P`;
- `nearest P` and `farthest P`: by distance from `P`;
- `left A B` and `right A B`: on that side of the directed line `A → B`.

`P` can be a point name or a literal such as `(0, 1)`.

**Strings** have three escapes: `\"`, `\\` and `\n`. Any other backslash is
kept, so Typst escapes such as `\*` work in notes. Notes are Typst markup in the
PDF (`$eta_0 = c_1 + c_4$` is mathematics) and are shown as source on the page.

**Pages:** each `snapshot` ends a page, and the steps after the last one form
the final page. Consecutive snapshots count once, so the PDF has one page more
than the script has snapshots. Each page shows the figure after its last step;
when the page adds several objects, each gets its own colour, used for its name
too.

**Predicates:**

- `on P x`: `P` lies on the line or circle `x`;
- `P == Q` and `P != Q`;
- `dist A B == dist C D`, also with `!=`, `<`, `<=`, `>` and `>=`;
- `collinear A B C …` and `concyclic A B C D …`;
- `parallel l m` and `perpendicular l m`;
- `angle A B C == angle D E F`;
- `ccw A B C`: the points turn counterclockwise;
- `between A B C`: `B` lies strictly between `A` and `C`.

**Constructions** (macros) are defined with `def`. They can call each other and
contain assertions:

```
def nextVertex(P, Q, u) -> R:
  k = circle Q P
  R = meet k u except P
end
```

**Built-in constructions** are written in the same language, so every one of
them reduces to straightedge-and-compass steps:

- `perpBisector(A, B)`;
- `midpoint(A, B)`;
- `circleDiameter(A, B)`;
- `perpAt(A, B, P)`: the perpendicular to the line `AB` at its point `P` (Euclid I.11);
- `perpFrom(A, B, P)`: the perpendicular from `P` to the line `AB` (Euclid I.12);
- `parallelThrough(A, B, P)`;
- `angleBisector(A, O, B)` (Euclid I.9);
- `transfer(O, A, B)`: the circle with centre `O` and radius `AB` (Euclid I.2).

### What a check proves

The checks are exact computations for the given coordinates. With **two given
points**, this proves the claims in general: every straightedge-and-compass
construction commutes with similarities, so any other choice of the two points
gives a similar figure. With three or more given points, the checks prove the
claims for that configuration only. The tool says which case applies.

## Universal statements

A script with variables proves a statement for all values of the variables at
once.

```
given A = (0, 0)
given B = (1, 0)
param p ~ 1/3               -- a real variable; 1/3 is the value drawn
param q ~ 3/4
given C = (p, q)            -- by a similarity, any triangle
assume ccw A B C            -- a hypothesis: q > 0
…
assert collinear O G H      -- proved for all p and q > 0
```

| Statement | Meaning |
|---|---|
| `param p ~ 1/3` | A real variable, with the value used for drawing. |
| `given C = (p, q)` | A given point whose coordinates are variables or rationals. |
| `P = point on u ~ (0, 1)` | Any point of the line or circle `u`, drawn near `(0, 1)`. |
| `assume PREDICATE` | A hypothesis: `ccw`, a `dist` comparison, `!=`, or an equation such as `on P u`. |
| `cases meet u k` | A split with the branches `none:`, `one T:`, `two T1, T2:` and `same:`. |
| `cases side P A B` | `left:`, `on:`, `right:` |
| `cases compare dist A B, dist C D` | `less:`, `equal:`, `greater:` |
| `cases P == Q`, `cases PREDICATE` | `equal:`/`different:`, or `yes:`/`no:` |
| `end` | Closes the cases; later statements continue in every branch. |
| `sample p ~ 2/3` | Other values to draw from here on. |
| `snapshot p ~ 2/3` | A page break, with other values from here on. |

**What the tool proves.** The numbers are towers of square roots over the
rational functions in the variables. Every decision of the construction is
settled for all values at once:

- **Computed hypotheses.** A degenerate case (a line through two equal points,
  parallel lines) or the existence of two intersection points becomes a
  hypothesis of the theorem, stated with its step: for the nine-point circle,
  "A ≠ H: p ≠ 0" excludes the right angle at A. Hypotheses that follow from the
  others are dropped.
- **Claims.** An equality must be an identity in the variables. A sign (`ccw`,
  `between`, `dist <`) is decided by the sign engine from the hypotheses.
- **Sign engine.** It uses factors and squares, sums of squares, and the signs
  of the hypotheses. Every sign it proves comes with a certificate that Lean
  checks. In one variable it finds two configurations with opposite signs
  exactly. When it cannot decide, the error message shows them.
- **Choices.** When a selector, a lexicographic order or a sign depends on the
  values, the tool reports it and suggests a `cases` split.
- **Splits.** The branches of `cases` together cover every possibility. A
  branch you leave out must be impossible; if it is not, the error gives an
  example.
- **Equation branches.** A branch whose condition is an equation (`one`,
  `equal`, `on`) is solved for a variable, and the solution is substituted.
  For the tangents, `one` gives the sub-cases p = 1 and p = −1. A solution that
  belongs to another case is ruled out: for two equal circles, d = 0 solves the
  touching equation but is the case `same`.

Each case has a sample configuration (its values in force), which is checked
exactly against the case's hypotheses. If a case has no sample of its own, the
tool finds one. So no case is empty.

**On the page:**

- Sliders set the variables and the positions of free points: an angle on a
  circle, measured from its through-point, or the parameter t on a line.
  Points with variable coordinates, and free points, can also be dragged.
- The case menu draws the sample of each case.
- The figure follows the case the current values fall in, and says when they
  are outside the theorem, for example when an assumption fails.
- The step panel shows the hypotheses each step needs, and why each claim
  holds for all values.

**In the PDF:**

- Page 1 states the theorem: the variables, the hypotheses, and the claims
  case by case.
- Every case gets a section, with its figures drawn at its sample values.

**Limits:**

- The conditions of `cases` and `assume` must be polynomial in the variables,
  with no square roots.
- An equation branch must be solvable for one variable: linear in it, or with
  rational roots.

## Using the page

- **The script** is on the left. It is re-checked as you type, and an error
  marks its line. Your draft is kept in the browser.
- **Playback** is on the right. Use ⏮ ◀ ▶ ⏭, the slider (its ticks mark the
  snapshots), or the keys ← → Home End.
  - *by page* steps from page to page and colours the figure as the PDF does;
  - *macro steps* shows the inside of a construction call;
  - the wheel, **+** and **−** zoom, dragging pans, and **Fit**, a double-click
    or the key `0` restores the view.
- **The console** spans the page below both panes. It accepts statements,
  which are added to the script, and these commands:
  - `show X`, `goto N`, `steps`, `undo`, `clear`, `help`;
  - `export pdf`, `export typst`, `export lean`.
- **Examples, Open, Save** load and store `.sc` files.
- **PDF** compiles the proof with [Typst](https://typst.app/) inside the browser.
  The first export downloads the compiler, about 28 MB, from jsDelivr.
- **Lean** downloads the core export for the Lean side.
- **Help** explains the script language and the controls.

### The PDF

Page 1 opens with the title, the introduction, the given points and the
claims. Each page then shows its figure and its steps: the source line, the
notes, every new object with its exact value and a decimal approximation, and
every check. The last page ends with a summary.

Every page fits one sheet. A page with too much on it gets a smaller figure,
then two columns, and only then smaller type; add a `snapshot` to split it.
Exact values use typeset radicals, nested where needed. Typst cannot break a
formula inside a radical, so a long radicand is named (ρ₁, ρ₂, …) and defined
under the step where it first appears.

## The Lean side

`examples/heptadecagon/Heptadecagon.lean` contains one command:

```lean
construction_proof heptadecagon from "examples/heptadecagon/heptadecagon.core.json"
```

It produces a theorem of this form:

```lean
theorem heptadecagon :
    ∃ «O» «P0» «P0'» «P#7» «Q#8» «A» … «P17» : StraightedgeCompass.P2,
      «O» = !₂[(0 : ℝ), (0 : ℝ)] ∧ «P0» = !₂[(1 : ℝ), (0 : ℝ)] ∧ … ∧
      «P1» ∈ lineThrough «P#75» «Q#76» ∧ «P1» ∈ circleThrough «O» «P0» ∧ … ∧
      «P17» = «P0» ∧ dist «P1» «P2» = dist «P0» «P1» ∧ … ∧ IsLeft «P0» «P1» «P16»
```

Here `P2` is `EuclideanSpace ℝ (Fin 2)`, `lineThrough A B = line[ℝ, A, B]` and
`circleThrough O A = Metric.sphere O (dist O A)`. Points made inside a
construction call are named `«name#id»`. The theorem lists every
construction step: the given coordinates, the incidences, the non-degeneracy
conditions, and which intersection point was chosen. Then it lists every claim.

The proof is by reflection:

- **Hints:** the export carries every point's coordinates as integer trees over
  one common tower of square roots. They are hints only.
- **Kernel checks:** each conjunct follows from one Boolean check that the
  kernel evaluates (`decide +kernel`). The checks use ring operations and signs
  only.
- **Soundness:** `StraightedgeCompass.Tower.Sound` proves once and for all that
  these operations agree with real arithmetic.
- **Axioms:** the theorems depend only on `propext`, `Classical.choice` and
  `Quot.sound` (no `native_decide`).

Every predicate has a statement in Mathlib's terms:

| Predicate | Statement |
|---|---|
| `on P x` | `P ∈ lineThrough A B` or `P ∈ circleThrough O A` |
| `P == Q`, `P != Q` | `P = Q`, `P ≠ Q` |
| `dist A B ⋈ dist C D` | `dist A B ⋈ dist C D` |
| `ccw A B C` | `IsLeft A B C` (a positive cross product) |
| `between A B C` | `Sbtw ℝ A B C` |
| `collinear A B C …` | `Collinear ℝ {A, B, C, …}` |
| `concyclic A B C D …` | `EuclideanGeometry.Concyclic {A, B, C, D, …}` |
| `parallel l m` | `(lineThrough A B).Parallel (lineThrough C D)` |
| `perpendicular l m` | `(lineThrough A B).direction ⟂ (lineThrough C D).direction` |
| `angle A B C == angle D E F` | `∠ A B C = ∠ D E F` (unoriented angles) |

For the 17-gon, elaboration takes about 35 seconds.

### Universal statements in Lean

For a script with variables, the same command gives one theorem per case of
the proof, with the hypotheses of the PDF:

```lean
theorem tangentsFromAPoint.case_two :
    ∀ (p : ℝ), p ^ 2 - 1 > 0 →
      ∃ «O» «A» «P» … «T1» «T2» : StraightedgeCompass.P2,
        «O» = !₂[0, 0] ∧ «A» = !₂[1, 0] ∧ «P» = !₂[p, 0] ∧ … ∧
        «T1» ∈ circleThrough «O» «A» ∧ … ∧ dist «P» «T1» = dist «P» «T2»
```

A substituted variable is not quantified (`case_one_p_eq_1` is about the
point P = (1, 0)), and a free point is stated by its parameter, as
`«P» = rotAbout «O» «B» t_P` or `«P» = AffineMap.lineMap «A» «B» t_P`. Every
`cases` statement also gets a coverage theorem, which says that its cases
cover every configuration:

```lean
theorem tangentsFromAPoint.cover :
    ∀ (p : ℝ), p ≠ 0 → p ^ 2 - 1 > 0 ∨ p = -1 ∨ p = 1 ∨ p ^ 2 - 1 < 0
```

The proofs are again by reflection:

- **Points:** every point has polynomial coordinates x / d, y / d in the
  variables and in the square roots of the case.
- **Certificates:** every fact the statement needs (a point on a line, a sign,
  a nonzero denominator) is derived from the hypotheses by a chain of
  polynomial identities, which the kernel checks in one `decide +kernel`.
  The sign engine of the tool writes these certificates; the signs of
  a + b√r follow from the signs of a, b and of the norm a² − r·b².
- **Coverage:** a decision tree on the signs of polynomials. Every branch ends
  in one of the cases or in a contradiction.

The examples take from a few seconds (Thales) to about a minute (Napoleon).
The format is in `spec/universal-format.md`.

**Limits:**

- The theorem is an existence statement. It does not yet say that each line or
  circle intersection has at most the points listed.

`test/check-core.mjs` checks the same exported conditions independently in
JavaScript, which is faster than Lean when you work on the format. For
universal exports it re-checks every certificate (`test/check-universal.mjs`).

## Development

**Prerequisites:**

- Node.js 26 (`.nvmrc`; for example `nvm install && nvm use`);
- for Lean, [elan](https://github.com/leanprover/elan).

```sh
npm ci              # PureScript compiler, spago, esbuild (pinned in package.json)
npm run dev         # http://localhost:3000, rebuilt on change
npm test            # unit tests (spago test)
npm run check       # check the example scripts
npm run core        # write examples/*/*.core.json and re-check them independently
npm run bench       # time the 17-gon construction
npm run release     # minified build into docs/ (the published site)

node src/cli.mjs check FILE.sc
node src/cli.mjs pdf FILE.sc [OUT.pdf]
node src/cli.mjs typst FILE.sc [OUT.typ]
node src/cli.mjs core FILE.sc

lake exe cache get  # Mathlib's compiled files
npm run lean        # the library and the example theorems (lake build)
```

Lake does not see the `.core.json` files that `construction_proof` reads, so a
plain `lake build` can replay an old result after `npm run core`. `npm run lean`
removes the example theorems' build outputs first.

**Publishing:** GitHub Pages serves the committed `docs/` folder (branch
`master`, folder `/docs`). There is no CI. After a change:

1. `npm run core` if an example changed, then `npm run lean`;
2. `npm run release`;
3. commit `docs/` and `examples/` with the sources, and push.

**Layout:**

| Path | Contents |
|---|---|
| `src/Constructible/` | Exact arithmetic: rationals over `BigInt`, towers of square roots, polynomials and rational functions, towers over ℚ(x) |
| `src/Geometry.purs` | Lines, circles and intersections |
| `src/Proof/` | The script language: parser, evaluator, predicates, built-ins; `Prove`, `Signs` and `Cert` for universal statements |
| `src/Render/`, `src/Export/` | Figures, text, Typst, core and universal exports |
| `src/App/` | The web page (Halogen, CodeMirror) |
| `examples/<name>/` | One example: the script, its core export and its Lean theorem |
| `StraightedgeCompass/` | Lean: tower arithmetic, soundness, geometry bridge, polynomials and certificates (`Poly`, `Cert`, `UGeometry`), the elaborator |
| `spec/core-format.md` | The exchange format between the tool and Lean |
| `spec/universal-format.md` | The exchange format for universal statements |

## Credits

- `src/Constructible/Field.purs` is a port of the Haskell library
  [constructible](https://github.com/andersk/haskell-constructible) by Anders
  Kaseorg (BSD-3-Clause; see `NOTICE`). The first version of this project, a
  Haskell REPL from 2019, used that library directly.
- The pentagon, 15-gon and 17-gon use Carlyle circles, after
  L. DeTemple, *Carlyle circles and the Lemoine simplicity of polygon
  constructions*, American Mathematical Monthly 98 (1991).

BSD-3-Clause; see `LICENSE`.
