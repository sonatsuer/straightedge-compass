# Universal export format, version 1

The universal export is the file that the web tool and the CLI write for a
script with variables (`param`, variable coordinates, or free points), and
that the Lean elaborator reads. It is produced by `src/Export/Universal.purs`:

```
node src/cli.mjs core examples/thales/thales.sc    # writes examples/thales/thales.core.json
```

It has the same file name as a core export (`spec/core-format.md`); the
`format` field tells them apart, and `construction_proof` reads both.

The file contains:

- one **case** (a leaf of the proof tree) per theorem: its variables and
  hypotheses, its points with polynomial coordinates, the conjuncts of its
  statement, and a chain of sign certificates that proves every fact the
  conjuncts need;
- one **cover** per `cases` statement: its hypotheses, its cases as lists of
  conditions, and a decision tree that proves that the cases cover every
  configuration.

A consumer must not trust any of it. It re-checks every certificate, as
`test/check-universal.mjs` and the Lean kernel (`checkGoals`, `checkCover` in
`StraightedgeCompass/Cert.lean`) do: with polynomial arithmetic only.

## Top level

```json
{
  "format": "straightedge-compass-universal",
  "version": 1,
  "title": "…" | null,
  "leaves": [Leaf, …],
  "covers": [Cover, …]
}
```

## Polynomials and facts

**Variables** are numbered from 0: first the variables of the case (the
script's variables and free-point parameters that are not substituted, in
their order), then one variable per square root of the case's tower.

**A polynomial** has integer coefficients:

```json
[[exponents, "coeff"], …]
```

`exponents` lists the exponent of each variable from 0 (trailing zeros may
be dropped); `"coeff"` is an integer string. `[[[2, 1], "3"], [[], "-1"]]` is
3·x₀²·x₁ − 1.

**A fact** is a polynomial with a relation to zero:

```json
{ "p": Polynomial, "rel": "pos" | "nonneg" | "nonzero" | "zero" }
```

## Chains

A chain proves facts in order. The facts known at the start are the
hypotheses; every step appends facts. A fact is referred to by its index in
the list of facts so far.

```json
{
  "nparams": 2,
  "hyps": [Fact, …],
  "sqrts": [[n, d], …],
  "steps": [Step, …]
}
```

A step is one of:

- **A fact with a certificate:** `{ "fact": Fact, "cert": Cert }`.
- **A square root:** `{ "sqrt": { "n": P, "d": P, "rad": i, "den": j } }`. It
  must match the next entry of `sqrts` and use the variables so far only. It
  needs fact `i`, n·d ≥ 0 (or > 0), and fact `j`, d > 0, with exactly these
  polynomials. It adds the next variable s = √(n / d) and two facts:
  d·s² − n = 0 and s ≥ 0 (s > 0 if fact `i` is > 0).

A **term** is a product `c · Π refs · Π sqrefs² · sq²`:

```json
{ "c": "k", "refs": [i, …], "sqrefs": [j, …], "sq": Polynomial }
```

with `c` a positive integer string, `refs` facts ≥ 0 (or > 0), `sqrefs` facts
≠ 0 (or > 0). It is ≥ 0. It is **strict** (> 0) when every ref is > 0 and `sq`
is a nonzero constant. It is **nonzero** when every ref and sqref is ≠ 0 (or
> 0) and `sq` is a nonzero constant.

A certificate is one of:

| Certificate | Identity | Proves |
|---|---|---|
| `{ "sum": { "mult": m, "terms": [t, …], "eqs": [e, …] } }` | m · p = Σ t + Σ c·z | p > 0 (or ≠ 0) with a strict term and m ≥ 0; p ≥ 0 with m strict; p = 0 with no terms and m nonzero |
| `{ "nonzero": { "m": P, "c": "k", "refs": [i, …], "eqs": [e, …] } }` | p · m = k · Π refs + Σ c·z | p ≠ 0, with k ≠ 0 and the refs ≠ 0 |
| `{ "strict": [i, j] }` | | p > 0 from fact i, p ≥ 0, and fact j, p ≠ 0 (both exactly p) |

An equation term `e` is `{ "c": Polynomial, "ref": z }` with fact `z` = 0.
The identities are polynomial identities.

## Leaves

```json
{
  "name": "case_two" | "",
  "label": "two" | "one (p = 1)" | "",
  "params": ["p", …],
  "hyps": [Hypothesis, …],
  "chain": Chain,
  "upts": [{ "name": "u3", "def": UPt }, …],
  "points": [{ "id": 3, "binder": "T1", "upt": "u3" }, …],
  "literals": [Literal, …],
  "goals": [Goal, …],
  "conjuncts": [Conjunct, …]
}
```

`name` is the Lean name of the case (the theorem `name.case_two`, or `name`
itself for a script without `cases`); equation cases are named after their
solution (`case_one_p_eq_neg1`).

**Hypotheses** are those of the PDF, in its order. Each has a readable form
and the scale of its fact (`chain.hyps`, same order):

```json
{ "stmt": "p ^ 2 - 1 > 0", "text": "u and k meet in two points", "kind": "pos", "c": "1", "expr": "p ^ 2 - 1" }
{ "stmt": "p ≠ 1", "kind": "ne", "c": "2", "v": "p", "k": "1" }
```

The fact's polynomial evaluates to c · expr (kinds `pos`, `nonneg`,
`nonzero`), to c · (−expr) (kinds `neg`, `nonpos`), or to c · (v − k) (kind
`ne`), with c > 0 (c ≠ 0 for `nonzero` and `ne`).

**Points** have polynomial coordinates x / d, y / d. A UPt is
`{ "x": P, "y": P, "d": P }`, or a free point:
`{ "along": ["u1", "u2"], "t": i }` (A + tᵢ·(B − A) on the line A B) or
`{ "rot": ["u1", "u2"], "t": i }` (B turned about O by tan(θ/2) = tᵢ), as
`UPt.alongU` and `UPt.rotU` compute them. `points` lists the points of the
statement in order. Auxiliary points have their own names: `o5`, the other
intersection point of object 5; `r5`, the literal reference point of object
5; `z0`, the centre of a concyclic claim.

**Goals** are the facts the conjuncts need, computed from the points:

```json
{ "fn": "crossN", "args": ["u1", "u2", "u5"], "neg": "no", "rel": "zero", "index": 41 }
```

`fn` is a function of `StraightedgeCompass.UPt` (`dist2N`, `crossN`, `circN`,
`cmpN`, `leftN`, `lexXN`, `lexYN`, `dxN`, `dyN`, `crossDirN`, `dotDirN`,
`dotSignN`, `angleN`), or `d` for the denominator of a point. With `neg`, the
polynomial is negated. The goal must be fact `index` of the chain, with the
same polynomial and the same (or a stronger) relation.

**Conjuncts** follow the objects in creation order, then the claims. Each
names its points by UPt name and its goals by position in `goals`:

| Kind | Statement |
|---|---|
| `given` | X = !₂[x, y], with each coordinate `{ expr, num, den, k, c }`: num / den, and den · k = c · d |
| `along`, `rot` | X = AffineMap.lineMap A B t, X = rotAbout O B t |
| `ne` | A ≠ B |
| `mem` | X ∈ lineThrough A B, X ∈ circleThrough O A |
| `lineNe` | lineThrough A B ≠ lineThrough C D |
| `other` | ∃ Y on both curves with the choice condition (`lexX`, `lexY`, `nearest`, `farthest`, `left`) |
| `eq`, `dist`, `collinear3`, `collinear`, `concyclic`, `parallel`, `perp`, `angle`, `ccw`, `between` | the claims, as for core exports |

A **literal** reference point (`nearest (0, 1)`) appears in the statement as
`!₂[x, y]`; `{ "upt", "stmt", "x", "y", "poly", "dgoal" }` gives its point.

## Covers

```json
{
  "name": "cover" | "case_two.cover",
  "label": "…",
  "split": "cases meet u k",
  "params": ["p"],
  "hyps": [Hypothesis, …],
  "facts": [Fact, …],
  "nparams": 1,
  "root": [Step, …],
  "tree": Tree,
  "cases": [{ "label": "two", "conds": [Condition, …] }, …],
  "D": [[Fact, …], …]
}
```

The theorem is: for all values of `params` with the hypotheses, one of the
cases holds; case k is the conjunction of its conditions, whose facts are
`D[k]`. Conditions are hypotheses (as above), or a substitution:
`{ "kind": "eq", "v": "p", "k": "1", "c": … }` (p = 1, the fact is c·(v − k))
or `{ "kind": "eqdiv", "v", "n", "m", "c" }` (v = n / m, the fact is
c·(v·m − n), after the condition m ≠ 0).

The facts start as `facts`; `root` derives the hypotheses that the PDF drops.
A tree is:

- `{ "split": P, "neg": T, "zero": T, "pos": T }`: the subtrees go on with the
  fact −P > 0, P = 0, or P > 0;
- `{ "chain": [Step, …], "next": T }`: more facts;
- `{ "case": k, "steps": [Step, …], "idx": [i, …] }`: after the steps, the
  facts `idx` are the conditions `D[k]`;
- `{ "absurd": [i, j], "steps": [Step, …] }`: after the steps, facts i and j
  contradict: i is P = 0 and j is P ≠ 0 (or P > 0), or i is P > 0 and j is
  −P ≥ 0.

Covers have no square roots.

## Compatibility

A consumer must reject files whose `format` or `version` it does not know. A
change that alters the meaning of an existing field increases `version`.
