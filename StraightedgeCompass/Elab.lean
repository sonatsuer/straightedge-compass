import Lean
import StraightedgeCompass

/-!
# The `construction_proof` command

```
construction_proof euclidI1 from "examples/euclid-i-1.core.json"
```

reads a core export of the web tool (see `spec/core-format.md`) and adds:

* `euclidI1.T`, `euclidI1.p7`, …: the tower and the hinted coordinates of every
  point (untrusted data);
* `euclidI1.T_wf`: the tower is well formed (checked by the kernel);
* `euclidI1`: the theorem. It says that there are points of the Euclidean plane
  that satisfy every construction step (given coordinates, incidences with the
  lines and circles drawn, non-degeneracy, which intersection point was
  chosen) and every assertion of the script, in Mathlib's terms: `dist`,
  `Collinear`, `Sbtw`, `AffineSubspace.Parallel`, orthogonal directions,
  `EuclideanGeometry.angle` and `EuclideanGeometry.Concyclic`.

The statement is written in Mathlib's terms: points are
`EuclideanSpace ℝ (Fin 2)`, lines are `line[ℝ, A, B]` (`lineThrough A B`),
circles are `Metric.sphere O (dist O A)` (`circleThrough O A`). Each conjunct is
proved by a lemma of `StraightedgeCompass.Geometry` from one Boolean check
evaluated by the kernel (`decide +kernel`), so the proof uses no axioms beyond
Lean's standard ones.

The path is relative to the package root (the working directory of
`lake build` and of the editor's Lean server).
-/

open Lean Elab Command

namespace StraightedgeCompass.Elab

/-- Errors in the input, reported at the command. -/
abbrev M := Except String

def field (j : Json) (k : String) : M Json := j.getObjVal? k

def str (j : Json) (k : String) : M String := do (← field j k).getStr?

def nat (j : Json) (k : String) : M Nat := do (← field j k).getNat?

def arr (j : Json) (k : String) : M (Array Json) := do (← field j k).getArr?

def nats (j : Json) (k : String) : M (Array Nat) := do (← arr j k).mapM (·.getNat?)

/-- Lean source for a tree. -/
partial def treeText (j : Json) : M String :=
  match j with
  | .str s => do
    let some n := s.toInt? | throw s!"not an integer: {s}"
    return if n < 0 then s!"(.leaf ({n}))" else s!"(.leaf {n})"
  | .arr #[a, b] => return s!"(.node {← treeText a} {← treeText b})"
  | _ => throw s!"not a tree: {j.compress}"

/-- Lean source for an element `{ num, den }`. -/
def eltText (j : Json) : M String := do
  let den ← str j "den"
  let some d := den.toNat? | throw s!"not a positive integer: {den}"
  if d == 0 then throw "zero denominator"
  return s!"⟨{← treeText (← field j "num")}, {d}⟩"

def ptText (j : Json) : M String := do
  return s!"⟨{← eltText (← field j "x")}, {← eltText (← field j "y")}⟩"

/-- A rational `"p"` or `"p/q"` as numerator and denominator. -/
def ratParts (s : String) : M (Int × Nat) :=
  match s.splitOn "/" with
  | [n] => do
    let some a := n.toInt? | throw s!"not a rational: {s}"
    return (a, 1)
  | [n, d] => do
    let some a := n.toInt? | throw s!"not a rational: {s}"
    let some b := d.toNat? | throw s!"not a rational: {s}"
    if b == 0 then throw s!"zero denominator: {s}"
    return (a, b)
  | _ => throw s!"not a rational: {s}"

/-- A rational as a real-number literal. -/
def ratReal (s : String) : M String := do
  let (a, b) ← ratParts s
  return if b == 1 then s!"({a} : ℝ)" else s!"({a} / {b} : ℝ)"

structure Obj where
  id : Nat
  name : String
  named : Bool
  kind : String
  defn : Json

structure Gen where
  /-- `def`s of hint data -/
  data : Array String := #[]
  /-- bound points: binder name and witness -/
  binders : Array (String × String) := #[]
  /-- conjuncts: statement and proof -/
  conjuncts : Array (String × String) := #[]

def Gen.add (g : Gen) (c : String × String) : Gen := { g with conjuncts := g.conjuncts.push c }

/-- Builds the commands for `construction_proof name from file`. -/
def generate (name : String) (core : Json) : M (Array String) := do
  unless (← str core "format") == "straightedge-compass-core" do throw "not a core export"
  unless (← nat core "version") == 1 do throw "unsupported core export version"
  let ns := s!"«{name}»"
  let T := s!"{ns}.T"
  let wf := s!"{ns}.T_wf"
  let tower ← arr core "tower"
  -- the tower, top level first
  let towerText ← (tower.reverse.mapM fun r => do treeText (← field r "num"))
  let objsJson ← arr core "objects"
  let objs ← objsJson.mapM fun o => do
    return ({ id := ← nat o "id", name := ← str o "name"
              named := (← str o "named") == "yes", kind := ← str o "kind"
              defn := ← field o "def" } : Obj)
  let byId : Std.HashMap Nat Obj := objs.foldl (fun m o => m.insert o.id o) {}
  let get (i : Nat) : M Obj := match byId[i]? with
    | some o => pure o
    | none => throw s!"unknown object {i}"
  -- binder names: script names for named points, `name#id` for the others;
  -- names of the library's own constants get primes so they are not shadowed
  let reserved := ["lineThrough", "circleThrough", "IsLeft", "LexLt"]
  let rec unshadow (n : String) (fuel : Nat) : String :=
    match fuel with
    | 0 => n
    | k + 1 => if reserved.contains n then unshadow (n ++ "'") k else n
  let binder (o : Obj) : String :=
    if o.named then s!"«{unshadow o.name 10}»" else s!"«{o.name}#{o.id}»"
  let pt (i : Nat) := s!"{ns}.p{i}"
  let other (i : Nat) := s!"{ns}.q{i}"
  let pointName (i : Nat) : M String := do
    let o ← get i
    unless o.kind == "point" do throw s!"{o.name} is not a point"
    return binder o
  -- the line or circle an object stands for, in terms of point binders
  let curve (i : Nat) : M String := do
    let o ← get i
    match o.kind with
    | "line" =>
      let ps ← nats o.defn "line"
      return s!"lineThrough {← pointName ps[0]!} {← pointName ps[1]!}"
    | "circle" =>
      let ps ← nats o.defn "circle"
      return s!"circleThrough {← pointName ps[0]!} {← pointName ps[1]!}"
    | _ => throw s!"{o.name} is not a line or a circle"
  -- a proof that a point lies on the object `i`
  let memProof (i : Nat) : M String := do
    let o ← get i
    match o.kind with
    | "line" => return s!"mem_line_of_check {wf} (by decide +kernel)"
    | "circle" => return s!"mem_circle_of_check {wf} (by decide +kernel)"
    | _ => throw s!"{o.name} is not a line or a circle"
  let check := "(by decide +kernel)"
  -- the other intersection point; no script name contains a space
  let Y := "«other point»"
  let mut g : Gen := {}
  for oj in objsJson do
    let o ← get (← nat oj "id")
    if o.kind != "point" then
      -- non-degeneracy of the drawing
      let key := if o.kind == "line" then "line" else "circle"
      let ps ← nats o.defn key
      let a ← pointName ps[0]!
      let b ← pointName ps[1]!
      g := g.add (s!"{a} ≠ {b}", s!"ne_of_check {wf} {check}")
      continue
    let coords ← field oj "coords"
    g := { g with
      data := g.data.push s!"def {pt o.id} : Pt := {← ptText coords}"
      binders := g.binders.push (binder o, s!"Pt.toP2 {T} {pt o.id}") }
    let X := binder o
    match o.defn.getObjVal? "given" with
    | .ok r =>
      let (a, c) ← ratParts (← str r "x")
      let (b, d) ← ratParts (← str r "y")
      let stmt := s!"{X} = !₂[{← ratReal (← str r "x")}, {← ratReal (← str r "y")}]"
      let prf := s!"by simpa using given_of_check (T := {T}) (p := {pt o.id}) (a := {a}) " ++
        s!"(b := {b}) (c := {c}) (d := {d}) {check} (by decide) (by decide)"
      g := g.add (stmt, prf)
    | .error _ =>
      let ab ← nats o.defn "meet"
      let (ia, ib) := (ab[0]!, ab[1]!)
      let A ← curve ia
      let B ← curve ib
      g := g.add (s!"{X} ∈ {A}", ← memProof ia)
      g := g.add (s!"{X} ∈ {B}", ← memProof ib)
      -- the two objects are distinct
      let oa ← get ia
      let ob ← get ib
      if oa.kind == "line" && ob.kind == "line" then
        g := g.add (s!"{A} ≠ {B}", s!"by first | exact line_ne_of_check {wf} {check} " ++
          s!"| exact line_ne_of_check' {wf} {check}")
      else if oa.kind == "circle" && ob.kind == "circle" then
        let ca := (← nats oa.defn "circle")[0]!
        let cb := (← nats ob.defn "circle")[0]!
        let stmt := s!"{← pointName ca} ≠ {← pointName cb}"
        g := g.add (stmt, s!"ne_of_check {wf} {check}")
      -- which of the intersection points
      let choice ← field o.defn "choice"
      let kind ← str choice "kind"
      let withOther (cond : String) (condProof : String) : M Gen := do
        let oc ← field o.defn "other"
        let g' := { g with data := g.data.push s!"def {other o.id} : Pt := {← ptText oc}" }
        -- parenthesised: `∃` would otherwise extend over the following conjuncts
        let stmt := s!"(∃ {Y} : StraightedgeCompass.P2, {Y} ∈ {A} ∧ {Y} ∈ {B} ∧ {cond})"
        let prf := s!"⟨Pt.toP2 {T} {other o.id}, {← memProof ia}, {← memProof ib}, {condProof}⟩"
        return g'.add (stmt, prf)
      -- The reference point of a selector: a point binder, or a literal
      -- `!₂[x, y]` with its tower point (a `def`) and a rewrite of the
      -- literal into that point for the proofs.
      let refPoint : M (String × Option (String × String)) := do
        let r ← field choice "ref"
        match r.getObjVal? "object" with
        | .ok i => do let i ← i.getNat?; return (← pointName i, none)
        | .error _ =>
          let p ← field r "point"
          let (a, c) ← ratParts (← str p "x")
          let (b, d) ← ratParts (← str p "y")
          let lit := s!"!₂[{← ratReal (← str p "x")}, {← ratReal (← str p "y")}]"
          let rp := s!"{ns}.r{o.id}"
          let dataDef := s!"def {rp} : Pt := {← ptText (← field r "hint")}"
          let rw := s!"have hr : ({lit} : StraightedgeCompass.P2) = Pt.toP2 {T} {rp} := " ++
            s!"(by simpa using (given_of_check (T := {T}) (p := {rp}) (a := {a}) (b := {b}) " ++
            s!"(c := {c}) (d := {d}) {check} (by decide) (by decide)).symm)"
          return (lit, some (dataDef, rw))
      -- a proof about the reference point, after rewriting a literal
      let viaRef (ref : Option (String × String)) (prf : String) : String :=
        match ref with
        | none => prf
        | some (_, rw) => s!"(by {rw}; rw [hr]; exact {prf})"
      let refData (ref : Option (String × String)) (g : Gen) : Gen :=
        match ref with
        | none => g
        | some (d, _) => { g with data := g.data.push d }
      match kind with
      | "unique" => pure ()
      | "lexFirst" => g ← withOther s!"LexLt {X} {Y}" s!"lexLt_of_check {wf} {check}"
      | "lexSecond" => g ← withOther s!"LexLt {Y} {X}" s!"lexLt_of_check {wf} {check}"
      | "except" =>
        let (R, ref) ← refPoint
        g := refData ref g
        g := g.add (s!"{R} ∈ {A}", viaRef ref (← memProof ia))
        g := g.add (s!"{R} ∈ {B}", viaRef ref (← memProof ib))
        g := g.add (s!"{R} ≠ {X}", viaRef ref s!"ne_of_check {wf} {check}")
      | "nearest" =>
        let (R, ref) ← refPoint
        -- `withOther` starts from the generator as it was: add the data after
        g ← withOther s!"dist {X} {R} < dist {Y} {R}" (viaRef ref s!"dist_lt_of_check {wf} {check}")
        g := refData ref g
      | "farthest" =>
        let (R, ref) ← refPoint
        g ← withOther s!"dist {Y} {R} < dist {X} {R}" (viaRef ref s!"dist_lt_of_check {wf} {check}")
        g := refData ref g
      | "left" | "right" =>
        let pq ← nats choice "line"
        let (P, Q) := (← pointName pq[0]!, ← pointName pq[1]!)
        let (P, Q) := if kind == "left" then (P, Q) else (Q, P)
        g ← withOther s!"IsLeft {P} {Q} {X} ∧ ¬IsLeft {P} {Q} {Y}"
          s!"isLeft_of_check {wf} {check}, not_isLeft_of_check {wf} {check}"
      | k => throw s!"unknown choice {k}"
  -- assertions
  let asserts ← arr core "asserts"
  for i in [0:asserts.size] do
    let aj := asserts[i]!
    let pred ← field aj "pred"
    let kind ← str pred "kind"
    let args ← nats pred "args"
    let P (k : Nat) : M String := pointName args[k]!
    -- `{A, B, C}` and a proof of `∀ p ∈ {A, B, C}, goal p` from one lemma
    let pointSet : M String := do
      return s!"(\{{", ".intercalate (← args.toList.mapM pointName)}} : Set StraightedgeCompass.P2)"
    let forEach (lem : String) : String :=
      let alts := " | ".intercalate (List.replicate args.size "rfl")
      s!"(by intro p hp; simp only [Set.mem_insert_iff, Set.mem_singleton_iff] at hp; " ++
        s!"rcases hp with {alts} <;> exact {lem} {wf} {check})"
    let (stmt, proof) ← match kind with
      | "on" => do pure (s!"{← P 0} ∈ {← curve args[1]!}", ← memProof args[1]!)
      | "eq" => do pure (s!"{← P 0} = {← P 1}", s!"eq_of_check {check}")
      | "ne" => do pure (s!"{← P 0} ≠ {← P 1}", s!"ne_of_check {wf} {check}")
      | "ccw" => do pure (s!"IsLeft {← P 0} {← P 1} {← P 2}", s!"isLeft_of_check {wf} {check}")
      | "between" => do
        pure (s!"Sbtw ℝ {← P 0} {← P 1} {← P 2}", s!"sbtw_of_check {wf} {check}")
      | "parallel" => do
        pure (s!"({← curve args[0]!}).Parallel ({← curve args[1]!})",
          s!"parallel_of_check {wf} {check}")
      | "perpendicular" => do
        pure (s!"({← curve args[0]!}).direction ⟂ ({← curve args[1]!}).direction",
          s!"perp_of_check {wf} {check}")
      | "angle" => do
        pure (s!"EuclideanGeometry.angle {← P 0} {← P 1} {← P 2} = " ++
            s!"EuclideanGeometry.angle {← P 3} {← P 4} {← P 5}",
          s!"angle_eq_of_check {wf} {check}")
      | "collinear" => do
        pure (s!"Collinear ℝ {← pointSet}",
          s!"collinear_of_mem_line (A := Pt.toP2 {T} {pt args[0]!}) " ++
            s!"(B := Pt.toP2 {T} {pt args[1]!}) (by simp) {forEach "mem_line_of_check"}")
      | "concyclic" => do
        let c ← field aj "centre"
        let z := s!"{ns}.z{i}"
        g := { g with data := g.data.push s!"def {z} : Pt := {← ptText c}" }
        pure (s!"EuclideanGeometry.Concyclic {← pointSet}",
          s!"concyclic_of_dist (O := Pt.toP2 {T} {z}) " ++
            s!"(r := dist (Pt.toP2 {T} {pt args[0]!}) (Pt.toP2 {T} {z})) " ++
            forEach "dist_eq_of_check")
      | "dist" => do
        let (a, b, c, d) := (← P 0, ← P 1, ← P 2, ← P 3)
        match ← str pred "cmp" with
        | "==" => pure (s!"dist {a} {b} = dist {c} {d}", s!"dist_eq_of_check {wf} {check}")
        | "!=" => pure (s!"dist {a} {b} ≠ dist {c} {d}", s!"dist_ne_of_check {wf} {check}")
        | "<" => pure (s!"dist {a} {b} < dist {c} {d}", s!"dist_lt_of_check {wf} {check}")
        | "<=" => pure (s!"dist {a} {b} ≤ dist {c} {d}", s!"dist_le_of_check {wf} {check}")
        | ">" => pure (s!"dist {c} {d} < dist {a} {b}", s!"dist_lt_of_check {wf} {check}")
        | ">=" => pure (s!"dist {c} {d} ≤ dist {a} {b}", s!"dist_le_of_check {wf} {check}")
        | c => throw s!"unknown comparison {c}"
      | k => throw s!"unknown predicate '{k}' ({← str aj "text"})"
    g := g.add (stmt, proof)
  -- assemble
  let title := match core.getObjValAs? String "title" with
    | .ok t => t
    | .error _ => name
  let givens ← nat core "givens"
  let doc := s!"/-- {title}\n\nGenerated from a core export by `construction_proof`. " ++
    s!"{givens} given points; the theorem states the construction steps and the claims " ++
    "for the given coordinates. -/"
  let binders := " ".intercalate (g.binders.map (·.1)).toList
  let body := if g.conjuncts.isEmpty then "True"
    else " ∧\n    ".intercalate (g.conjuncts.map (·.1)).toList
  let statement := if g.binders.isEmpty then body
    else s!"∃ {binders} : StraightedgeCompass.P2,\n    {body}"
  let witnesses := (g.binders.map (·.2)) ++ (g.conjuncts.map (·.2))
  let proof := if g.conjuncts.isEmpty then "trivial" else s!"⟨{", ".intercalate witnesses.toList}⟩"
  let towerDef := s!"def {T} : List Tree := [{", ".intercalate towerText.toList}]"
  let wfDef := s!"theorem {wf} : Tree.WF {T} := Tree.wf_of_towerOk (by decide +kernel)"
  return #[towerDef, wfDef] ++ g.data ++
    #[s!"set_option maxRecDepth 100000 in\nset_option maxHeartbeats 0 in\n{doc}\n" ++
      s!"theorem {ns} :\n    {statement} :=\n  {proof}"]

/-! ## Universal statements

A universal export (`straightedge-compass-universal`, see
`spec/universal-format.md`) has one entry per case of the proof. Each case
becomes a theorem `name.case_…` (or `name` without cases):

    ∀ (p q : ℝ), hypotheses → ∃ (A B … : P2), construction ∧ claims

The hypotheses are those of the PDF, as polynomial conditions on the
variables. The proof is by reflection: the points have polynomial coordinates
(`UPt`) in the variables and the square roots of the case's tower, and the
kernel checks one chain of sign certificates that proves every fact the
conjuncts need (`checkGoals`); each conjunct then follows from a lemma of
`StraightedgeCompass.UGeometry`. -/

/-- A polynomial `[[exponents, "coeff"], …]` as Lean source. -/
def polyText (j : Json) : M String := do
  let ts ← j.getArr?
  let parts ← ts.mapM fun t => do
    let es ← (← t.getArrVal? 0).getArr?
    let es ← es.mapM (·.getNat?)
    let c ← (← t.getArrVal? 1).getStr?
    let some _ := c.toInt? | throw s!"not an integer: {c}"
    return s!"([{", ".intercalate (es.toList.map toString)}], {c})"
  return s!"[{", ".intercalate parts.toList}]"

def relText (r : String) : M String :=
  match r with
  | "pos" | "nonneg" | "nonzero" | "zero" => return s!".{r}"
  | _ => throw s!"unknown relation {r}"

def factText (j : Json) : M String := do
  return s!"⟨{← polyText (← field j "p")}, {← relText (← str j "rel")}⟩"

def natList (j : Json) (k : String) : M String := do
  return s!"[{", ".intercalate ((← nats j k).toList.map toString)}]"

def termText (j : Json) : M String := do
  let c ← str j "c"
  let some n := c.toNat? | throw s!"not a natural number: {c}"
  return s!"⟨{n}, {← natList j "refs"}, {← natList j "sqrefs"}, {← polyText (← field j "sq")}⟩"

def eqText (j : Json) : M String := do
  return s!"⟨{← polyText (← field j "c")}, {← nat j "ref"}⟩"

def listText (xs : Array String) : String := s!"[{", ".intercalate xs.toList}]"

def stepText (j : Json) : M String := do
  match j.getObjVal? "sqrt" with
  | .ok q =>
    return s!".sqrt ⟨{← polyText (← field q "n")}, {← polyText (← field q "d")}, " ++
      s!"{← nat q "rad"}, {← nat q "den"}⟩"
  | .error _ =>
    let f ← factText (← field j "fact")
    let c ← field j "cert"
    let cert ← match c.getObjVal? "sum" with
      | .ok x => do
        let ts ← (← arr x "terms").mapM termText
        let es ← (← arr x "eqs").mapM eqText
        pure s!"(.sum {← termText (← field x "mult")} {listText ts} {listText es})"
      | .error _ => match c.getObjVal? "nonzero" with
        | .ok x => do
          let es ← (← arr x "eqs").mapM eqText
          let cc ← str x "c"
          let some _ := cc.toInt? | throw s!"not an integer: {cc}"
          pure s!"(.nonzero {← polyText (← field x "m")} ({cc}) {← natList x "refs"} {listText es})"
        | .error _ => do
          let ij ← nats c "strict"
          pure s!"(.strict {ij[0]!} {ij[1]!})"
    return s!".fact {f} {cert}"

/-- The commands for one case. -/
def generateLeaf (base : String) (title : String) (leaf : Json) : M (Array String) := do
  let lname ← str leaf "name"
  let thm := if lname == "" then base else s!"{base}.{lname}"
  let label ← str leaf "label"
  let params ← (← arr leaf "params").mapM (·.getStr?)
  let chain ← field leaf "chain"
  let nparams ← nat chain "nparams"
  let ps := s!"[{", ".intercalate params.toList}]"
  let vs := s!"(extend {ps} {thm}.S)"
  -- data
  let hyps ← (← arr chain "hyps").mapM factText
  let sqrts ← (← arr chain "sqrts").mapM fun q => do
    return s!"({← polyText (← q.getArrVal? 0)}, {← polyText (← q.getArrVal? 1)})"
  let steps ← (← arr chain "steps").mapM stepText
  let mut data : Array String := #[
    s!"def {thm}.H : List Fact := {listText hyps}",
    s!"def {thm}.S : List (Poly × Poly) := {listText sqrts}",
    s!"def {thm}.C : List Step := {listText steps}"]
  let uptText (d : Json) : M String := do
    match d.getObjVal? "along", d.getObjVal? "rot" with
    | .ok ab, _ => do
      let ab ← ab.getArr?
      return s!"UPt.alongU {thm}.{← ab[0]!.getStr?} {thm}.{← ab[1]!.getStr?} " ++
        s!"(Poly.var {← nat d "t"})"
    | _, .ok ab => do
      let ab ← ab.getArr?
      return s!"UPt.rotU {thm}.{← ab[0]!.getStr?} {thm}.{← ab[1]!.getStr?} (Poly.var {← nat d "t"})"
    | _, _ => return s!"⟨{← polyText (← field d "x")}, {← polyText (← field d "y")}, " ++
        s!"{← polyText (← field d "d")}⟩"
  for u in ← arr leaf "upts" do
    data := data.push s!"def {thm}.{← str u "name"} : UPt := {← uptText (← field u "def")}"
  let conjs ← arr leaf "conjuncts"
  for c in conjs do
    if (← str c "kind") == "concyclic" then
      let z ← str c "centre"
      data := data.push s!"def {thm}.{z} : UPt := {← uptText (← field c "centreDef")}"
  -- goals
  let goals ← arr leaf "goals"
  let goalText (g : Json) : M String := do
    let fn ← str g "fn"
    let args ← (← arr g "args").mapM (·.getStr?)
    let rel ← relText (← str g "rel")
    if fn == "d" then return s!"⟨({thm}.{args[0]!}).d, {rel}⟩"
    let call := s!"UPt.{fn} {" ".intercalate (args.toList.map (s!"{thm}." ++ ·))}"
    let p := if (← str g "neg") == "yes" then s!"Poly.neg ({call})" else call
    return s!"⟨{p}, {rel}⟩"
  let gs ← goals.mapM goalText
  let idx ← goals.mapM fun g => do return toString (← nat g "index")
  data := data ++ #[
    s!"def {thm}.idx : List Nat := {listText idx}",
    s!"def {thm}.G : List Fact := {listText gs}",
    s!"set_option maxRecDepth 100000 in\ntheorem {thm}.ok : " ++
      s!"checkGoals {nparams} {thm}.H {thm}.S {thm}.C {thm}.idx {thm}.G = true := " ++
      "by decide +kernel"]
  -- the d-goal of every point
  let mut dgoal : Std.HashMap String Nat := {}
  for i in [0:goals.size] do
    let g := goals[i]!
    if (← str g "fn") == "d" then
      dgoal := dgoal.insert (← (← arr g "args")[0]!.getStr?) i
  let D (u : String) : M String := match dgoal[u]? with
    | some k => pure s!"g{k}"
    | none => throw s!"no denominator goal for {u}"
  let G (k : Nat) : String := s!"g{k}"
  -- `toP2 vs u = !₂[x, y]` for a point in the parameters
  let givenProof (ut : String) (x y : Json) (dg : String) : M String := do
    let part (c : Json) (n : String) : M String := do
      return s!"(n{n} := {← str c "num"}) (m{n} := {← str c "den"}) (c{n} := {← str c "c"}) " ++
        s!"(k{n} := {← polyText (← field c "k")})"
    return s!"UPt.toP2_given (u := {ut}) {← part x "x"} {← part y "y"} rfl {dg} " ++
      "(by ring) (by norm_num) " ++
      "(by poly_eval) (by poly_eval) (by ring) (by norm_num) (by poly_eval) (by poly_eval)"
  -- names in the statement
  let reserved := ["lineThrough", "circleThrough", "IsLeft", "LexLt", "rotAbout"]
  let rec unshadow (n : String) (fuel : Nat) : String :=
    match fuel with
    | 0 => n
    | k + 1 => if reserved.contains n then unshadow (n ++ "'") k else n
  let mut binder : Std.HashMap String String := {}
  let mut binders : Array (String × String) := #[]
  for p in ← arr leaf "points" do
    let b := s!"«{unshadow (← str p "binder") 10}»"
    let u ← str p "upt"
    binder := binder.insert u b
    binders := binders.push (b, s!"UPt.toP2 {vs} {thm}.{u}")
  let mut literal : Std.HashSet String := {}
  let mut litProofs : Array String := #[]
  for l in ← arr leaf "literals" do
    let u ← str l "upt"
    binder := binder.insert u (← str l "stmt")
    literal := literal.insert u
    let pl ← field l "poly"
    let ut := s!"⟨{← polyText (← field pl "x")}, {← polyText (← field pl "y")}, " ++
      s!"{← polyText (← field pl "d")}⟩"
    litProofs := litProofs.push (s!"have h{u} : UPt.toP2 {vs} {thm}.{u} = {← str l "stmt"} := " ++
      s!"{← givenProof ut (← field l "x") (← field l "y") s!"g{← nat l "dgoal"}"}")
  let Y := "«other point»"
  let B (u : String) : M String := match binder[u]? with
    | some b => pure b
    | none => throw s!"unknown point {u}"
  let curveText (c : Json) : M String := do
    match c.getObjVal? "line" with
    | .ok ab => do
      let ab ← ab.getArr?
      return s!"lineThrough {← B (← ab[0]!.getStr?)} {← B (← ab[1]!.getStr?)}"
    | .error _ => do
      let ab ← (← field c "circle").getArr?
      return s!"circleThrough {← B (← ab[0]!.getStr?)} {← B (← ab[1]!.getStr?)}"
  let memProof (m : Json) : M String := do
    let x ← str m "p"
    let gl ← nats m "g"
    let c ← field m "curve"
    match c.getObjVal? "line" with
    | .ok ab => do
      let ab ← ab.getArr?
      let (a, b) := (← ab[0]!.getStr?, ← ab[1]!.getStr?)
      return s!"UPt.mem_line_of_crossN {← D a} {← D b} {← D x} {G gl[0]!} {G gl[1]!}"
    | .error _ => do
      let ab ← (← field c "circle").getArr?
      let (o, a) := (← ab[0]!.getStr?, ← ab[1]!.getStr?)
      return s!"UPt.mem_circle_of_circN {← D o} {← D a} {← D x} {G gl[0]!}"
  let pointSet (us : Array String) : M String := do
    return s!"(\{{", ".intercalate (← us.toList.mapM B)}} : Set StraightedgeCompass.P2)"
  let forEach (prfs : Array String) : String :=
    let alts := " | ".intercalate (List.replicate prfs.size "rfl")
    s!"(by intro p hp; simp only [Set.mem_insert_iff, Set.mem_singleton_iff] at hp; " ++
      s!"rcases hp with {alts}; exacts [{", ".intercalate prfs.toList}])"
  -- the conjuncts: statement, proof, and the points they mention
  let mut conj : Array (String × String) := #[]
  for c in conjs do
    let kind ← str c "kind"
    let strsOf (k : String) : M (Array String) := do (← arr c k).mapM (·.getStr?)
    let (stmt, prf, mentions) ← match kind with
      | "given" => do
        let p ← str c "p"
        let pl ← field c "poly"
        let ut := s!"⟨{← polyText (← field pl "x")}, {← polyText (← field pl "y")}, " ++
          s!"{← polyText (← field pl "d")}⟩"
        let cx ← field c "x"
        let cy ← field c "y"
        pure (s!"{← B p} = !₂[{← str cx "expr"}, {← str cy "expr"}]",
          ← givenProof ut cx cy (← D p), #[p])
      | "along" | "rot" => do
        let (p, a, b) := (← str c "p", ← str c "a", ← str c "b")
        let t ← nat c "t"
        let tn := params[t]!
        let (f, lem) := if kind == "along" then ("AffineMap.lineMap", "UPt.toP2_along_param")
          else ("rotAbout", "UPt.toP2_rot_param")
        pure (s!"{← B p} = {f} {← B a} {← B b} {tn}",
          s!"{lem} (i := {t}) rfl (by simp [Poly.eval_var]) {← D a} {← D b}", #[p, a, b])
      | "ne" => do
        let (a, b) := (← str c "a", ← str c "b")
        pure (s!"{← B a} ≠ {← B b}",
          s!"UPt.ne_of_dist2N {← D a} {← D b} {G (← nat c "g")}", #[a, b])
      | "mem" => do
        let x ← str c "p"
        let cv ← field c "curve"
        let pts ← match cv.getObjVal? "line" with
          | .ok ab => ab.getArr? >>= (·.mapM (·.getStr?))
          | .error _ => do (← (← field cv "circle").getArr?).mapM (·.getStr?)
        pure (s!"{← B x} ∈ {← curveText cv}", ← memProof c, #[x] ++ pts)
      | "lineNe" => do
        let l1 ← strsOf "l1"
        let l2 ← strsOf "l2"
        pure (s!"lineThrough {← B l1[0]!} {← B l1[1]!} ≠ lineThrough {← B l2[0]!} {← B l2[1]!}",
          s!"UPt.line_ne_of_crossDirN {← D l1[0]!} {← D l1[1]!} {← D l2[0]!} {← D l2[1]!} " ++
            s!"{G (← nat c "g")}", l1 ++ l2)
      | "other" => do
        let x ← str c "p"
        let y ← str c "y"
        let cvs ← arr c "curves"
        let mems ← arr c "mem"
        let cond ← field c "cond"
        let ck ← str cond "kind"
        let gl ← nats cond "g"
        let name (u : String) : M String := if u == y then pure Y else B u
        let (ct, cp, cm) ← match ck with
          | "lexX" | "lexY" => do
            let (a, b) := (← str cond "a", ← str cond "b")
            let da ← D a
            let db ← D b
            let p := if ck == "lexX" then s!"UPt.lexLt_of_lexXN {da} {db} {G gl[0]!}"
              else s!"UPt.lexLt_of_lexYN {da} {db} {G gl[0]!} {G gl[1]!}"
            pure (s!"LexLt {← name a} {← name b}", p, #[a, b])
          | "nearest" => do
            let r ← str cond "r"
            pure (s!"dist {← B x} {← B r} < dist {Y} {← B r}",
              s!"UPt.dist_lt_of_cmpN {← D y} {← D r} {← D x} {← D r} {G gl[0]!}", #[r])
          | "farthest" => do
            let r ← str cond "r"
            pure (s!"dist {Y} {← B r} < dist {← B x} {← B r}",
              s!"UPt.dist_lt_of_cmpN {← D x} {← D r} {← D y} {← D r} {G gl[0]!}", #[r])
          | "left" => do
            let pq ← (← arr cond "line").mapM (·.getStr?)
            let (p, q) := (pq[0]!, pq[1]!)
            pure (s!"IsLeft {← B p} {← B q} {← B x} ∧ ¬IsLeft {← B p} {← B q} {Y}",
              s!"⟨UPt.isLeft_of_leftN {← D p} {← D q} {← D x} {G gl[0]!}, " ++
                s!"UPt.not_isLeft_of_leftN {← D p} {← D q} {← D y} {G gl[1]!}⟩", #[p, q])
          | k => throw s!"unknown choice condition {k}"
        let stmt := s!"(∃ {Y} : StraightedgeCompass.P2, {Y} ∈ {← curveText cvs[0]!} ∧ " ++
          s!"{Y} ∈ {← curveText cvs[1]!} ∧ {ct})"
        let prf := s!"⟨UPt.toP2 {vs} {thm}.{y}, {← memProof mems[0]!}, {← memProof mems[1]!}, {cp}⟩"
        pure (stmt, prf, #[x] ++ cm)
      | "eq" => do
        let (a, b) := (← str c "a", ← str c "b")
        let gl ← nats c "g"
        pure (s!"{← B a} = {← B b}",
          s!"UPt.eq_of_dN {← D a} {← D b} {G gl[0]!} {G gl[1]!}", #[a, b])
      | "dist" => do
        let as ← strsOf "args"
        let (a, b, cc, d) := (as[0]!, as[1]!, as[2]!, as[3]!)
        let g := G (← nat c "g")
        let da ← D a
        let db ← D b
        let dc ← D cc
        let dd ← D d
        let (A, Bb, C, Dd) := (← B a, ← B b, ← B cc, ← B d)
        let r ← match ← str c "cmp" with
          | "==" => pure (s!"dist {A} {Bb} = dist {C} {Dd}",
            s!"UPt.dist_eq_of_cmpN {da} {db} {dc} {dd} {g}")
          | "!=" => pure (s!"dist {A} {Bb} ≠ dist {C} {Dd}",
            s!"UPt.dist_ne_of_cmpN {da} {db} {dc} {dd} {g}")
          | "<" => pure (s!"dist {A} {Bb} < dist {C} {Dd}",
            s!"UPt.dist_lt_of_cmpN {dc} {dd} {da} {db} {g}")
          | "<=" => pure (s!"dist {A} {Bb} ≤ dist {C} {Dd}",
            s!"UPt.dist_le_of_cmpN {dc} {dd} {da} {db} {g}")
          | ">" => pure (s!"dist {C} {Dd} < dist {A} {Bb}",
            s!"UPt.dist_lt_of_cmpN {da} {db} {dc} {dd} {g}")
          | ">=" => pure (s!"dist {C} {Dd} ≤ dist {A} {Bb}",
            s!"UPt.dist_le_of_cmpN {da} {db} {dc} {dd} {g}")
          | k => throw s!"unknown comparison {k}"
        pure (r.1, r.2, as)
      | "collinear3" => do
        let as ← strsOf "args"
        pure (s!"Collinear ℝ {← pointSet as}",
          s!"UPt.collinear_of_crossN {← D as[0]!} {← D as[1]!} {← D as[2]!} " ++
            s!"{G (← nats c "g")[0]!}", as)
      | "collinear" => do
        let as ← strsOf "args"
        let bs ← strsOf "base"
        let nd ← nat c "nd"
        let gl ← nats c "g"
        let prfs ← (Array.range as.size).mapM fun i => do
          return s!"UPt.mem_line_of_crossN {← D bs[0]!} {← D bs[1]!} {← D as[i]!} {G nd} {G gl[i]!}"
        pure (s!"Collinear ℝ {← pointSet as}",
          s!"collinear_of_mem_line (A := UPt.toP2 {vs} {thm}.{bs[0]!}) " ++
            s!"(B := UPt.toP2 {vs} {thm}.{bs[1]!}) (by simp) {forEach prfs}", as)
      | "concyclic" => do
        let as ← strsOf "args"
        let z ← str c "centre"
        let gl ← nats c "g"
        let dz := G (← nat c "dgoal")
        let prfs ← (Array.range as.size).mapM fun i => do
          return s!"UPt.dist_eq_of_circN {dz} {← D as[0]!} {← D as[i]!} {G gl[i]!}"
        pure (s!"EuclideanGeometry.Concyclic {← pointSet as}",
          s!"concyclic_of_dist (O := UPt.toP2 {vs} {thm}.{z}) " ++
            s!"(r := dist (UPt.toP2 {vs} {thm}.{as[0]!}) (UPt.toP2 {vs} {thm}.{z})) " ++
            forEach prfs, as)
      | "parallel" => do
        let l1 ← strsOf "l1"
        let l2 ← strsOf "l2"
        let gl ← nats c "g"
        pure (s!"(lineThrough {← B l1[0]!} {← B l1[1]!}).Parallel " ++
            s!"(lineThrough {← B l2[0]!} {← B l2[1]!})",
          s!"UPt.parallel_of_crossDirN {← D l1[0]!} {← D l1[1]!} {← D l2[0]!} {← D l2[1]!} " ++
            s!"{G gl[0]!} {G gl[1]!} {G gl[2]!}", l1 ++ l2)
      | "perp" => do
        let l1 ← strsOf "l1"
        let l2 ← strsOf "l2"
        let gl ← nats c "g"
        pure (s!"(lineThrough {← B l1[0]!} {← B l1[1]!}).direction ⟂ " ++
            s!"(lineThrough {← B l2[0]!} {← B l2[1]!}).direction",
          s!"UPt.perp_of_dotDirN {← D l1[0]!} {← D l1[1]!} {← D l2[0]!} {← D l2[1]!} {G gl[0]!}",
          l1 ++ l2)
      | "angle" => do
        let as ← strsOf "args"
        let gl ← nats c "g"
        let ds ← as.mapM D
        let sameSign ← match ← str c "sign" with
          | "pos" => pure s!"(UPt.sameSign_pos {G gl[4]!} {G gl[5]!})"
          | "zero" => pure s!"(UPt.sameSign_zero {G gl[4]!} {G gl[5]!})"
          | "neg" => pure s!"(UPt.sameSign_neg {G gl[4]!} {G gl[5]!})"
          | k => throw s!"unknown sign {k}"
        pure (s!"EuclideanGeometry.angle {← B as[0]!} {← B as[1]!} {← B as[2]!} = " ++
            s!"EuclideanGeometry.angle {← B as[3]!} {← B as[4]!} {← B as[5]!}",
          s!"UPt.angle_eq_of_N {" ".intercalate ds.toList} {G gl[0]!} {G gl[1]!} {G gl[2]!} " ++
            s!"{G gl[3]!} {sameSign} {G gl[6]!}", as)
      | "ccw" => do
        let as ← strsOf "args"
        let gl ← nats c "g"
        pure (s!"IsLeft {← B as[0]!} {← B as[1]!} {← B as[2]!}",
          s!"UPt.isLeft_of_leftN {← D as[0]!} {← D as[1]!} {← D as[2]!} {G gl[0]!}", as)
      | "between" => do
        let as ← strsOf "args"
        let gl ← nats c "g"
        pure (s!"Sbtw ℝ {← B as[0]!} {← B as[1]!} {← B as[2]!}",
          s!"UPt.sbtw_of_N {← D as[0]!} {← D as[1]!} {← D as[2]!} {G gl[0]!} {G gl[1]!} " ++
            s!"{G gl[2]!}", as)
      | k => throw s!"unknown conjunct {k}"
    -- a literal point appears as `!₂[x, y]`: rewrite it into its tower point
    let lits := mentions.toList.filter (literal.contains ·) |>.eraseDups
    let prf := if lits.isEmpty then prf
      else s!"(by rw [{", ".intercalate (lits.map (s!"← h" ++ ·))}]; exact {prf})"
    -- the same statement from another step is stated once
    unless conj.any (·.1 == stmt) do
      conj := conj.push (stmt, prf)
  -- hypotheses
  let hypsJ ← arr leaf "hyps"
  let mut hypStmts : Array String := #[]
  let mut convs : Array String := #[]
  for i in [0:hypsJ.size] do
    let h := hypsJ[i]!
    hypStmts := hypStmts.push (← str h "stmt")
    let c ← str h "c"
    let lem ← match ← str h "kind" with
      | "pos" => pure "holds_pos_of"
      | "neg" => pure "holds_neg_of"
      | "nonneg" => pure "holds_nonneg_of"
      | "nonpos" => pure "holds_nonpos_of"
      | "nonzero" => pure "holds_nonzero_of"
      | "ne" => pure "holds_ne_of"
      | k => throw s!"unknown hypothesis kind {k}"
    convs := convs.push s!"({lem} (c := {c}) (by norm_num) h{i} (by poly_eval))"
  let hH := convs.foldr (fun cv acc => s!"AllHold.cons {cv} ({acc})") "AllHold.nil _"
  -- the statement
  let quant := if params.isEmpty then ""
    else s!"∀ {" ".intercalate (params.toList.map (s!"({·} : ℝ)"))}, "
  let hypsText := String.join (hypStmts.toList.map (· ++ " →
    "))
  let body := if conj.isEmpty then "True" else " ∧
      ".intercalate (conj.map (·.1)).toList
  let exStmt := if binders.isEmpty then body
    else s!"∃ {" ".intercalate (binders.map (·.1)).toList} : StraightedgeCompass.P2,
      {body}"
  let statement := s!"{quant}{hypsText}{exStmt}"
  let intros := params.toList ++ (List.range hypsJ.size).map (s!"h{·}")
  let goalNames := (List.range goals.size).map (s!"g{·}")
  let witnesses := (binders.map (·.2)) ++ (conj.map (·.2))
  let proof := "by
" ++
    (if intros.isEmpty then "" else s!"  intro {" ".intercalate intros}
") ++
    s!"  have hH : AllHold {ps} {thm}.H := {hH}
" ++
    s!"  obtain ⟨{", ".intercalate goalNames}, -⟩ := goals_sound (ps := {ps}) {thm}.ok hH
" ++
    String.join (litProofs.toList.map (s!"  " ++ · ++ "
")) ++
    s!"  exact ⟨{", ".intercalate witnesses.toList}⟩"
  let hypDoc := String.join ((← hypsJ.mapM fun h => do
    return s!"
* {← str h "text"}: `{← str h "stmt"}`").toList)
  let doc := s!"/-- {title}" ++ (if label == "" then "" else s!", case {label}") ++
    ".

Generated from a universal export by `construction_proof`." ++
    (if hypsJ.isEmpty then "" else s!" Hypotheses:{hypDoc}") ++ " -/"
  return data ++ #[s!"set_option maxRecDepth 100000 in
set_option maxHeartbeats 0 in
{doc}
" ++
    s!"theorem {thm} :
    {statement} :=
{proof}"]

/-- A decision tree as Lean source. -/
partial def treeSrc (j : Json) : M String := do
  match j.getObjVal? "split" with
  | .ok p =>
    return s!"(.split {← polyText p} {← treeSrc (← field j "neg")} " ++
      s!"{← treeSrc (← field j "zero")} " ++
      s!"{← treeSrc (← field j "pos")})"
  | .error _ =>
  match j.getObjVal? "chain" with
  | .ok cs => do
    let steps ← (← cs.getArr?).mapM stepText
    return s!"(.chain {listText steps} {← treeSrc (← field j "next")})"
  | .error _ =>
    let steps ← (← arr j "steps").mapM stepText
    match j.getObjVal? "case" with
    | .ok k => return s!"(.case {← k.getNat?} {listText steps} {← natList j "idx"})"
    | .error _ => do
      let ij ← nats j "absurd"
      return s!"(.absurd {listText steps} {ij[0]!} {ij[1]!})"

/-- The coverage theorem of one `cases` statement:
`∀ params, hypotheses → case₁ ∨ case₂ ∨ …`. -/
def generateCover (base : String) (title : String) (cv : Json) : M (Array String) := do
  let thm := s!"{base}.{← str cv "name"}"
  let params ← (← arr cv "params").mapM (·.getStr?)
  let nparams ← nat cv "nparams"
  let ps := s!"[{", ".intercalate params.toList}]"
  let hyps ← (← arr cv "facts").mapM factText
  let root ← (← arr cv "root").mapM stepText
  let D ← (← arr cv "D").mapM fun d => do return listText (← d.getArr? >>= (·.mapM factText))
  let data := #[
    s!"def {thm}.H : List Fact := {listText hyps}",
    s!"def {thm}.D : List (List Fact) := {listText D}",
    s!"def {thm}.T : Cover := .chain {listText root} {← treeSrc (← field cv "tree")}",
    s!"set_option maxRecDepth 100000 in\ntheorem {thm}.ok : " ++
      s!"checkCover {nparams} {thm}.D {thm}.H {thm}.T = " ++
      "true := by decide +kernel"]
  -- hypotheses
  let hypsJ ← arr cv "hyps"
  let mut hypStmts : Array String := #[]
  let mut convs : Array String := #[]
  for i in [0:hypsJ.size] do
    let h := hypsJ[i]!
    hypStmts := hypStmts.push (← str h "stmt")
    let lem ← match ← str h "kind" with
      | "pos" => pure "holds_pos_of"
      | "neg" => pure "holds_neg_of"
      | "nonneg" => pure "holds_nonneg_of"
      | "nonpos" => pure "holds_nonpos_of"
      | "nonzero" => pure "holds_nonzero_of"
      | "ne" => pure "holds_ne_of"
      | k => throw s!"unknown hypothesis kind {k}"
    convs := convs.push s!"({lem} (c := {← str h "c"}) (by norm_num) h{i} (by poly_eval))"
  let hH := convs.foldr (fun cv acc => s!"AllHold.cons {cv} ({acc})") "AllHold.nil _"
  -- the cases, readable
  let cases ← arr cv "cases"
  let mut disj : Array String := #[]
  let mut alts : Array String := #[]
  let mut proofs : Array String := #[]
  for k in [0:cases.size] do
    let conds ← arr cases[k]! "conds"
    let names := (List.range conds.size).map (s!"c{k}_{·}")
    let mut stmts : Array String := #[]
    let mut prfs : Array String := #[]
    for i in [0:conds.size] do
      let c := conds[i]!
      let x := s!"c{k}_{i}"
      let cc ← str c "c"
      stmts := stmts.push (← str c "stmt")
      let prf ← match ← str c "kind" with
        | "pos" => pure s!"pos_of_holds (c := {cc}) (by norm_num) (by poly_eval) {x}"
        | "neg" => pure s!"neg_of_holds (c := {cc}) (by norm_num) (by poly_eval) {x}"
        | "nonneg" => pure s!"nonneg_of_holds (c := {cc}) (by norm_num) (by poly_eval) {x}"
        | "nonpos" => pure s!"nonpos_of_holds (c := {cc}) (by norm_num) (by poly_eval) {x}"
        | "nonzero" => pure s!"nonzero_of_holds (c := {cc}) (by poly_eval) {x}"
        | "ne" => pure s!"ne_of_holds (c := {cc}) (by poly_eval) {x}"
        | "eq" => pure s!"eq_of_holds (c := {cc}) (by norm_num) (by poly_eval) {x}"
        | "eqdiv" =>
          -- the denominator's condition comes just before
          pure s!"eq_div_of_holds (c := {cc}) (by norm_num) {prfs[i - 1]!} (by poly_eval) {x}"
        | k => throw s!"unknown condition kind {k}"
      prfs := prfs.push s!"({prf})"
    disj := disj.push (if stmts.isEmpty then "True" else s!"({" ∧ ".intercalate stmts.toList})")
    alts := alts.push (if names.isEmpty then "_" else s!"⟨{", ".intercalate names}, -⟩")
    let body := if prfs.isEmpty then "trivial" else if prfs.size == 1 then prfs[0]!
      else s!"⟨{", ".intercalate prfs.toList}⟩"
    let wrapped := if k + 1 == cases.size then body
      else s!"Or.inl {body}"
    proofs := proofs.push
      (String.join (List.replicate k "Or.inr (") ++ wrapped ++ String.join (List.replicate k ")"))
  let quant := if params.isEmpty then ""
    else s!"∀ {" ".intercalate (params.toList.map (s!"({·} : ℝ)"))}, "
  let hypsText := String.join (hypStmts.toList.map (· ++ " →\n    "))
  let alternatives := if disj.isEmpty then "False" else " ∨\n      ".intercalate disj.toList
  let statement := s!"{quant}{hypsText}{alternatives}"
  let intros := params.toList ++ (List.range hypsJ.size).map (s!"h{·}")
  let proof := "by\n" ++
    (if intros.isEmpty then "" else s!"  intro {" ".intercalate intros}\n") ++
    s!"  have hH : AllHold {ps} {thm}.H := {hH}\n" ++
    s!"  rcases cover_sound (ps := {ps}) {thm}.ok hH with " ++
      s!"{" | ".intercalate (alts.toList ++ ["hF"])}\n" ++
    String.join (proofs.toList.map (s!"  · exact " ++ · ++ "\n")) ++
    "  · exact hF.elim"
  let caseDoc := String.join ((← cases.mapM fun c => do return s!"\n* {← str c "label"}").toList)
  let label ← str cv "label"
  let doc := s!"/-- {title}: the cases of `{← str cv "split"}`" ++
    (if label == "" then "" else s!" (in the case {label})") ++
    s!" cover every configuration. The cases:{caseDoc} -/"
  return data ++ #[s!"set_option maxRecDepth 100000 in\nset_option maxHeartbeats 0 in\n{doc}\n" ++
    s!"theorem {thm} :\n    {statement} :=\n{proof}"]

/-- Builds the commands for a universal export. -/
def generateUniversal (name : String) (core : Json) : M (Array String) := do
  unless (← nat core "version") == 1 do throw "unsupported universal export version"
  let ns := s!"«{name}»"
  let title := match core.getObjValAs? String "title" with
    | .ok t => t
    | .error _ => name
  let leaves ← arr core "leaves"
  let mut cmds : Array String := #[]
  for l in leaves do
    cmds := cmds ++ (← generateLeaf ns title l)
  if let .ok covers := core.getObjValAs? (Array Json) "covers" then
    for c in covers do
      cmds := cmds ++ (← generateCover ns title c)
  return cmds

end StraightedgeCompass.Elab

open StraightedgeCompass.Elab in
/-- `construction_proof name from "file.core.json"` adds the theorem `name`
proved from a core export; see the module documentation. -/
elab "construction_proof " name:ident " from " path:str : command => do
  let file := path.getString
  let text ← try IO.FS.readFile file
    catch e => throwErrorAt path m!"cannot read {file}: {e.toMessageData}"
  let json ← match Json.parse text with
    | .ok j => pure j
    | .error e => throwErrorAt path m!"{file}: invalid JSON: {e}"
  let gen := match json.getObjValAs? String "format" with
    | .ok "straightedge-compass-universal" => generateUniversal
    | _ => generate
  let cmds ← match gen (name.getId.toString (escape := false)) json with
    | .ok cs => pure cs
    | .error e => throwErrorAt path m!"{file}: {e}"
  for c in cmds do
    let c := "open StraightedgeCompass in\n" ++ c
    let stx ← match Parser.runParserCategory (← getEnv) `command c with
      | .ok s => pure s
      | .error e => throwErrorAt name m!"internal error: generated command does not parse: {e}\n{c}"
    withRef name (elabCommand stx)
