module

public import StraightedgeCompass.Poly
public import Mathlib.Analysis.Real.Sqrt
public import Mathlib.Tactic.FieldSimp
public import Mathlib.Tactic.Positivity

/-!
# Sign certificates

A *fact* is a polynomial with a sign condition (`> 0`, `≥ 0`, `≠ 0`, `= 0`)
at a point `vs : List ℝ` of the variables. Facts are proved from known facts
by certificates whose checking is pure polynomial arithmetic, so that the
kernel can do it (`decide +kernel`):

* `sum m terms eqs`: `m · p = Σ c · Π fᵢ · Π gⱼ² · q² + Σ e · z` where each
  term is a product of known facts `fᵢ ≥ 0` (or `> 0`), squares of known
  facts `gⱼ ≠ 0` and a square `q²`, and the `z` are known facts `= 0`; the
  multiplier `m` is such a term too. This gives `p > 0` (a strictly positive
  term, `m ≥ 0`), `p ≥ 0` (`m > 0`) or `p = 0` (no terms, `m ≠ 0`);
* `nonzero m c refs eqs`: `p · m = c · Π fᵢ + Σ e · z` with the `fᵢ` nonzero
  and `c ≠ 0`, so `p ≠ 0`;
* `strict i j`: `p ≥ 0` and `p ≠ 0` (known facts), so `p > 0`.

A *chain* proves facts in order from the hypotheses. It also introduces the
square roots of a construction: a step `sqrt n d` adds the variable
`s = √(n / d)` once `n · d ≥ 0` and `d ≠ 0` are known, with the facts
`d · s² - n = 0` and `s ≥ 0` (or `s > 0` when `n · d > 0`).
-/

@[expose] public section

namespace StraightedgeCompass

inductive Rel where
  | pos
  | nonneg
  | nonzero
  | zero
  deriving DecidableEq, Repr

structure Fact where
  p : Poly
  rel : Rel
  deriving DecidableEq, Repr

noncomputable def Fact.holds (vs : List ℝ) (f : Fact) : Prop :=
  match f.rel with
  | .pos => 0 < Poly.eval vs f.p
  | .nonneg => 0 ≤ Poly.eval vs f.p
  | .nonzero => Poly.eval vs f.p ≠ 0
  | .zero => Poly.eval vs f.p = 0

/-- Every fact of the list holds. -/
noncomputable def AllHold (vs : List ℝ) : List Fact → Prop
  | [] => True
  | f :: fs => f.holds vs ∧ AllHold vs fs

theorem AllHold.get {vs : List ℝ} : ∀ {K : List Fact} (_ : AllHold vs K) (i : Nat) {f : Fact},
    K[i]? = some f → f.holds vs
  | [], _, _, _, h => by simp at h
  | g :: K, h, 0, f, hi => by
    simp only [List.getElem?_cons_zero, Option.some.injEq] at hi; subst hi; exact h.1
  | g :: K, h, i + 1, f, hi => AllHold.get h.2 i (by simpa using hi)

theorem AllHold.nil (vs : List ℝ) : AllHold vs [] := by unfold AllHold; trivial

theorem AllHold.cons {vs : List ℝ} {f : Fact} {K : List Fact} (h : f.holds vs)
    (hK : AllHold vs K) : AllHold vs (f :: K) := by
  unfold AllHold; exact ⟨h, hK⟩

theorem AllHold.append {vs : List ℝ} : ∀ {K L : List Fact}, AllHold vs K → AllHold vs L →
    AllHold vs (K ++ L)
  | [], _, _, hL => hL
  | _ :: _, _, hK, hL => AllHold.cons hK.1 (AllHold.append hK.2 hL)

/-- A term `c · Π refs · Π sqrefs² · sq²`. -/
structure Term where
  c : Nat
  refs : List Nat
  sqrefs : List Nat
  sq : Poly
  deriving Repr

/-- A term `c · z` with `z` a known fact `= 0`. -/
structure EqTerm where
  c : Poly
  ref : Nat
  deriving Repr

inductive Cert where
  | sum (mult : Term) (terms : List Term) (eqs : List EqTerm)
  | nonzero (m : Poly) (c : Int) (refs : List Nat) (eqs : List EqTerm)
  | strict (i j : Nat)
  deriving Repr

/-- A sqrt step: `s = √(n / d)`, with the facts `n · d ≥ 0` (or `> 0`) and
`d ≠ 0` (or `> 0`) at the given indices. -/
structure SqrtStep where
  n : Poly
  d : Poly
  rad : Nat
  den : Nat
  deriving Repr

inductive Step where
  | fact (f : Fact) (c : Cert)
  | sqrt (s : SqrtStep)
  deriving Repr

namespace Cert

def prodPoly (K : List Fact) (refs : List Nat) : Poly :=
  refs.foldr (fun i acc => Poly.mul ((K[i]?.map Fact.p).getD []) acc) (Poly.const 1)

def termPoly (K : List Fact) (t : Term) : Poly :=
  Poly.mul (Poly.const t.c) (Poly.mul (prodPoly K t.refs)
    (Poly.mul (prodPoly K t.sqrefs) (Poly.mul (prodPoly K t.sqrefs) (Poly.mul t.sq t.sq))))

def eqPoly (K : List Fact) (e : EqTerm) : Poly :=
  Poly.mul e.c ((K[e.ref]?.map Fact.p).getD [])

def sumPoly (K : List Fact) (terms : List Term) (eqs : List EqTerm) : Poly :=
  Poly.add (terms.foldr (fun t acc => Poly.add (termPoly K t) acc) [])
    (eqs.foldr (fun e acc => Poly.add (eqPoly K e) acc) [])

def relIs (K : List Fact) (i : Nat) (ok : Rel → Bool) : Bool :=
  match K[i]? with
  | some f => ok f.rel
  | none => false

def isPosRel : Rel → Bool
  | .pos => true
  | _ => false

def isNonnegRel : Rel → Bool
  | .pos | .nonneg => true
  | _ => false

def isNonzeroRel : Rel → Bool
  | .pos | .nonzero => true
  | _ => false

def isZeroRel : Rel → Bool
  | .zero => true
  | _ => false

/-- `sq` is a nonzero constant. -/
def isNonzeroConst (sq : Poly) : Bool :=
  match Poly.norm sq with
  | [(m, c)] => m.isEmpty && c != 0
  | _ => false

def termNonneg (K : List Fact) (t : Term) : Bool :=
  0 < t.c && t.refs.all (fun i => relIs K i isNonnegRel) &&
    t.sqrefs.all (fun i => relIs K i isNonzeroRel)

def termStrict (K : List Fact) (t : Term) : Bool :=
  termNonneg K t && t.refs.all (fun i => relIs K i isPosRel) && isNonzeroConst t.sq

/-- The term is a nonzero product. -/
def termNonzero (K : List Fact) (t : Term) : Bool :=
  0 < t.c && t.refs.all (fun i => relIs K i isNonzeroRel) &&
    t.sqrefs.all (fun i => relIs K i isNonzeroRel) && isNonzeroConst t.sq

def eqsOk (K : List Fact) (eqs : List EqTerm) : Bool := eqs.all (fun e => relIs K e.ref isZeroRel)

/-- The fact `i` has the polynomial `p` (as a polynomial) and a relation
accepted by `ok`. -/
def factIs (K : List Fact) (i : Nat) (p : Poly) (ok : Rel → Bool) : Bool :=
  match K[i]? with
  | some g => Poly.beq g.p p && ok g.rel
  | none => false

/-- Checks a certificate for the fact `f` from the known facts `K`. -/
def check (K : List Fact) (f : Fact) : Cert → Bool
  | .sum mult terms eqs =>
    eqsOk K eqs && terms.all (termNonneg K) &&
    Poly.beq (Poly.mul (termPoly K mult) f.p) (sumPoly K terms eqs) &&
    match f.rel with
    | .zero => terms.isEmpty && termNonzero K mult
    | .nonneg => termStrict K mult
    | .pos | .nonzero => terms.any (termStrict K) && termNonneg K mult
  | .nonzero m c refs eqs =>
    f.rel == .nonzero && c != 0 && eqsOk K eqs && refs.all (fun i => relIs K i isNonzeroRel) &&
    Poly.beq (Poly.mul f.p m) (Poly.add (Poly.mul (Poly.const c) (prodPoly K refs))
      (eqs.foldr (fun e acc => Poly.add (eqPoly K e) acc) []))
  | .strict i j =>
    f.rel == .pos && factIs K i f.p isNonnegRel && factIs K j f.p isNonzeroRel

end Cert

/-! ## Soundness of certificates -/

section Sound

variable {vs : List ℝ} {K : List Fact}

theorem rel_pos {i : Nat} (hK : AllHold vs K) (h : Cert.relIs K i Cert.isPosRel = true) :
    0 < Poly.eval vs ((K[i]?.map Fact.p).getD []) := by
  unfold Cert.relIs at h
  split at h
  · rename_i f hf
    have := hK.get i hf
    simp only [hf, Option.map_some, Option.getD_some]
    cases hr : f.rel <;> simp_all [Cert.isPosRel, Fact.holds]
  · exact absurd h (by simp)

theorem rel_nonneg {i : Nat} (hK : AllHold vs K) (h : Cert.relIs K i Cert.isNonnegRel = true) :
    0 ≤ Poly.eval vs ((K[i]?.map Fact.p).getD []) := by
  unfold Cert.relIs at h
  split at h
  · rename_i f hf
    have := hK.get i hf
    simp only [hf, Option.map_some, Option.getD_some]
    cases hr : f.rel <;> simp_all [Cert.isNonnegRel, Fact.holds]
    linarith
  · exact absurd h (by simp)

theorem rel_nonzero {i : Nat} (hK : AllHold vs K) (h : Cert.relIs K i Cert.isNonzeroRel = true) :
    Poly.eval vs ((K[i]?.map Fact.p).getD []) ≠ 0 := by
  unfold Cert.relIs at h
  split at h
  · rename_i f hf
    have := hK.get i hf
    simp only [hf, Option.map_some, Option.getD_some]
    cases hr : f.rel <;>
      simp_all only [Cert.isNonzeroRel, Fact.holds, ne_eq, Bool.false_eq_true, not_false_eq_true]
    exact ne_of_gt this
  · exact absurd h (by simp)

theorem rel_zero {i : Nat} (hK : AllHold vs K) (h : Cert.relIs K i Cert.isZeroRel = true) :
    Poly.eval vs ((K[i]?.map Fact.p).getD []) = 0 := by
  unfold Cert.relIs at h
  split at h
  · rename_i f hf
    have := hK.get i hf
    simp only [hf, Option.map_some, Option.getD_some]
    cases hr : f.rel <;> simp_all [Cert.isZeroRel, Fact.holds]
  · exact absurd h (by simp)

theorem eval_prodPoly (refs : List Nat) :
    Poly.eval vs (Cert.prodPoly K refs) =
      (refs.map fun i => Poly.eval vs ((K[i]?.map Fact.p).getD [])).prod := by
  induction refs with
  | nil => simp [Cert.prodPoly, Poly.eval_const]
  | cons i refs ih =>
    simp only [Cert.prodPoly, List.foldr_cons] at *
    rw [Poly.eval_mul, ih]; simp

theorem prod_nonneg_of {refs : List Nat} (hK : AllHold vs K)
    (h : refs.all (fun i => Cert.relIs K i Cert.isNonnegRel) = true) :
    0 ≤ Poly.eval vs (Cert.prodPoly K refs) := by
  rw [eval_prodPoly]
  apply List.prod_nonneg
  intro x hx
  simp only [List.mem_map] at hx
  obtain ⟨i, hi, rfl⟩ := hx
  exact rel_nonneg hK (by simp only [List.all_eq_true] at h; exact h i hi)

theorem prod_pos_of {refs : List Nat} (hK : AllHold vs K)
    (h : refs.all (fun i => Cert.relIs K i Cert.isPosRel) = true) :
    0 < Poly.eval vs (Cert.prodPoly K refs) := by
  rw [eval_prodPoly]
  apply List.prod_pos
  intro x hx
  simp only [List.mem_map] at hx
  obtain ⟨i, hi, rfl⟩ := hx
  exact rel_pos hK (by simp only [List.all_eq_true] at h; exact h i hi)

theorem prod_ne_zero_of : ∀ {refs : List Nat}, AllHold vs K →
    refs.all (fun i => Cert.relIs K i Cert.isNonzeroRel) = true →
    Poly.eval vs (Cert.prodPoly K refs) ≠ 0
  | [], _, _ => by simp [Cert.prodPoly, Poly.eval_const]
  | i :: refs, hK, h => by
    simp only [List.all_cons, Bool.and_eq_true] at h
    simp only [Cert.prodPoly, List.foldr_cons, Poly.eval_mul]
    exact mul_ne_zero (rel_nonzero hK h.1) (prod_ne_zero_of hK h.2)

theorem eval_termPoly (t : Term) :
    Poly.eval vs (Cert.termPoly K t) = (t.c : ℝ) * Poly.eval vs (Cert.prodPoly K t.refs) *
      (Poly.eval vs (Cert.prodPoly K t.sqrefs)) ^ 2 * (Poly.eval vs t.sq) ^ 2 := by
  simp only [Cert.termPoly, Poly.eval_mul, Poly.eval_const]; push_cast; ring

theorem term_nonneg {t : Term} (hK : AllHold vs K) (h : Cert.termNonneg K t = true) :
    0 ≤ Poly.eval vs (Cert.termPoly K t) := by
  simp only [Cert.termNonneg, Bool.and_eq_true, decide_eq_true_eq] at h
  rw [eval_termPoly]
  have := prod_nonneg_of hK h.1.2
  positivity

theorem term_pos {t : Term} (hK : AllHold vs K) (h : Cert.termStrict K t = true) :
    0 < Poly.eval vs (Cert.termPoly K t) := by
  simp only [Cert.termStrict, Cert.termNonneg, Bool.and_eq_true, decide_eq_true_eq] at h
  obtain ⟨⟨⟨⟨hc, _⟩, hsq⟩, hrefs⟩, hconst⟩ := h
  rw [eval_termPoly]
  have h1 := prod_pos_of hK hrefs
  have h2 := prod_ne_zero_of hK hsq
  have h3 : Poly.eval vs t.sq ≠ 0 := by
    unfold Cert.isNonzeroConst at hconst
    rw [← Poly.eval_norm]
    split at hconst
    · rename_i m c hm
      simp only [Bool.and_eq_true, List.isEmpty_iff, bne_iff_ne, ne_eq] at hconst
      rw [hm, hconst.1]
      simp [Mono.eval, hconst.2]
    · exact absurd hconst (by simp)
  have hc' : (0 : ℝ) < t.c := by exact_mod_cast hc
  have : 0 < (Poly.eval vs (Cert.prodPoly K t.sqrefs)) ^ 2 := by positivity
  have : 0 < (Poly.eval vs t.sq) ^ 2 := by positivity
  positivity

theorem terms_nonneg : ∀ {terms : List Term}, AllHold vs K →
    terms.all (Cert.termNonneg K) = true →
    0 ≤ Poly.eval vs (terms.foldr (fun t acc => Poly.add (Cert.termPoly K t) acc) [])
  | [], _, _ => by simp
  | t :: ts, hK, h => by
    simp only [List.all_cons, Bool.and_eq_true] at h
    simp only [List.foldr_cons, Poly.eval_add]
    have := term_nonneg hK h.1
    have := terms_nonneg (terms := ts) hK h.2
    linarith

theorem terms_pos : ∀ {terms : List Term}, AllHold vs K →
    terms.all (Cert.termNonneg K) = true → terms.any (Cert.termStrict K) = true →
    0 < Poly.eval vs (terms.foldr (fun t acc => Poly.add (Cert.termPoly K t) acc) [])
  | [], _, _, h => by simp at h
  | t :: ts, hK, h, hs => by
    simp only [List.all_cons, Bool.and_eq_true] at h
    simp only [List.any_cons, Bool.or_eq_true] at hs
    simp only [List.foldr_cons, Poly.eval_add]
    rcases hs with hs | hs
    · have := term_pos hK hs
      have := terms_nonneg (terms := ts) hK h.2
      linarith
    · have := term_nonneg hK h.1
      have := terms_pos (terms := ts) hK h.2 hs
      linarith

theorem eqs_zero : ∀ {eqs : List EqTerm}, AllHold vs K → Cert.eqsOk K eqs = true →
    Poly.eval vs (eqs.foldr (fun e acc => Poly.add (Cert.eqPoly K e) acc) []) = 0
  | [], _, _ => by simp
  | e :: es, hK, h => by
    simp only [Cert.eqsOk, List.all_cons, Bool.and_eq_true] at h
    simp only [List.foldr_cons, Poly.eval_add]
    rw [eqs_zero hK (by simpa [Cert.eqsOk] using h.2)]
    simp only [Cert.eqPoly, Poly.eval_mul, rel_zero hK h.1]
    ring

theorem const_ne_zero {sq : Poly} (h : Cert.isNonzeroConst sq = true) : Poly.eval vs sq ≠ 0 := by
  unfold Cert.isNonzeroConst at h
  rw [← Poly.eval_norm]
  split at h
  · rename_i m c hm
    simp only [Bool.and_eq_true, List.isEmpty_iff, bne_iff_ne, ne_eq] at h
    rw [hm, h.1]
    simp [Mono.eval, h.2]
  · exact absurd h (by simp)

theorem term_ne_zero {t : Term} (hK : AllHold vs K) (h : Cert.termNonzero K t = true) :
    Poly.eval vs (Cert.termPoly K t) ≠ 0 := by
  simp only [Cert.termNonzero, Bool.and_eq_true, decide_eq_true_eq] at h
  obtain ⟨⟨⟨hc, hrefs⟩, hsq⟩, hconst⟩ := h
  rw [eval_termPoly]
  have h1 := prod_ne_zero_of hK hrefs
  have h2 := prod_ne_zero_of hK hsq
  have h3 := const_ne_zero (vs := vs) hconst
  have hc' : (t.c : ℝ) ≠ 0 := by exact_mod_cast hc.ne'
  exact mul_ne_zero (mul_ne_zero (mul_ne_zero hc' h1) (pow_ne_zero 2 h2)) (pow_ne_zero 2 h3)

theorem factIs_eval {i : Nat} {p : Poly} {ok : Rel → Bool} (h : Cert.factIs K i p ok = true) :
    ∃ g, K[i]? = some g ∧ (∀ vs, Poly.eval vs g.p = Poly.eval vs p) ∧ ok g.rel = true := by
  unfold Cert.factIs at h
  split at h
  · rename_i g hg
    simp only [Bool.and_eq_true] at h
    exact ⟨g, hg, Poly.eval_eq_of_beq h.1, h.2⟩
  · exact absurd h (by simp)

/-- A checked certificate proves its fact. -/
theorem Cert.sound {f : Fact} {c : Cert} (hK : AllHold vs K) (h : Cert.check K f c = true) :
    f.holds vs := by
  obtain ⟨p, rel⟩ := f
  cases c with
  | sum mult terms eqs =>
    simp only [Cert.check, Bool.and_eq_true] at h
    obtain ⟨⟨⟨heqs, hterms⟩, hid⟩, hrel⟩ := h
    have hid' := Poly.eval_eq_of_beq hid vs
    simp only [Poly.eval_mul, Cert.sumPoly, Poly.eval_add, eqs_zero hK heqs, add_zero] at hid'
    unfold Fact.holds
    cases rel <;> simp only [Bool.and_eq_true] at hrel
    · -- pos
      have hpos := terms_pos hK hterms hrel.1
      have hm := term_nonneg hK hrel.2
      have h1 : 0 < Poly.eval vs (Cert.termPoly K mult) * Poly.eval vs p := by
        rw [hid']; exact hpos
      exact pos_of_mul_pos_right h1 hm
    · -- nonneg
      have hnn := terms_nonneg hK hterms
      have hm := term_pos hK hrel
      have h1 : 0 ≤ Poly.eval vs (Cert.termPoly K mult) * Poly.eval vs p := by
        rw [hid']; exact hnn
      exact nonneg_of_mul_nonneg_right h1 hm
    · -- nonzero (from positivity)
      have hpos := terms_pos hK hterms hrel.1
      have hm := term_nonneg hK hrel.2
      have h1 : 0 < Poly.eval vs (Cert.termPoly K mult) * Poly.eval vs p := by
        rw [hid']; exact hpos
      exact ne_of_gt (pos_of_mul_pos_right h1 hm)
    · -- zero
      have hempty : terms = [] := by simpa using hrel.1
      subst hempty
      have hm := term_ne_zero hK hrel.2
      have h1 : Poly.eval vs (Cert.termPoly K mult) * Poly.eval vs p = 0 := by
        rw [hid']; simp
      exact (mul_eq_zero.mp h1).resolve_left hm
  | nonzero m c refs eqs =>
    simp only [Cert.check, Bool.and_eq_true, beq_iff_eq, bne_iff_ne, ne_eq] at h
    obtain ⟨⟨⟨⟨hrel, hc⟩, heqs⟩, hrefs⟩, hid⟩ := h
    have hid' := Poly.eval_eq_of_beq hid vs
    simp only [Poly.eval_mul, Poly.eval_add, Poly.eval_const, eqs_zero hK heqs, add_zero] at hid'
    subst hrel
    change Poly.eval vs p ≠ 0
    intro hz
    have hp := prod_ne_zero_of hK hrefs
    have h1 : (c : ℝ) * Poly.eval vs (Cert.prodPoly K refs) = 0 := by rw [← hid', hz]; ring
    rcases mul_eq_zero.mp h1 with h0 | h0
    · exact hc (by exact_mod_cast h0)
    · exact hp h0
  | strict i j =>
    simp only [Cert.check, Bool.and_eq_true, beq_iff_eq] at h
    obtain ⟨⟨hrel, hi⟩, hj⟩ := h
    subst hrel
    obtain ⟨g, hg, hge, hgr⟩ := factIs_eval hi
    obtain ⟨g', hg', hge', hgr'⟩ := factIs_eval hj
    have h1 := hK.get i hg
    have h2 := hK.get j hg'
    change 0 < Poly.eval vs p
    have a : 0 ≤ Poly.eval vs p := by
      rw [← hge]
      unfold Fact.holds at h1
      cases hr : g.rel <;> simp_all [Cert.isNonnegRel]
      linarith
    have b : Poly.eval vs p ≠ 0 := by
      rw [← hge']
      unfold Fact.holds at h2
      cases hr : g'.rel <;>
        simp_all only [Cert.isNonzeroRel, ne_eq, Bool.false_eq_true, not_false_eq_true]
      exact ne_of_gt h2
    exact lt_of_le_of_ne a (Ne.symm b)

end Sound

/-! ## Chains -/

/-- The point of the variables: the parameters, then each square root. -/
noncomputable def extend (ps : List ℝ) : List (Poly × Poly) → List ℝ
  | [] => ps
  | nd :: S => extend (ps ++ [√(Poly.eval ps nd.1 / Poly.eval ps nd.2)]) S

/-- A square root step is admissible: it matches the next radicand, which
only involves the variables so far, with `n · d ≥ 0` and `d ≠ 0` known. -/
def sqrtOk (nv : Nat) (K : List Fact) (st : SqrtStep) (nd : Poly × Poly) : Bool :=
  Poly.beq st.n nd.1 && Poly.beq st.d nd.2 &&
    Poly.varsBelow nv st.n && Poly.varsBelow nv st.d &&
    (match K[st.rad]? with
      | some f => Poly.beq f.p (Poly.mul st.n st.d) && (f.rel == .pos || f.rel == .nonneg)
      | none => false) &&
    (match K[st.den]? with
      | some g => Poly.beq g.p st.d && Cert.isNonzeroRel g.rel
      | none => false)

/-- Checks a chain: each step's certificate against the hypotheses and the
facts before it. `nv` is the number of variables so far. Returns the facts
proved, or `none`. -/
def checkChain (nv : Nat) (K : List Fact) : List (Poly × Poly) → List Step → Option (List Fact)
  | S, [] => if S.isEmpty then some K else none
  | S, .fact f c :: rest => if Cert.check K f c then checkChain nv (K ++ [f]) S rest else none
  | [], .sqrt _ :: _ => none
  | nd :: S, .sqrt st :: rest =>
    if sqrtOk nv K st nd then
      let s := Poly.var nv
      checkChain (nv + 1)
        (K ++ [⟨Poly.sub (Poly.mul st.d (Poly.mul s s)) st.n, .zero⟩,
          ⟨s, if Cert.relIs K st.rad Cert.isPosRel then .pos else .nonneg⟩]) S rest
    else none

/-! ## Soundness of chains -/

theorem Mono.eval_append (vs ws : List ℝ) :
    ∀ (m : Mono), m.length ≤ vs.length → Mono.eval (vs ++ ws) m = Mono.eval vs m
  | [], _ => rfl
  | e :: m, h => by
    cases vs with
    | nil => simp at h
    | cons v vs =>
      simp only [List.length_cons, Nat.add_le_add_iff_right] at h
      simp only [List.cons_append, Mono.eval, List.headD_cons, List.tail_cons,
        Mono.eval_append vs ws m h]

theorem Poly.eval_append_of_varsBelow {ps ws : List ℝ} {p : Poly}
    (h : Poly.varsBelow ps.length p = true) : Poly.eval (ps ++ ws) p = Poly.eval ps p := by
  induction p with
  | nil => rfl
  | cons t p ih =>
    simp only [Poly.varsBelow, List.all_cons, Bool.and_eq_true, decide_eq_true_eq] at h
    simp only [Poly.eval_cons]
    rw [← Mono.eval_trim, Mono.eval_append ps ws _ h.1, Mono.eval_trim,
      ih (by simpa [Poly.varsBelow] using h.2)]

theorem extend_prefix : ∀ (ps : List ℝ) (S : List (Poly × Poly)), ∃ ws, extend ps S = ps ++ ws
  | ps, [] => ⟨[], by simp [extend]⟩
  | ps, nd :: S => by
    obtain ⟨ws, h⟩ := extend_prefix (ps ++ [√(Poly.eval ps nd.1 / Poly.eval ps nd.2)]) S
    refine ⟨√(Poly.eval ps nd.1 / Poly.eval ps nd.2) :: ws, ?_⟩
    rw [extend, h]; simp

theorem Mono.eval_var : ∀ (vs : List ℝ) (i : Nat),
    Mono.eval vs (List.replicate i 0 ++ [1]) = vs.getD i 0
  | [], 0 => by simp [Mono.eval]
  | v :: vs, 0 => by simp [Mono.eval]
  | [], i + 1 => by
    simp only [List.replicate_succ, List.cons_append, Mono.eval, List.headD_nil, pow_zero,
      one_mul, List.tail_nil]
    rw [Mono.eval_var [] i]; simp
  | v :: vs, i + 1 => by
    simp only [List.replicate_succ, List.cons_append, Mono.eval, List.headD_cons, pow_zero,
      one_mul, List.tail_cons]
    rw [Mono.eval_var vs i]; simp

theorem Poly.eval_var (vs : List ℝ) (i : Nat) : Poly.eval vs (Poly.var i) = vs.getD i 0 := by
  simp [Poly.var, Mono.eval_var]

theorem checkChain_sound : ∀ (C : List Step) (ps : List ℝ) (S : List (Poly × Poly))
    (K K' : List Fact), AllHold (extend ps S) K →
    checkChain ps.length K S C = some K' → AllHold (extend ps S) K'
  | [], ps, S, K, K', hK, h => by
    rw [checkChain] at h
    split at h
    · cases h; exact hK
    · cases h
  | .fact f c :: rest, ps, S, K, K', hK, h => by
    simp only [checkChain] at h
    split at h
    · rename_i hc
      exact checkChain_sound rest ps S _ K'
        (AllHold.append hK (AllHold.cons (Cert.sound hK hc) (AllHold.nil _))) h
    · cases h
  | .sqrt st :: rest, ps, [], K, K', hK, h => by simp [checkChain] at h
  | .sqrt st :: rest, ps, nd :: S, K, K', hK, h => by
    simp only [checkChain] at h
    split at h
    · rename_i hok
      simp only [sqrtOk, Bool.and_eq_true] at hok
      obtain ⟨⟨⟨⟨⟨hn, hd⟩, hvn⟩, hvd⟩, hrad⟩, hden⟩ := hok
      -- the value of the new variable
      set x := √(Poly.eval ps nd.1 / Poly.eval ps nd.2) with hx
      have hvs : extend ps (nd :: S) = extend (ps ++ [x]) S := rfl
      obtain ⟨ws, hws⟩ := extend_prefix (ps ++ [x]) S
      have hlen : (ps ++ [x]).length = ps.length + 1 := by simp
      -- values of `n`, `d` and `s` at the full point
      have en : Poly.eval (extend ps (nd :: S)) st.n = Poly.eval ps st.n := by
        rw [hvs, hws, List.append_assoc]; exact Poly.eval_append_of_varsBelow hvn
      have ed : Poly.eval (extend ps (nd :: S)) st.d = Poly.eval ps st.d := by
        rw [hvs, hws, List.append_assoc]; exact Poly.eval_append_of_varsBelow hvd
      have es : Poly.eval (extend ps (nd :: S)) (Poly.var ps.length) = x := by
        rw [Poly.eval_var, hvs, hws]; simp
      have en' := Poly.eval_eq_of_beq hn ps
      have ed' := Poly.eval_eq_of_beq hd ps
      -- the facts about the radicand
      have hdne : Poly.eval ps st.d ≠ 0 := by
        split at hden
        · rename_i g hg
          simp only [Bool.and_eq_true] at hden
          have h0 := rel_nonzero (i := st.den) hK (by simp [Cert.relIs, hg, hden.2])
          simp only [hg, Option.map_some, Option.getD_some] at h0
          rwa [Poly.eval_eq_of_beq hden.1, ed] at h0
        · exact absurd hden (by simp)
      have hradf : ∃ f, K[st.rad]? = some f ∧ Poly.beq f.p (Poly.mul st.n st.d) = true ∧
          (f.rel = .pos ∨ f.rel = .nonneg) := by
        split at hrad
        · rename_i f hf
          simp only [Bool.and_eq_true, Bool.or_eq_true, beq_iff_eq] at hrad
          exact ⟨f, hf, hrad.1, hrad.2⟩
        · exact absurd hrad (by simp)
      obtain ⟨f, hf, hfp, hfrel⟩ := hradf
      have hfh := hK.get st.rad hf
      have hfv : Poly.eval (extend ps (nd :: S)) f.p = Poly.eval ps st.n * Poly.eval ps st.d := by
        rw [Poly.eval_eq_of_beq hfp, Poly.eval_mul, en, ed]
      have hq : 0 ≤ Poly.eval ps st.n / Poly.eval ps st.d := by
        have hnd : 0 ≤ Poly.eval ps st.n * Poly.eval ps st.d := by
          rcases hfrel with h | h <;> simp only [Fact.holds, h] at hfh <;> rw [hfv] at hfh <;>
            linarith
        have : Poly.eval ps st.n / Poly.eval ps st.d =
            Poly.eval ps st.n * Poly.eval ps st.d / (Poly.eval ps st.d) ^ 2 := by
          field_simp
        rw [this]; positivity
      have hxx : x ^ 2 = Poly.eval ps nd.1 / Poly.eval ps nd.2 := by
        rw [hx, Real.sq_sqrt (by rw [← en', ← ed']; exact hq)]
      have hrel : Poly.eval ps st.d * x ^ 2 - Poly.eval ps st.n = 0 := by
        rw [hxx, ← en', ← ed']; field_simp; ring
      apply checkChain_sound rest (ps ++ [x]) S _ K' _ (by rw [hlen]; exact h)
      rw [← hvs]
      refine AllHold.append hK (AllHold.cons ?_ (AllHold.cons ?_ (AllHold.nil _)))
      · change Poly.eval _ _ = 0
        rw [Poly.eval_sub, Poly.eval_mul, Poly.eval_mul, es, en, ed]
        linear_combination hrel
      · by_cases hpos : Cert.relIs K st.rad Cert.isPosRel = true
        · simp only [hpos, ite_true, Fact.holds]
          rw [es, hx]
          apply Real.sqrt_pos.mpr
          have h0 := rel_pos hK hpos
          simp only [hf, Option.map_some, Option.getD_some] at h0
          rw [hfv] at h0
          have : Poly.eval ps st.n / Poly.eval ps st.d =
              Poly.eval ps st.n * Poly.eval ps st.d / (Poly.eval ps st.d) ^ 2 := by field_simp
          rw [← en', ← ed', this]; positivity
        · simp only [hpos, Bool.false_eq_true, ite_false, Fact.holds]
          rw [es, hx]; exact Real.sqrt_nonneg _
    · cases h

/-- Hypotheses in the parameters hold at the full point. -/
theorem AllHold.extend_of_varsBelow {ps : List ℝ} {S : List (Poly × Poly)} :
    ∀ {H : List Fact}, H.all (fun f => Poly.varsBelow ps.length f.p) = true →
    AllHold ps H → AllHold (extend ps S) H
  | [], _, _ => trivial
  | f :: H, hv, hH => by
    simp only [List.all_cons, Bool.and_eq_true] at hv
    obtain ⟨ws, hws⟩ := extend_prefix ps S
    refine ⟨?_, AllHold.extend_of_varsBelow hv.2 hH.2⟩
    have := hH.1
    unfold Fact.holds at this ⊢
    rw [hws, Poly.eval_append_of_varsBelow hv.1]
    exact this

/-- A fact with the relation `r` gives one with `s`. -/
def relGives (r s : Rel) : Bool :=
  r == s || (r == .pos && (s == .nonneg || s == .nonzero))

theorem holds_of_relGives {vs : List ℝ} {p : Poly} {r s : Rel} (h : relGives r s = true)
    (hr : Fact.holds vs ⟨p, r⟩) : Fact.holds vs ⟨p, s⟩ := by
  cases r <;> cases s <;>
    first
    | exact hr
    | exact le_of_lt hr
    | exact ne_of_gt hr
    | exact absurd h (by decide)

/-- The goals are facts of the list, by index. -/
def goalsOk (K : List Fact) : List Nat → List Fact → Bool
  | [], [] => true
  | i :: is, g :: gs => Cert.factIs K i g.p (relGives · g.rel) && goalsOk K is gs
  | _, _ => false

/-- Checks a chain from the hypotheses `H` (in the first `n` variables) and
that it proves the goals `G` (at the indices `idx`). -/
def checkGoals (n : Nat) (H : List Fact) (S : List (Poly × Poly)) (C : List Step)
    (idx : List Nat) (G : List Fact) : Bool :=
  H.all (fun f => Poly.varsBelow n f.p) &&
    match checkChain n H S C with
    | some K => goalsOk K idx G
    | none => false

/-- The facts of a checked chain hold at the full point. -/
theorem chain_sound {ps : List ℝ} {S : List (Poly × Poly)} {H : List Fact} {C : List Step}
    {K : List Fact} (hv : H.all (fun f => Poly.varsBelow ps.length f.p) = true)
    (hH : AllHold ps H) (hc : checkChain ps.length H S C = some K) : AllHold (extend ps S) K :=
  checkChain_sound C ps S H K (AllHold.extend_of_varsBelow hv hH) hc

theorem goalsOk_sound {vs : List ℝ} {K : List Fact} (hK : AllHold vs K) :
    ∀ (idx : List Nat) (G : List Fact), goalsOk K idx G = true → AllHold vs G
  | [], [], _ => trivial
  | i :: is, g :: gs, h => by
    simp only [goalsOk, Bool.and_eq_true] at h
    obtain ⟨f, hf, hfe, hfr⟩ := factIs_eval h.1
    refine AllHold.cons ?_ (goalsOk_sound hK is gs h.2)
    have h1 : Fact.holds vs ⟨g.p, f.rel⟩ := by
      have := hK.get i hf
      unfold Fact.holds at this ⊢
      rw [← hfe vs]
      exact this
    exact holds_of_relGives hfr h1
  | [], _ :: _, h => by simp [goalsOk] at h
  | _ :: _, [], h => by simp [goalsOk] at h

/-- The goals of a checked chain hold at the full point. -/
theorem goals_sound {ps : List ℝ} {S : List (Poly × Poly)} {H : List Fact} {C : List Step}
    {idx : List Nat} {G : List Fact} (hc : checkGoals ps.length H S C idx G = true)
    (hH : AllHold ps H) : AllHold (extend ps S) G := by
  simp only [checkGoals, Bool.and_eq_true] at hc
  obtain ⟨hv, hc⟩ := hc
  split at hc
  · rename_i K hK
    exact goalsOk_sound (chain_sound hv hH hK) idx G hc
  · exact absurd hc (by simp)

/-- A polynomial in the first variables has the same value after square roots
are adjoined. -/
theorem eval_extend {ps : List ℝ} {S : List (Poly × Poly)} {p : Poly}
    (h : Poly.varsBelow ps.length p = true) : Poly.eval (extend ps S) p = Poly.eval ps p := by
  obtain ⟨ws, hws⟩ := extend_prefix ps S
  rw [hws, Poly.eval_append_of_varsBelow h]

/-! ## Hypotheses in readable form

A hypothesis `x > 0` (and so on) of a theorem is the fact `⟨P, .pos⟩` with
`P` a positive multiple `c · x` of `x` (as a polynomial). -/

theorem holds_pos_of {vs : List ℝ} {P : Poly} {c x : ℝ} (hc : 0 < c) (h : x > 0)
    (e : Poly.eval vs P = c * x) : Fact.holds vs ⟨P, .pos⟩ := by
  change 0 < Poly.eval vs P; rw [e]; positivity

theorem holds_neg_of {vs : List ℝ} {P : Poly} {c x : ℝ} (hc : 0 < c) (h : x < 0)
    (e : Poly.eval vs P = c * -x) : Fact.holds vs ⟨P, .pos⟩ := by
  change 0 < Poly.eval vs P; rw [e]; exact mul_pos hc (by linarith)

theorem holds_nonneg_of {vs : List ℝ} {P : Poly} {c x : ℝ} (hc : 0 < c) (h : x ≥ 0)
    (e : Poly.eval vs P = c * x) : Fact.holds vs ⟨P, .nonneg⟩ := by
  change 0 ≤ Poly.eval vs P; rw [e]; exact mul_nonneg hc.le h

theorem holds_nonpos_of {vs : List ℝ} {P : Poly} {c x : ℝ} (hc : 0 < c) (h : x ≤ 0)
    (e : Poly.eval vs P = c * -x) : Fact.holds vs ⟨P, .nonneg⟩ := by
  change 0 ≤ Poly.eval vs P; rw [e]; exact mul_nonneg hc.le (by linarith)

theorem holds_nonzero_of {vs : List ℝ} {P : Poly} {c x : ℝ} (hc : c ≠ 0) (h : x ≠ 0)
    (e : Poly.eval vs P = c * x) : Fact.holds vs ⟨P, .nonzero⟩ := by
  change Poly.eval vs P ≠ 0; rw [e]; exact mul_ne_zero hc h

/-- `v ≠ k` as the fact `c (v - k) ≠ 0`. -/
theorem holds_ne_of {vs : List ℝ} {P : Poly} {c v k : ℝ} (hc : c ≠ 0) (h : v ≠ k)
    (e : Poly.eval vs P = c * (v - k)) : Fact.holds vs ⟨P, .nonzero⟩ := by
  change Poly.eval vs P ≠ 0; rw [e]; exact mul_ne_zero hc (sub_ne_zero.mpr h)

/-! ## Coverage of cases

A coverage theorem says that, under some hypotheses, one of several lists of
facts holds (one per case of a `cases` statement). It is proved by a
decision tree on the signs of polynomials (`Cover`): a `split` on `p` goes on
with `p < 0`, `p = 0` and `p > 0` as new facts; a `case` derives the facts of
one case by a chain; an `absurd` derives two contradictory facts. The
polynomials involve the parameters only (no square roots). -/

/-- Some list of facts holds. -/
noncomputable def AnyHold (vs : List ℝ) : List (List Fact) → Prop
  | [] => False
  | d :: ds => AllHold vs d ∨ AnyHold vs ds

theorem AnyHold.of_get {vs : List ℝ} : ∀ {D : List (List Fact)} {k : Nat} {d : List Fact},
    D[k]? = some d → AllHold vs d → AnyHold vs D
  | [], _, _, h, _ => by simp at h
  | _ :: _, 0, _, h, hd => by
    simp only [List.getElem?_cons_zero, Option.some.injEq] at h; subst h; exact Or.inl hd
  | _ :: _, _ + 1, _, h, hd => Or.inr (AnyHold.of_get (by simpa using h) hd)

inductive Cover where
  | split (p : Poly) (neg zero pos : Cover)
  | chain (C : List Step) (next : Cover)
  | case (k : Nat) (C : List Step) (idx : List Nat)
  | absurd (C : List Step) (i j : Nat)
  deriving Repr

/-- Two contradictory facts: `p = 0` and `p ≠ 0` (or `p > 0`), or `p > 0`
and `-p ≥ 0`. -/
def contra (K : List Fact) (i j : Nat) : Bool :=
  match K[i]?, K[j]? with
  | some f, some g =>
    (f.rel == .zero && Cert.isNonzeroRel g.rel && Poly.beq f.p g.p) ||
      (f.rel == .pos && Cert.isNonnegRel g.rel && Poly.beq g.p (Poly.neg f.p))
  | _, _ => false

theorem contra_false {vs : List ℝ} {K : List Fact} {i j : Nat} (hK : AllHold vs K)
    (h : contra K i j = true) : False := by
  unfold contra at h
  split at h
  · rename_i f g hf hg
    have h1 := hK.get i hf
    have h2 := hK.get j hg
    simp only [Bool.or_eq_true, Bool.and_eq_true, beq_iff_eq] at h
    rcases h with ⟨⟨hr, hn⟩, hb⟩ | ⟨⟨hr, hn⟩, hb⟩
    · have e := Poly.eval_eq_of_beq hb vs
      unfold Fact.holds at h1 h2
      rw [hr] at h1
      cases hgr : g.rel <;> simp_all [Cert.isNonzeroRel]
    · have e := Poly.eval_eq_of_beq hb vs
      rw [Poly.eval_neg] at e
      unfold Fact.holds at h1 h2
      rw [hr] at h1
      cases hgr : g.rel <;> simp_all [Cert.isNonnegRel] <;> linarith
  · exact absurd h (by simp)

/-- Checks a decision tree from the facts `K` (in the first `n` variables). -/
def checkCover (n : Nat) (D : List (List Fact)) : List Fact → Cover → Bool
  | K, .split p a b c =>
    checkCover n D (K ++ [⟨Poly.neg p, .pos⟩]) a && checkCover n D (K ++ [⟨p, .zero⟩]) b &&
      checkCover n D (K ++ [⟨p, .pos⟩]) c
  | K, .chain C t =>
    match checkChain n K [] C with
    | some K' => checkCover n D K' t
    | none => false
  | K, .case k C idx =>
    match checkChain n K [] C, D[k]? with
    | some K', some d => goalsOk K' idx d
    | _, _ => false
  | K, .absurd C i j =>
    match checkChain n K [] C with
    | some K' => contra K' i j
    | none => false

theorem checkCover_sound {D : List (List Fact)} : ∀ (T : Cover) (ps : List ℝ) (K : List Fact),
    AllHold ps K → checkCover ps.length D K T = true → AnyHold ps D
  | .split p a b c, ps, K, hK, h => by
    simp only [checkCover, Bool.and_eq_true] at h
    obtain ⟨⟨ha, hb⟩, hc⟩ := h
    rcases lt_trichotomy (Poly.eval ps p) 0 with hp | hp | hp
    · have h1 : Fact.holds ps ⟨Poly.neg p, .pos⟩ := by
        change 0 < Poly.eval ps (Poly.neg p)
        rw [Poly.eval_neg]; linarith
      exact checkCover_sound a ps _ (AllHold.append hK (AllHold.cons h1 (AllHold.nil _))) ha
    · have h1 : Fact.holds ps ⟨p, .zero⟩ := hp
      exact checkCover_sound b ps _ (AllHold.append hK (AllHold.cons h1 (AllHold.nil _))) hb
    · have h1 : Fact.holds ps ⟨p, .pos⟩ := hp
      exact checkCover_sound c ps _ (AllHold.append hK (AllHold.cons h1 (AllHold.nil _))) hc
  | .chain C t, ps, K, hK, h => by
    simp only [checkCover] at h
    split at h
    · rename_i K' hC
      exact checkCover_sound t ps K' (checkChain_sound C ps [] K K' hK hC) h
    · exact absurd h (by simp)
  | .case k C idx, ps, K, hK, h => by
    simp only [checkCover] at h
    split at h
    · rename_i K' d hC hd
      exact AnyHold.of_get hd (goalsOk_sound (checkChain_sound C ps [] K K' hK hC) idx d h)
    · exact absurd h (by simp)
  | .absurd C i j, ps, K, hK, h => by
    simp only [checkCover] at h
    split at h
    · rename_i K' hC
      exact (contra_false (checkChain_sound C ps [] K K' hK hC) h).elim
    · exact absurd h (by simp)

/-- One of the cases holds. -/
theorem cover_sound {ps : List ℝ} {H : List Fact} {D : List (List Fact)} {T : Cover}
    (h : checkCover ps.length D H T = true) (hH : AllHold ps H) : AnyHold ps D :=
  checkCover_sound T ps H hH h

/-! ### Cases in readable form -/

theorem pos_of_holds {vs : List ℝ} {P : Poly} {c x : ℝ} (hc : 0 < c) (e : Poly.eval vs P = c * x)
    (h : Fact.holds vs ⟨P, .pos⟩) : x > 0 := by
  have : 0 < Poly.eval vs P := h
  rw [e] at this; exact pos_of_mul_pos_right this hc.le

theorem neg_of_holds {vs : List ℝ} {P : Poly} {c x : ℝ} (hc : 0 < c) (e : Poly.eval vs P = c * -x)
    (h : Fact.holds vs ⟨P, .pos⟩) : x < 0 := by
  have : 0 < Poly.eval vs P := h
  rw [e] at this; have := pos_of_mul_pos_right this hc.le; linarith

theorem nonneg_of_holds {vs : List ℝ} {P : Poly} {c x : ℝ} (hc : 0 < c)
    (e : Poly.eval vs P = c * x) (h : Fact.holds vs ⟨P, .nonneg⟩) : x ≥ 0 := by
  have : 0 ≤ Poly.eval vs P := h
  rw [e] at this; exact nonneg_of_mul_nonneg_right this hc

theorem nonpos_of_holds {vs : List ℝ} {P : Poly} {c x : ℝ} (hc : 0 < c)
    (e : Poly.eval vs P = c * -x) (h : Fact.holds vs ⟨P, .nonneg⟩) : x ≤ 0 := by
  have : 0 ≤ Poly.eval vs P := h
  rw [e] at this; have := nonneg_of_mul_nonneg_right this hc; linarith

theorem nonzero_of_holds {vs : List ℝ} {P : Poly} {c x : ℝ} (e : Poly.eval vs P = c * x)
    (h : Fact.holds vs ⟨P, .nonzero⟩) : x ≠ 0 := by
  have : Poly.eval vs P ≠ 0 := h
  rw [e] at this; exact right_ne_zero_of_mul this

theorem ne_of_holds {vs : List ℝ} {P : Poly} {c v k : ℝ} (e : Poly.eval vs P = c * (v - k))
    (h : Fact.holds vs ⟨P, .nonzero⟩) : v ≠ k := by
  have : Poly.eval vs P ≠ 0 := h
  rw [e] at this; exact sub_ne_zero.mp (right_ne_zero_of_mul this)

theorem eq_of_holds {vs : List ℝ} {P : Poly} {c v k : ℝ} (hc : c ≠ 0)
    (e : Poly.eval vs P = c * (v - k)) (h : Fact.holds vs ⟨P, .zero⟩) : v = k := by
  have : Poly.eval vs P = 0 := h
  rw [e] at this
  exact sub_eq_zero.mp ((mul_eq_zero.mp this).resolve_left hc)

theorem eq_div_of_holds {vs : List ℝ} {P : Poly} {c v n m : ℝ} (hc : c ≠ 0) (hm : m ≠ 0)
    (e : Poly.eval vs P = c * (v * m - n)) (h : Fact.holds vs ⟨P, .zero⟩) : v = n / m := by
  have : Poly.eval vs P = 0 := h
  rw [e] at this
  have := sub_eq_zero.mp ((mul_eq_zero.mp this).resolve_left hc)
  rw [eq_div_iff hm, this]

end StraightedgeCompass
