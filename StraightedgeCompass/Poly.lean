module

public import Mathlib.Basic.Real.Basic
public import Mathlib.Tactic.Ring
public import Mathlib.Tactic.Linarith
public import Mathlib.Tactic.LinearCombination

/-!
# Polynomials for kernel computation

Multivariate polynomials with integer coefficients in the variables
`0, 1, 2, …`, as lists of terms `(exponents, coefficient)`. The operations
keep a canonical form (terms in a fixed order, like terms combined, zero
terms dropped, trailing zero exponents trimmed), so that equal polynomials
are equal lists and `decide +kernel` can compare them. Every function is
structurally recursive, so that the kernel can evaluate it.

Soundness (`eval_add`, `eval_mul`, …) holds for all lists, canonical or not:
only the completeness of equality tests depends on the canonical form, which
`norm` restores. Addition merges the two lists (linear for canonical inputs),
and multiplication merges the products of the terms of `p` with `q`.
-/

@[expose] public section

namespace StraightedgeCompass

/-- A monomial: exponents by variable. -/
abbrev Mono := List Nat

/-- A polynomial: a list of terms. -/
abbrev Poly := List (Mono × Int)

namespace Mono

/-- Drops trailing zero exponents. -/
def trim : Mono → Mono
  | [] => []
  | e :: m => if (trim m).isEmpty && e == 0 then [] else e :: trim m

/-- The product of monomials: exponents add. -/
def mul : Mono → Mono → Mono
  | [], b => b
  | a, [] => a
  | x :: a, y :: b => (x + y) :: mul a b

/-- Compares a monomial with `1` (the empty monomial). -/
def cmpOne : Mono → Ordering
  | [] => .eq
  | x :: a => if x == 0 then cmpOne a else .gt

/-- A total order on monomials (missing exponents count as zero). -/
def cmp : Mono → Mono → Ordering
  | [], b => (cmpOne b).swap
  | x :: a, [] => if x == 0 then cmpOne a else .gt
  | x :: a, y :: b => if x < y then .lt else if y < x then .gt else cmp a b

/-- The value of a monomial; variables beyond the list are `0`. -/
noncomputable def eval (vs : List ℝ) : Mono → ℝ
  | [] => 1
  | e :: m => (vs.headD 0) ^ e * eval vs.tail m

end Mono

namespace Poly

/-- Inserts a term, keeping the order and combining like terms. -/
def insert (t : Mono × Int) : Poly → Poly
  | [] => if t.2 == 0 then [] else [(t.1.trim, t.2)]
  | s :: p =>
    match Mono.cmp t.1 s.1 with
    | .lt => if t.2 == 0 then s :: p else (t.1.trim, t.2) :: s :: p
    | .eq => if t.2 + s.2 == 0 then p else (s.1, t.2 + s.2) :: p
    | .gt => s :: insert t p

/-- Merges two term lists, combining like terms; `fuel` (the total length
suffices) keeps the recursion structural. -/
def merge : Nat → Poly → Poly → Poly
  | 0, p, q => p ++ q
  | _ + 1, [], q => q
  | _ + 1, p, [] => p
  | k + 1, s :: p, t :: q =>
    match Mono.cmp s.1 t.1 with
    | .lt => s :: merge k p (t :: q)
    | .gt => t :: merge k (s :: p) q
    | .eq => if s.2 + t.2 == 0 then merge k p q else (s.1, s.2 + t.2) :: merge k p q

def add (p q : Poly) : Poly := merge (p.length + q.length) p q

def scale (c : Int) (p : Poly) : Poly := p.foldr (fun t acc => insert (t.1, c * t.2) acc) []

def neg (p : Poly) : Poly := scale (-1) p

def sub (p q : Poly) : Poly := add p (neg q)

/-- The products of one term with the terms of `q` (in order, for a canonical
`q`: the monomial order is compatible with products). -/
def mulTerm (s : Mono × Int) (q : Poly) : Poly :=
  if s.2 == 0 then [] else q.map fun t => (Mono.mul s.1 t.1, s.2 * t.2)

def mul (p q : Poly) : Poly := p.foldr (fun s acc => add (mulTerm s q) acc) []

def const (c : Int) : Poly := insert ([], c) []

/-- The variable with index `i`. -/
def var (i : Nat) : Poly := [(List.replicate i 0 ++ [1], 1)]

def pow (p : Poly) : Nat → Poly
  | 0 => const 1
  | n + 1 => mul p (pow p n)

/-- The canonical form (insertion sort; linear for a sorted list). -/
def norm (p : Poly) : Poly := p.foldr insert []

/-- Equal as polynomials (canonical forms compared). -/
def beq (p q : Poly) : Bool := norm (sub p q) == []

/-- Every variable index is below `n`. -/
def varsBelow (n : Nat) (p : Poly) : Bool := p.all fun t => (t.1.trim).length ≤ n

/-- The value at `vs` (missing variables are `0`). -/
noncomputable def eval (vs : List ℝ) (p : Poly) : ℝ :=
  p.foldr (fun t acc => (t.2 : ℝ) * Mono.eval vs t.1 + acc) 0

end Poly

/-! ## Soundness -/

namespace Mono

theorem eval_mul (vs : List ℝ) (a b : Mono) : eval vs (mul a b) = eval vs a * eval vs b := by
  induction a generalizing vs b with
  | nil => simp [mul, eval]
  | cons x a ih =>
    cases b with
    | nil => simp [mul, eval]
    | cons y b => simp only [mul, eval, ih, pow_add]; ring

/-- Trailing zero exponents do not change the value. -/
theorem eval_trim (vs : List ℝ) (m : Mono) : eval vs m.trim = eval vs m := by
  induction m generalizing vs with
  | nil => rfl
  | cons e m ih =>
    simp only [trim]
    split
    · rename_i h
      simp only [Bool.and_eq_true, List.isEmpty_iff, beq_iff_eq] at h
      obtain ⟨h1, rfl⟩ := h
      have := ih vs.tail
      rw [h1] at this
      simp [eval, ← this]
    · simp [eval, ih]

theorem eval_of_cmpOne_eq (vs : List ℝ) : ∀ (m : Mono), cmpOne m = .eq → eval vs m = 1
  | [], _ => rfl
  | x :: a, h => by
    simp only [cmpOne] at h
    split at h
    · rename_i hx
      have hx : x = 0 := by simpa using hx
      subst hx
      simp [eval, eval_of_cmpOne_eq vs.tail a h]
    · exact absurd h (by decide)

/-- Monomials that compare equal have equal values. -/
theorem eval_of_cmp_eq (vs : List ℝ) : ∀ (a b : Mono), cmp a b = .eq → eval vs a = eval vs b
  | [], b, h => by
    simp only [cmp] at h
    have : cmpOne b = .eq := by
      cases hb : cmpOne b <;> simp_all [Ordering.swap]
    rw [eval_of_cmpOne_eq vs b this]; rfl
  | x :: a, [], h => by
    simp only [cmp] at h
    split at h
    · rename_i hx
      have hx : x = 0 := by simpa using hx
      subst hx
      simp [eval, eval_of_cmpOne_eq vs.tail a h]
    · exact absurd h (by decide)
  | x :: a, y :: b, h => by
    simp only [cmp] at h
    split at h
    · exact absurd h (by decide)
    · split at h
      · exact absurd h (by decide)
      · rename_i h1 h2
        have : x = y := by omega
        subst this
        simp only [eval, eval_of_cmp_eq vs.tail a b h]

end Mono

namespace Poly

@[simp] theorem eval_nil (vs : List ℝ) : eval vs [] = 0 := rfl

@[simp] theorem eval_cons (vs : List ℝ) (t : Mono × Int) (p : Poly) :
    eval vs (t :: p) = (t.2 : ℝ) * Mono.eval vs t.1 + eval vs p := rfl

theorem eval_insert (vs : List ℝ) (t : Mono × Int) (p : Poly) :
    eval vs (insert t p) = (t.2 : ℝ) * Mono.eval vs t.1 + eval vs p := by
  induction p with
  | nil =>
    simp only [insert]
    split
    · rename_i h; simp [show t.2 = 0 by simpa using h]
    · simp [Mono.eval_trim]
  | cons s p ih =>
    simp only [insert]
    split
    · split
      · rename_i h; simp [show t.2 = 0 by simpa using h]
      · simp [Mono.eval_trim]
    · rename_i hc
      have he := Mono.eval_of_cmp_eq vs _ _ hc
      split
      · rename_i h
        have h' : t.2 + s.2 = 0 := by simpa using h
        simp only [eval_cons, he]
        have : (t.2 : ℝ) + s.2 = 0 := by exact_mod_cast h'
        linear_combination (-(Mono.eval vs s.1)) * this
      · rw [eval_cons, eval_cons, he]; push_cast; ring
    · simp only [eval_cons, ih]; ring

theorem eval_append' (vs : List ℝ) (p q : Poly) : eval vs (p ++ q) = eval vs p + eval vs q := by
  induction p with
  | nil => simp
  | cons t p ih => simp only [List.cons_append, eval_cons, ih]; ring

theorem eval_merge (vs : List ℝ) : ∀ (k : Nat) (p q : Poly),
    eval vs (merge k p q) = eval vs p + eval vs q
  | 0, p, q => by simp [merge, eval_append']
  | _ + 1, [], q => by simp [merge]
  | _ + 1, _ :: _, [] => by simp [merge]
  | k + 1, s :: p, t :: q => by
    simp only [merge]
    split
    · rw [eval_cons, eval_merge vs k p (t :: q), eval_cons, eval_cons]; ring
    · rw [eval_cons, eval_merge vs k (s :: p) q, eval_cons, eval_cons]; ring
    · rename_i hc
      have he := Mono.eval_of_cmp_eq vs _ _ hc
      split
      · rename_i h
        have h' : s.2 + t.2 = 0 := by simpa using h
        have : (s.2 : ℝ) + t.2 = 0 := by exact_mod_cast h'
        rw [eval_merge vs k p q, eval_cons, eval_cons, he]
        linear_combination (-(Mono.eval vs t.1)) * this
      · rw [eval_cons, eval_merge vs k p q, eval_cons, eval_cons, he]; push_cast; ring

theorem eval_add (vs : List ℝ) (p q : Poly) : eval vs (add p q) = eval vs p + eval vs q :=
  eval_merge vs _ p q

theorem eval_scale (vs : List ℝ) (c : Int) (p : Poly) : eval vs (scale c p) = c * eval vs p := by
  induction p with
  | nil => simp [scale]
  | cons t p ih =>
    simp only [scale, List.foldr_cons] at *
    rw [eval_insert, ih]; simp; ring

theorem eval_neg (vs : List ℝ) (p : Poly) : eval vs (neg p) = - eval vs p := by
  simp [neg, eval_scale]

theorem eval_sub (vs : List ℝ) (p q : Poly) : eval vs (sub p q) = eval vs p - eval vs q := by
  simp [sub, eval_add, eval_neg]; ring

theorem eval_mulTerm (vs : List ℝ) (s : Mono × Int) (q : Poly) :
    eval vs (mulTerm s q) = (s.2 : ℝ) * Mono.eval vs s.1 * eval vs q := by
  unfold mulTerm
  split
  · rename_i h; simp [show s.2 = 0 by simpa using h]
  · induction q with
    | nil => simp
    | cons t q ihq =>
      rw [List.map_cons, eval_cons, ihq, eval_cons, Mono.eval_mul]
      push_cast; ring

theorem eval_mul (vs : List ℝ) (p q : Poly) : eval vs (mul p q) = eval vs p * eval vs q := by
  induction p with
  | nil => simp [mul]
  | cons s p ih =>
    simp only [mul, List.foldr_cons] at *
    rw [eval_add, eval_mulTerm, ih]; simp; ring

theorem eval_const (vs : List ℝ) (c : Int) : eval vs (const c) = c := by
  simp [const, eval_insert, Mono.eval]

theorem eval_pow (vs : List ℝ) (p : Poly) (n : Nat) : eval vs (pow p n) = eval vs p ^ n := by
  induction n with
  | zero => simp [pow, eval_const]
  | succ n ih => simp [pow, eval_mul, ih, pow_succ]; ring

theorem eval_norm (vs : List ℝ) (p : Poly) : eval vs (norm p) = eval vs p := by
  induction p with
  | nil => rfl
  | cons t p ih => simp only [norm, List.foldr_cons] at *; rw [eval_insert, ih]; rfl

/-- Polynomials equal as polynomials have equal values. -/
theorem eval_eq_of_beq {p q : Poly} (h : beq p q = true) (vs : List ℝ) :
    eval vs p = eval vs q := by
  have h0 : norm (sub p q) = [] := by simpa [beq] using h
  have : eval vs (norm (sub p q)) = 0 := by rw [h0]; rfl
  rw [eval_norm, eval_sub] at this
  linarith

end Poly

end StraightedgeCompass
