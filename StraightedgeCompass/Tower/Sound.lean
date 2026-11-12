module

public import StraightedgeCompass.Tower
public import Mathlib.Analysis.Real.Sqrt
public import Mathlib.Tactic.LinearCombination

/-!
# Soundness of tower arithmetic

`Tree.denote rs t` is the real number a tree stands for. When the tower is
well formed (`WF`: every radicand positive), the operations of
`StraightedgeCompass.Tower` agree with real arithmetic, and `Tree.sign`
computes the sign. Nothing here needs the radicands to be non-squares: that
only matters for completeness (equal numbers having equal trees), not for
soundness.
-/

@[expose] public section

namespace StraightedgeCompass

/-- `s` is the sign of `x`. -/
def SignSpec (s : Int) (x : ℝ) : Prop :=
  (s = 1 ∧ 0 < x) ∨ (s = 0 ∧ x = 0) ∨ (s = -1 ∧ x < 0)

theorem signSpec_int (n : Int) : SignSpec (Int.sign n) (n : ℝ) := by
  rcases lt_trichotomy n 0 with h | rfl | h
  · exact Or.inr (Or.inr ⟨Int.sign_eq_neg_one_of_neg h, by exact_mod_cast h⟩)
  · exact Or.inr (Or.inl ⟨rfl, by simp⟩)
  · exact Or.inl ⟨Int.sign_eq_one_of_pos h, by exact_mod_cast h⟩

theorem SignSpec.neg {s : Int} {x : ℝ} (h : SignSpec s x) : SignSpec (-s) (-x) := by
  rcases h with ⟨rfl, h⟩ | ⟨rfl, h⟩ | ⟨rfl, h⟩
  · exact Or.inr (Or.inr ⟨rfl, by linarith⟩)
  · exact Or.inr (Or.inl ⟨rfl, by simp [h]⟩)
  · exact Or.inl ⟨rfl, by linarith⟩

/-- Multiplying by a positive number does not change the sign. -/
theorem SignSpec.of_mul_pos {s : Int} {x w n : ℝ} (hw : 0 < w) (hn : x * w = n)
    (h : SignSpec s n) : SignSpec s x := by
  subst hn
  rcases h with ⟨rfl, h⟩ | ⟨rfl, h⟩ | ⟨rfl, h⟩
  · exact Or.inl ⟨rfl, pos_of_mul_pos_left h hw.le⟩
  · exact Or.inr (Or.inl ⟨rfl, (mul_eq_zero.mp h).resolve_right hw.ne'⟩)
  · exact Or.inr (Or.inr ⟨rfl, neg_of_mul_neg_left h hw.le⟩)

theorem SignSpec.eq_zero {x : ℝ} (h : SignSpec 0 x) : x = 0 := by
  rcases h with ⟨h, _⟩ | ⟨_, h⟩ | ⟨h, _⟩ <;> first | exact h | omega

theorem SignSpec.pos {x : ℝ} (h : SignSpec 1 x) : 0 < x := by
  rcases h with ⟨_, h⟩ | ⟨h, _⟩ | ⟨h, _⟩ <;> first | exact h | omega

theorem SignSpec.neg_of {x : ℝ} (h : SignSpec (-1) x) : x < 0 := by
  rcases h with ⟨h, _⟩ | ⟨h, _⟩ | ⟨_, h⟩ <;> first | exact h | omega

theorem SignSpec.ne_zero {s : Int} {x : ℝ} (h : SignSpec s x) (hs : s ≠ 0) : x ≠ 0 := by
  rcases h with ⟨_, h⟩ | ⟨h, _⟩ | ⟨_, h⟩
  · exact h.ne'
  · exact absurd h hs
  · exact h.ne

namespace Tree

/-- The real number a tree stands for in a tower (top level first). -/
noncomputable def denote : List Tree → Tree → ℝ
  | [], t => (value0 t : ℝ)
  | _ :: _, leaf n => (n : ℝ)
  | ρ :: rs, node a b => denote rs a + denote rs b * √(denote rs ρ)

/-- Every radicand is positive in the tower below it. -/
def WF : List Tree → Prop
  | [] => True
  | ρ :: rs => 0 < denote rs ρ ∧ WF rs

@[simp] theorem denote_nil (t : Tree) : denote [] t = value0 t := by
  cases t <;> rfl

@[simp] theorem denote_leaf (rs : List Tree) (n : Int) : denote rs (leaf n) = n := by
  cases rs <;> rfl

@[simp] theorem denote_node (ρ : Tree) (rs : List Tree) (a b : Tree) :
    denote (ρ :: rs) (node a b) = denote rs a + denote rs b * √(denote rs ρ) := rfl

theorem value0_scale (c : Int) (t : Tree) : value0 (scale c t) = c * value0 t := by
  induction t with
  | leaf n => rfl
  | node a b iha _ => simpa [scale, value0] using iha

theorem denote_scale (rs : List Tree) (c : Int) (t : Tree) :
    denote rs (scale c t) = c * denote rs t := by
  induction rs generalizing t with
  | nil => simp [value0_scale]
  | cons ρ rs ih =>
    cases t with
    | leaf n => simp [scale]
    | node a b => simp only [scale, denote_node, ih]; ring

theorem denote_neg (rs : List Tree) (t : Tree) : denote rs (neg t) = -denote rs t := by
  simp [neg, denote_scale]

theorem denote_add (rs : List Tree) (a b : Tree) :
    denote rs (add rs a b) = denote rs a + denote rs b := by
  induction rs generalizing a b with
  | nil => simp [add]
  | cons ρ rs ih =>
    cases a <;> cases b <;> simp only [add, denote_leaf, denote_node, ih] <;> push_cast <;> ring

theorem denote_sub (rs : List Tree) (a b : Tree) :
    denote rs (sub rs a b) = denote rs a - denote rs b := by
  simp [sub, denote_add, denote_neg, sub_eq_add_neg]

theorem denote_mul {rs : List Tree} (h : WF rs) (a b : Tree) :
    denote rs (mul rs a b) = denote rs a * denote rs b := by
  induction rs generalizing a b with
  | nil => simp [mul]
  | cons ρ rs ih =>
    obtain ⟨hρ, hrs⟩ := h
    cases a with
    | leaf m => simp [mul, denote_scale]
    | node a₀ a₁ =>
      cases b with
      | leaf n => rw [mul, denote_scale, denote_leaf]; ring
      | node b₀ b₁ =>
        have hs : √(denote rs ρ) * √(denote rs ρ) = denote rs ρ := Real.mul_self_sqrt hρ.le
        simp only [mul, denote_node, denote_add, ih hrs]
        linear_combination (-(denote rs a₁ * denote rs b₁)) * hs

theorem value0_of_isZero {t : Tree} (h : isZero t = true) : value0 t = 0 := by
  induction t with
  | leaf n => simpa [isZero, value0] using h
  | node a b iha _ =>
    simp only [isZero, Bool.and_eq_true] at h
    simpa [value0] using iha h.1

theorem denote_of_isZero (rs : List Tree) {t : Tree} (h : isZero t = true) :
    denote rs t = 0 := by
  induction rs generalizing t with
  | nil => simp [value0_of_isZero h]
  | cons ρ rs ih =>
    cases t with
    | leaf n => simpa [isZero] using h
    | node a b =>
      simp only [isZero, Bool.and_eq_true] at h
      simp [ih h.1, ih h.2]

theorem sign_spec {rs : List Tree} (h : WF rs) (t : Tree) :
    SignSpec (sign rs t) (denote rs t) := by
  induction rs generalizing t with
  | nil => simpa [sign] using signSpec_int (value0 t)
  | cons ρ rs ih =>
    obtain ⟨hρ, hrs⟩ := h
    cases t with
    | leaf n => simpa [sign] using signSpec_int n
    | node a b =>
      have hs : 0 < √(denote rs ρ) := Real.sqrt_pos.mpr hρ
      have hss : √(denote rs ρ) * √(denote rs ρ) = denote rs ρ := Real.mul_self_sqrt hρ.le
      -- the norm a² − ρb², the product of a + b√ρ and its conjugate a − b√ρ
      have hn : (denote rs a + denote rs b * √(denote rs ρ)) *
          (denote rs a - denote rs b * √(denote rs ρ)) =
            denote rs (sub rs (mul rs a a) (mul rs ρ (mul rs b b))) := by
        rw [denote_sub, denote_mul hrs, denote_mul hrs, denote_mul hrs]
        linear_combination (-(denote rs b * denote rs b)) * hss
      have hN := ih hrs (sub rs (mul rs a a) (mul rs ρ (mul rs b b)))
      simp only [sign, denote_node]
      rcases ih hrs a with ⟨ha, ha'⟩ | ⟨ha, ha'⟩ | ⟨ha, ha'⟩ <;>
        rcases ih hrs b with ⟨hb, hb'⟩ | ⟨hb, hb'⟩ | ⟨hb, hb'⟩ <;>
        simp only [ha, hb] <;> norm_num
      -- a > 0, b > 0
      · exact Or.inl ⟨rfl, by positivity⟩
      -- a > 0, b = 0
      · exact Or.inl ⟨rfl, by rw [hb']; simpa using ha'⟩
      -- a > 0, b < 0: the sign of the norm
      · exact hN.of_mul_pos (by nlinarith) hn
      -- a = 0, b > 0
      · exact Or.inl ⟨rfl, by rw [ha']; positivity⟩
      -- a = 0, b = 0
      · exact Or.inr (Or.inl ⟨rfl, by rw [ha', hb']; ring⟩)
      -- a = 0, b < 0
      · exact Or.inr (Or.inr ⟨rfl, by rw [ha']; nlinarith⟩)
      -- a < 0, b > 0: the opposite sign of the norm
      · have hw : 0 < -(denote rs a - denote rs b * √(denote rs ρ)) := by nlinarith
        exact hN.neg.of_mul_pos hw (by rw [← hn]; ring)
      -- a < 0, b = 0
      · exact Or.inr (Or.inr ⟨rfl, by rw [hb']; simpa using ha'⟩)
      -- a < 0, b < 0
      · exact Or.inr (Or.inr ⟨rfl, by nlinarith⟩)

theorem wf_of_towerOk : ∀ {rs : List Tree}, towerOk rs = true → WF rs
  | [], _ => trivial
  | ρ :: rs, h => by
    simp only [towerOk, Bool.and_eq_true, beq_iff_eq] at h
    have hrs : WF rs := wf_of_towerOk h.2
    exact ⟨(h.1 ▸ sign_spec hrs ρ).pos, hrs⟩

end Tree

namespace Elt

/-- The real number an element stands for. -/
noncomputable def denote (T : List Tree) (x : Elt) : ℝ := Tree.denote T x.num / x.den

variable {T : List Tree}

theorem denote_add {x y : Elt} (hx : 0 < x.den) (hy : 0 < y.den) :
    denote T (add T x y) = denote T x + denote T y := by
  have hx' : (x.den : ℝ) ≠ 0 := by positivity
  have hy' : (y.den : ℝ) ≠ 0 := by positivity
  simp only [denote, add, Tree.denote_add, Tree.denote_scale, Nat.cast_mul, Int.cast_natCast]
  field_simp

theorem denote_neg (x : Elt) : denote T (neg x) = -denote T x := by
  simp [denote, neg, Tree.denote_neg, neg_div]

theorem denote_sub {x y : Elt} (hx : 0 < x.den) (hy : 0 < y.den) :
    denote T (sub T x y) = denote T x - denote T y := by
  rw [sub, denote_add hx (by simpa [neg] using hy), denote_neg, sub_eq_add_neg]

theorem denote_mul (h : Tree.WF T) (x y : Elt) :
    denote T (mul T x y) = denote T x * denote T y := by
  simp only [denote, mul, Tree.denote_mul h, Nat.cast_mul, div_mul_div_comm]

theorem den_add (x y : Elt) : (add T x y).den = x.den * y.den := rfl
theorem den_neg (x : Elt) : (neg x).den = x.den := rfl
theorem den_sub (x y : Elt) : (sub T x y).den = x.den * y.den := rfl
theorem den_mul (x y : Elt) : (mul T x y).den = x.den * y.den := rfl

theorem sign_spec (h : Tree.WF T) {x : Elt} (hx : 0 < x.den) :
    SignSpec (sign T x) (denote T x) := by
  have hd : (0 : ℝ) < x.den := by exact_mod_cast hx
  refine (Tree.sign_spec h x.num).of_mul_pos hd ?_
  simp [denote, div_mul_cancel₀ _ hd.ne']

theorem denote_of_isZero {x : Elt} (h : isZero x = true) : denote T x = 0 := by
  simp [denote, Tree.denote_of_isZero T h]

end Elt

end StraightedgeCompass
