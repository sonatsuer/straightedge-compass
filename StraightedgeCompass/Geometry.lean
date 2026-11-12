module

public import StraightedgeCompass.Tower.Sound
public import Mathlib.Analysis.InnerProductSpace.PiL2
public import Mathlib.Analysis.Convex.Between
public import Mathlib.Geometry.Euclidean.Angle.Unoriented.Affine
public import Mathlib.Geometry.Euclidean.Sphere.Basic
public import Mathlib.LinearAlgebra.AffineSpace.FiniteDimensional

/-!
# Geometry: from tower checks to Mathlib statements

Points of the plane are `EuclideanSpace ℝ (Fin 2)`. A straightedge draws
`lineThrough A B = line[ℝ, A, B]`, a compass draws
`circleThrough O A = Metric.sphere O (dist O A)`.

A `Pt` with coordinates in a tower becomes a point by `Pt.toP2`. Each lemma
below turns a Boolean check, which the kernel evaluates (`decide +kernel`),
into a statement about those points: incidence with lines and circles,
equality and distinctness, comparisons of distances, orientation.
-/

@[expose] public section

namespace StraightedgeCompass

/-- The Euclidean plane. -/
abbrev P2 := EuclideanSpace ℝ (Fin 2)

/-- The line through two points (drawn with the straightedge). -/
noncomputable abbrev lineThrough (A B : P2) : AffineSubspace ℝ P2 := line[ℝ, A, B]

/-- The circle with centre `O` through `A` (drawn with the compass). -/
noncomputable abbrev circleThrough (O A : P2) : Set P2 := Metric.sphere O (dist O A)

/-- `(B − A) × (X − A)`, twice the signed area of the triangle `A B X`. -/
noncomputable def cross2 (A B X : P2) : ℝ := (B 0 - A 0) * (X 1 - A 1) - (B 1 - A 1) * (X 0 - A 0)

/-- `(B − A) · (C − A)`. -/
noncomputable def dot2 (A B C : P2) : ℝ := (B 0 - A 0) * (C 0 - A 0) + (B 1 - A 1) * (C 1 - A 1)

/-- `(B − A) × (D − C)`, zero when the two directions are parallel. -/
noncomputable def crossDir2 (A B C D : P2) : ℝ :=
  (B 0 - A 0) * (D 1 - C 1) - (B 1 - A 1) * (D 0 - C 0)

/-- `(B − A) · (D − C)`, zero when the two directions are perpendicular. -/
noncomputable def dotDir2 (A B C D : P2) : ℝ :=
  (B 0 - A 0) * (D 0 - C 0) + (B 1 - A 1) * (D 1 - C 1)

/-- `X` lies strictly to the left of the directed line `A → B`; equivalently,
`A`, `B`, `X` turn counterclockwise. -/
def IsLeft (A B X : P2) : Prop := 0 < cross2 A B X

/-- Lexicographic order of points (x first, then y). -/
def LexLt (X Y : P2) : Prop := X 0 < Y 0 ∨ (X 0 = Y 0 ∧ X 1 < Y 1)

theorem dist_sq_coords (p q : P2) : dist p q ^ 2 = (p 0 - q 0) ^ 2 + (p 1 - q 1) ^ 2 := by
  rw [EuclideanSpace.dist_sq_eq, Fin.sum_univ_two, Real.dist_eq, Real.dist_eq, sq_abs, sq_abs]

theorem ext_coords {p q : P2} (h0 : p 0 = q 0) (h1 : p 1 = q 1) : p = q := by
  ext i
  fin_cases i
  · exact h0
  · exact h1

theorem cross2_eq_zero_of_mem {A B X : P2} (h : X ∈ lineThrough A B) : cross2 A B X = 0 := by
  obtain ⟨r, rfl⟩ := mem_affineSpan_pair_iff_exists_lineMap_eq.mp h
  simp only [cross2, AffineMap.lineMap_apply_module, PiLp.add_apply, PiLp.smul_apply, smul_eq_mul]
  ring

theorem mem_line_of_cross2 {A B X : P2} (hAB : A ≠ B) (h : cross2 A B X = 0) :
    X ∈ lineThrough A B := by
  rw [mem_affineSpan_pair_iff_exists_lineMap_eq]
  have hc : (B 0 - A 0) * (X 1 - A 1) = (B 1 - A 1) * (X 0 - A 0) := by
    unfold cross2 at h; linarith
  by_cases h0 : A 0 = B 0
  · have h1 : B 1 - A 1 ≠ 0 := fun h1 => hAB (ext_coords h0 (by linarith))
    have hx0 : X 0 = A 0 := by
      have : (B 1 - A 1) * (X 0 - A 0) = 0 := by rw [← hc, ← h0, sub_self, zero_mul]
      linarith [(mul_eq_zero.mp this).resolve_left h1]
    have hr : (X 1 - A 1) / (B 1 - A 1) * (B 1 - A 1) = X 1 - A 1 := div_mul_cancel₀ _ h1
    refine ⟨(X 1 - A 1) / (B 1 - A 1), ext_coords ?_ ?_⟩ <;>
      simp only [AffineMap.lineMap_apply_module, PiLp.add_apply, PiLp.smul_apply, smul_eq_mul]
    · rw [← h0, hx0]; ring
    · linear_combination hr
  · have h0' : B 0 - A 0 ≠ 0 := fun h => h0 (by linarith)
    have hr : (X 0 - A 0) / (B 0 - A 0) * (B 0 - A 0) = X 0 - A 0 := div_mul_cancel₀ _ h0'
    have hr' : (X 0 - A 0) / (B 0 - A 0) * (B 1 - A 1) = X 1 - A 1 := by
      rw [div_mul_eq_mul_div, div_eq_iff h0']; linarith
    refine ⟨(X 0 - A 0) / (B 0 - A 0), ext_coords ?_ ?_⟩ <;>
      simp only [AffineMap.lineMap_apply_module, PiLp.add_apply, PiLp.smul_apply, smul_eq_mul]
    · linear_combination hr
    · linear_combination hr'

/-- Positivity of the denominator of a computed element, from the positivity
of the denominators of the points involved (hypotheses in context). -/
syntax "den_pos" : tactic
macro_rules
  | `(tactic| den_pos) =>
    `(tactic| (simp only [Pt.dist2, Pt.cross, Pt.dot, Pt.crossDir, Pt.dotDir, Elt.den_add,
      Elt.den_sub, Elt.den_mul, Elt.den_neg] <;> positivity))

namespace Pt

variable {T : List Tree}

/-- The point with these coordinates. -/
noncomputable def toP2 (T : List Tree) (p : Pt) : P2 := !₂[Elt.denote T p.x, Elt.denote T p.y]

@[simp] theorem toP2_zero (p : Pt) : toP2 T p 0 = Elt.denote T p.x := rfl
@[simp] theorem toP2_one (p : Pt) : toP2 T p 1 = Elt.denote T p.y := rfl

/-- Both denominators are positive. -/
def Ok (p : Pt) : Prop := 0 < p.x.den ∧ 0 < p.y.den

theorem ok_of_denOk {p : Pt} (h : p.denOk = true) : p.Ok := by
  simpa [Ok, denOk, Elt.denOk] using h

section
variable {p q r : Pt} (hT : Tree.WF T) (hp : p.Ok) (hq : q.Ok) (hr : r.Ok)
include hT hp hq

theorem denote_dist2 : Elt.denote T (dist2 T p q) = dist (toP2 T p) (toP2 T q) ^ 2 := by
  obtain ⟨hp1, hp2⟩ := hp
  obtain ⟨hq1, hq2⟩ := hq
  rw [dist_sq_coords, dist2, Elt.denote_add (by den_pos) (by den_pos), Elt.denote_mul hT,
    Elt.denote_mul hT,
    Elt.denote_sub hq1 hp1, Elt.denote_sub hq2 hp2]
  simp only [toP2_zero, toP2_one]
  ring

include hr in
theorem denote_cross : Elt.denote T (cross T p q r) = cross2 (toP2 T p) (toP2 T q) (toP2 T r) := by
  obtain ⟨hp1, hp2⟩ := hp
  obtain ⟨hq1, hq2⟩ := hq
  obtain ⟨hr1, hr2⟩ := hr
  rw [cross, Elt.denote_sub (by den_pos) (by den_pos), Elt.denote_mul hT, Elt.denote_mul hT,
    Elt.denote_sub hq1 hp1, Elt.denote_sub hr2 hp2, Elt.denote_sub hq2 hp2, Elt.denote_sub hr1 hp1]
  simp only [cross2, toP2_zero, toP2_one]

include hr in
theorem denote_dot : Elt.denote T (dot T p q r) = dot2 (toP2 T p) (toP2 T q) (toP2 T r) := by
  obtain ⟨hp1, hp2⟩ := hp
  obtain ⟨hq1, hq2⟩ := hq
  obtain ⟨hr1, hr2⟩ := hr
  rw [dot, Elt.denote_add (by den_pos) (by den_pos), Elt.denote_mul hT, Elt.denote_mul hT,
    Elt.denote_sub hq1 hp1, Elt.denote_sub hr1 hp1, Elt.denote_sub hq2 hp2, Elt.denote_sub hr2 hp2]
  simp only [dot2, toP2_zero, toP2_one]

end

theorem denote_crossDir {a b c d : Pt} (hT : Tree.WF T) (ha : a.Ok) (hb : b.Ok) (hc : c.Ok)
    (hd : d.Ok) :
    Elt.denote T (crossDir T a b c d) = crossDir2 (toP2 T a) (toP2 T b) (toP2 T c) (toP2 T d) := by
  obtain ⟨ha1, ha2⟩ := ha
  obtain ⟨hb1, hb2⟩ := hb
  obtain ⟨hc1, hc2⟩ := hc
  obtain ⟨hd1, hd2⟩ := hd
  rw [crossDir, Elt.denote_sub (by den_pos) (by den_pos), Elt.denote_mul hT, Elt.denote_mul hT,
    Elt.denote_sub hb1 ha1, Elt.denote_sub hd2 hc2, Elt.denote_sub hb2 ha2, Elt.denote_sub hd1 hc1]
  simp only [crossDir2, toP2_zero, toP2_one]

theorem denote_dotDir {a b c d : Pt} (hT : Tree.WF T) (ha : a.Ok) (hb : b.Ok) (hc : c.Ok)
    (hd : d.Ok) :
    Elt.denote T (dotDir T a b c d) = dotDir2 (toP2 T a) (toP2 T b) (toP2 T c) (toP2 T d) := by
  obtain ⟨ha1, ha2⟩ := ha
  obtain ⟨hb1, hb2⟩ := hb
  obtain ⟨hc1, hc2⟩ := hc
  obtain ⟨hd1, hd2⟩ := hd
  rw [dotDir, Elt.denote_add (by den_pos) (by den_pos), Elt.denote_mul hT, Elt.denote_mul hT,
    Elt.denote_sub hb1 ha1, Elt.denote_sub hd1 hc1, Elt.denote_sub hb2 ha2, Elt.denote_sub hd2 hc2]
  simp only [dotDir2, toP2_zero, toP2_one]

end Pt

/-! ### The bridge lemmas used by the generated proofs -/

section Bridge

open Pt

variable {T : List Tree} (hT : Tree.WF T)
include hT

omit hT in
theorem eq_of_check {p q : Pt} (h : (p.denOk && q.denOk && same T p q) = true) :
    toP2 T p = toP2 T q := by
  simp only [Bool.and_eq_true, same] at h
  obtain ⟨⟨hp, hq⟩, hx, hy⟩ := h
  obtain ⟨hp1, hp2⟩ := ok_of_denOk hp
  obtain ⟨hq1, hq2⟩ := ok_of_denOk hq
  have ex := Elt.denote_of_isZero (T := T) hx
  have ey := Elt.denote_of_isZero (T := T) hy
  rw [Elt.denote_sub hp1 hq1] at ex
  rw [Elt.denote_sub hp2 hq2] at ey
  exact ext_coords (by simp; linarith) (by simp; linarith)

theorem ne_of_check {p q : Pt} (h : (p.denOk && q.denOk && differ T p q) = true) :
    toP2 T p ≠ toP2 T q := by
  simp only [Bool.and_eq_true, differ, Bool.or_eq_true, bne_iff_ne, ne_eq] at h
  obtain ⟨⟨hp, hq⟩, hxy⟩ := h
  obtain ⟨hp1, hp2⟩ := ok_of_denOk hp
  obtain ⟨hq1, hq2⟩ := ok_of_denOk hq
  intro he
  rcases hxy with hx | hy
  · have := (Elt.sign_spec hT (x := Elt.sub T p.x q.x) (Nat.mul_pos hp1 hq1)).ne_zero hx
    rw [Elt.denote_sub hp1 hq1] at this
    exact this (sub_eq_zero.mpr (by simpa using congrArg (· 0) he))
  · have := (Elt.sign_spec hT (x := Elt.sub T p.y q.y) (Nat.mul_pos hp2 hq2)).ne_zero hy
    rw [Elt.denote_sub hp2 hq2] at this
    exact this (sub_eq_zero.mpr (by simpa using congrArg (· 1) he))

theorem mem_circle_of_check {o a x : Pt}
    (h : (o.denOk && a.denOk && x.denOk && onCircle T o a x) = true) :
    toP2 T x ∈ circleThrough (toP2 T o) (toP2 T a) := by
  simp only [Bool.and_eq_true, onCircle] at h
  obtain ⟨⟨⟨ho, ha⟩, hx⟩, hz⟩ := h
  have ho' := ok_of_denOk ho
  have ha' := ok_of_denOk ha
  have hx' := ok_of_denOk hx
  have e := Elt.denote_of_isZero (T := T) hz
  obtain ⟨_, _⟩ := ho'
  obtain ⟨_, _⟩ := ha'
  obtain ⟨_, _⟩ := hx'
  rw [Elt.denote_sub (by den_pos) (by den_pos), denote_dist2 hT ⟨‹_›, ‹_›⟩ ⟨‹_›, ‹_›⟩,
    denote_dist2 hT ⟨‹_›, ‹_›⟩ ⟨‹_›, ‹_›⟩] at e
  rw [Metric.mem_sphere, dist_comm (toP2 T o)]
  exact (sq_eq_sq₀ dist_nonneg dist_nonneg).mp (by linarith)

theorem mem_line_of_check {a b x : Pt}
    (h : (a.denOk && b.denOk && x.denOk && differ T a b && onLine T a b x) = true) :
    toP2 T x ∈ lineThrough (toP2 T a) (toP2 T b) := by
  simp only [Bool.and_eq_true, onLine] at h
  obtain ⟨⟨⟨⟨ha, hb⟩, hx⟩, hab⟩, hz⟩ := h
  have hne := ne_of_check hT (p := a) (q := b) (by simp [ha, hb, hab])
  refine mem_line_of_cross2 hne ?_
  rw [← denote_cross hT (ok_of_denOk ha) (ok_of_denOk hb) (ok_of_denOk hx)]
  exact Elt.denote_of_isZero hz

omit hT in
/-- A point of one line that is not on another shows that the lines differ. -/
theorem line_ne_of_witness {A B C D W : P2} (hw : W ∈ lineThrough A B) (hn : cross2 C D W ≠ 0) :
    lineThrough A B ≠ lineThrough C D :=
  fun he => hn (cross2_eq_zero_of_mem (he ▸ hw))

theorem cross_ne_of_check {c d w : Pt}
    (h : (c.denOk && d.denOk && w.denOk && Elt.sign T (cross T c d w) != 0) = true) :
    cross2 (toP2 T c) (toP2 T d) (toP2 T w) ≠ 0 := by
  simp only [Bool.and_eq_true, bne_iff_ne, ne_eq] at h
  obtain ⟨⟨⟨hc, hd⟩, hw⟩, hs⟩ := h
  have hc' := ok_of_denOk hc
  have hd' := ok_of_denOk hd
  have hw' := ok_of_denOk hw
  obtain ⟨_, _⟩ := hc'
  obtain ⟨_, _⟩ := hd'
  obtain ⟨_, _⟩ := hw'
  have hpos : 0 < (cross T c d w).den := by den_pos
  have := (Elt.sign_spec hT hpos).ne_zero hs
  rwa [denote_cross hT ⟨‹_›, ‹_›⟩ ⟨‹_›, ‹_›⟩ ⟨‹_›, ‹_›⟩] at this

/-- The line `a b` differs from the line `c d`: `a` is not on `c d`. -/
theorem line_ne_of_check {a b c d : Pt}
    (h : (c.denOk && d.denOk && a.denOk && Elt.sign T (cross T c d a) != 0) = true) :
    lineThrough (toP2 T a) (toP2 T b) ≠ lineThrough (toP2 T c) (toP2 T d) :=
  line_ne_of_witness (left_mem_affineSpan_pair ℝ _ _) (cross_ne_of_check hT h)

/-- The line `a b` differs from the line `c d`: `b` is not on `c d`. -/
theorem line_ne_of_check' {a b c d : Pt}
    (h : (c.denOk && d.denOk && b.denOk && Elt.sign T (cross T c d b) != 0) = true) :
    lineThrough (toP2 T a) (toP2 T b) ≠ lineThrough (toP2 T c) (toP2 T d) :=
  line_ne_of_witness (right_mem_affineSpan_pair ℝ _ _) (cross_ne_of_check hT h)

theorem isLeft_of_check {a b x : Pt}
    (h : (a.denOk && b.denOk && x.denOk && ccw T a b x) = true) :
    IsLeft (toP2 T a) (toP2 T b) (toP2 T x) := by
  simp only [Bool.and_eq_true, ccw, beq_iff_eq] at h
  obtain ⟨⟨⟨ha, hb⟩, hx⟩, hs⟩ := h
  obtain ⟨_, _⟩ := ok_of_denOk ha
  obtain ⟨_, _⟩ := ok_of_denOk hb
  obtain ⟨_, _⟩ := ok_of_denOk hx
  have hpos : 0 < (cross T a b x).den := by den_pos
  have := Elt.sign_spec hT hpos
  rw [hs, denote_cross hT ⟨‹_›, ‹_›⟩ ⟨‹_›, ‹_›⟩ ⟨‹_›, ‹_›⟩] at this
  exact this.pos

theorem not_isLeft_of_check {a b x : Pt}
    (h : (a.denOk && b.denOk && x.denOk && !ccw T a b x) = true) :
    ¬IsLeft (toP2 T a) (toP2 T b) (toP2 T x) := by
  simp only [Bool.and_eq_true, ccw, Bool.not_eq_true', beq_eq_false_iff_ne, ne_eq] at h
  obtain ⟨⟨⟨ha, hb⟩, hx⟩, hs⟩ := h
  obtain ⟨_, _⟩ := ok_of_denOk ha
  obtain ⟨_, _⟩ := ok_of_denOk hb
  obtain ⟨_, _⟩ := ok_of_denOk hx
  have hpos : 0 < (cross T a b x).den := by den_pos
  have := Elt.sign_spec hT hpos
  rw [denote_cross hT ⟨‹_›, ‹_›⟩ ⟨‹_›, ‹_›⟩ ⟨‹_›, ‹_›⟩] at this
  unfold IsLeft
  rcases this with ⟨h1, _⟩ | ⟨_, h2⟩ | ⟨_, h2⟩
  · exact absurd h1 hs
  · rw [h2]; exact lt_irrefl 0
  · exact not_lt.mpr h2.le

theorem lexLt_of_check {p q : Pt} (h : (p.denOk && q.denOk && lexLt T p q) = true) :
    LexLt (toP2 T p) (toP2 T q) := by
  simp only [Bool.and_eq_true, lexLt, Bool.or_eq_true, beq_iff_eq] at h
  obtain ⟨⟨hp, hq⟩, hl⟩ := h
  obtain ⟨hp1, hp2⟩ := ok_of_denOk hp
  obtain ⟨hq1, hq2⟩ := ok_of_denOk hq
  unfold LexLt
  rcases hl with hx | ⟨hz, hy⟩
  · have := Elt.sign_spec hT (x := Elt.sub T p.x q.x) (Nat.mul_pos hp1 hq1)
    rw [hx, Elt.denote_sub hp1 hq1] at this
    left; simpa using sub_neg.mp this.neg_of
  · right
    have e := Elt.denote_of_isZero (T := T) hz
    rw [Elt.denote_sub hp1 hq1] at e
    have := Elt.sign_spec hT (x := Elt.sub T p.y q.y) (Nat.mul_pos hp2 hq2)
    rw [hy, Elt.denote_sub hp2 hq2] at this
    exact ⟨by simp; linarith, by simpa using sub_neg.mp this.neg_of⟩

/-- The sign of `|ab|² − |cd|²`, read as a comparison of distances. -/
theorem distSign_spec {a b c d : Pt} (h : (a.denOk && b.denOk && c.denOk && d.denOk) = true) :
    SignSpec (distSign T a b c d)
      (dist (toP2 T a) (toP2 T b) ^ 2 - dist (toP2 T c) (toP2 T d) ^ 2) := by
  simp only [Bool.and_eq_true] at h
  obtain ⟨⟨⟨ha, hb⟩, hc⟩, hd⟩ := h
  obtain ⟨_, _⟩ := ok_of_denOk ha
  obtain ⟨_, _⟩ := ok_of_denOk hb
  obtain ⟨_, _⟩ := ok_of_denOk hc
  obtain ⟨_, _⟩ := ok_of_denOk hd
  have hpos : 0 < (Elt.sub T (dist2 T a b) (dist2 T c d)).den := by den_pos
  have := Elt.sign_spec hT hpos
  rwa [Elt.denote_sub (by den_pos) (by den_pos), denote_dist2 hT ⟨‹_›, ‹_›⟩ ⟨‹_›, ‹_›⟩,
    denote_dist2 hT ⟨‹_›, ‹_›⟩ ⟨‹_›, ‹_›⟩] at this

theorem dist_eq_of_check {a b c d : Pt}
    (h : (a.denOk && b.denOk && c.denOk && d.denOk && distSign T a b c d == 0) = true) :
    dist (toP2 T a) (toP2 T b) = dist (toP2 T c) (toP2 T d) := by
  simp only [Bool.and_eq_true, beq_iff_eq] at h
  have := distSign_spec hT (a := a) (b := b) (c := c) (d := d) (by simp [h.1])
  rw [h.2] at this
  exact (sq_eq_sq₀ dist_nonneg dist_nonneg).mp (by linarith [this.eq_zero])

theorem dist_ne_of_check {a b c d : Pt}
    (h : (a.denOk && b.denOk && c.denOk && d.denOk && distSign T a b c d != 0) = true) :
    dist (toP2 T a) (toP2 T b) ≠ dist (toP2 T c) (toP2 T d) := by
  simp only [Bool.and_eq_true, bne_iff_ne, ne_eq] at h
  have := (distSign_spec hT (a := a) (b := b) (c := c) (d := d) (by simp [h.1])).ne_zero h.2
  intro he; rw [he] at this; exact this (sub_self _)

theorem dist_lt_of_check {a b c d : Pt}
    (h : (a.denOk && b.denOk && c.denOk && d.denOk && distSign T a b c d == -1) = true) :
    dist (toP2 T a) (toP2 T b) < dist (toP2 T c) (toP2 T d) := by
  simp only [Bool.and_eq_true, beq_iff_eq] at h
  have := distSign_spec hT (a := a) (b := b) (c := c) (d := d) (by simp [h.1])
  rw [h.2] at this
  exact lt_of_pow_lt_pow_left₀ 2 dist_nonneg (by linarith [this.neg_of])

theorem dist_le_of_check {a b c d : Pt}
    (h : (a.denOk && b.denOk && c.denOk && d.denOk && distSign T a b c d != 1) = true) :
    dist (toP2 T a) (toP2 T b) ≤ dist (toP2 T c) (toP2 T d) := by
  simp only [Bool.and_eq_true, bne_iff_ne, ne_eq] at h
  have := distSign_spec hT (a := a) (b := b) (c := c) (d := d) (by simp [h.1])
  refine le_of_pow_le_pow_left₀ two_ne_zero dist_nonneg ?_
  rcases this with ⟨h1, _⟩ | ⟨_, h2⟩ | ⟨_, h2⟩
  · exact absurd h1 h.2
  · linarith
  · linarith

omit hT in
/-- A given point: its hint equals the rational coordinates. -/
theorem given_of_check {p : Pt} {a b : Int} {c d : Nat}
    (h : (p.denOk && same T p ⟨⟨.leaf a, c⟩, ⟨.leaf b, d⟩⟩) = true) (hc : 0 < c) (hd : 0 < d) :
    toP2 T p = !₂[(a : ℝ) / c, (b : ℝ) / d] := by
  have := eq_of_check (T := T) (p := p) (q := ⟨⟨.leaf a, c⟩, ⟨.leaf b, d⟩⟩)
    (by simpa [Pt.denOk, Elt.denOk, hc, hd] using h)
  rw [this]
  ext i
  fin_cases i <;> simp [toP2, Elt.denote]

end Bridge

/-! ### Sets of points: collinear and concyclic -/

/-- Points on one line through a member `A` of the set are collinear. -/
theorem collinear_of_mem_line {A B : P2} {s : Set P2} (hA : A ∈ s)
    (h : ∀ p ∈ s, p ∈ lineThrough A B) : Collinear ℝ s := by
  rw [collinear_iff_of_mem hA]
  refine ⟨B -ᵥ A, fun p hp => ?_⟩
  obtain ⟨r, rfl⟩ := mem_affineSpan_pair_iff_exists_lineMap_eq.mp (h p hp)
  exact ⟨r, by rw [AffineMap.lineMap_apply]⟩

/-- Points at one distance from a centre are concyclic (the plane is
2-dimensional, so they are coplanar). -/
theorem concyclic_of_dist {O : P2} {r : ℝ} {s : Set P2} (h : ∀ p ∈ s, dist p O = r) :
    EuclideanGeometry.Concyclic s :=
  ⟨⟨O, r, h⟩, coplanar_of_finrank_eq_two s (by simp)⟩

theorem inner_vsub_eq_dot2 (A B C : P2) : inner ℝ (A -ᵥ B) (C -ᵥ B) = dot2 B A C := by
  simp only [PiLp.inner_apply, Fin.sum_univ_two, vsub_eq_sub, PiLp.sub_apply, RCLike.inner_apply,
    conj_trivial, dot2]
  ring

section Bridge2

open Pt

variable {T : List Tree} (hT : Tree.WF T)
include hT

theorem perp_of_check {a b c d : Pt}
    (h : (a.denOk && b.denOk && c.denOk && d.denOk && Elt.isZero (dotDir T a b c d)) = true) :
    (lineThrough (toP2 T a) (toP2 T b)).direction ⟂
      (lineThrough (toP2 T c) (toP2 T d)).direction := by
  simp only [Bool.and_eq_true] at h
  obtain ⟨⟨⟨⟨ha, hb⟩, hc⟩, hd⟩, hz⟩ := h
  have e := Elt.denote_of_isZero (T := T) hz
  rw [denote_dotDir hT (ok_of_denOk ha) (ok_of_denOk hb) (ok_of_denOk hc) (ok_of_denOk hd)] at e
  rw [direction_affineSpan, direction_affineSpan, vectorSpan_pair, vectorSpan_pair,
    Submodule.isOrtho_span]
  intro u hu v hv
  rw [Set.mem_singleton_iff] at hu hv
  subst hu hv
  simp only [PiLp.inner_apply, Fin.sum_univ_two, vsub_eq_sub, PiLp.sub_apply, RCLike.inner_apply,
    conj_trivial]
  unfold dotDir2 at e
  linear_combination e

theorem parallel_of_check {a b c d : Pt}
    (h : (a.denOk && b.denOk && c.denOk && d.denOk && differ T a b && differ T c d &&
      Elt.isZero (crossDir T a b c d)) = true) :
    (lineThrough (toP2 T a) (toP2 T b)).Parallel (lineThrough (toP2 T c) (toP2 T d)) := by
  simp only [Bool.and_eq_true] at h
  obtain ⟨⟨⟨⟨⟨⟨ha, hb⟩, hc⟩, hd⟩, hab⟩, hcd⟩, hz⟩ := h
  have hne1 := ne_of_check hT (p := a) (q := b) (by simp [ha, hb, hab])
  have hne2 := ne_of_check hT (p := c) (q := d) (by simp [hc, hd, hcd])
  have e := Elt.denote_of_isZero (T := T) hz
  rw [denote_crossDir hT (ok_of_denOk ha) (ok_of_denOk hb) (ok_of_denOk hc) (ok_of_denOk hd)] at e
  unfold crossDir2 at e
  generalize toP2 T a = A at *
  generalize toP2 T b = B at *
  generalize toP2 T c = C at *
  generalize toP2 T d = D at *
  rw [AffineSubspace.affineSpan_pair_parallel_iff_vectorSpan_eq, vectorSpan_pair, vectorSpan_pair,
    Submodule.span_singleton_eq_span_singleton]
  -- u = A − B and v = C − D are non-zero with u × v = 0, so v = z·u
  have nonzero : ∀ {P Q : P2}, P ≠ Q → (P 0 - Q 0) ^ 2 + (P 1 - Q 1) ^ 2 ≠ 0 := by
    intro P Q hPQ h0
    apply hPQ
    exact ext_coords (by nlinarith [sq_nonneg (P 0 - Q 0), sq_nonneg (P 1 - Q 1)])
      (by nlinarith [sq_nonneg (P 0 - Q 0), sq_nonneg (P 1 - Q 1)])
  have hu := nonzero hne1
  have hv := nonzero hne2
  set N := (A 0 - B 0) ^ 2 + (A 1 - B 1) ^ 2 with hN
  set z := ((C 0 - D 0) * (A 0 - B 0) + (C 1 - D 1) * (A 1 - B 1)) / N with hz'
  have lagrange : ((C 0 - D 0) ^ 2 + (C 1 - D 1) ^ 2) * N =
      ((C 0 - D 0) * (A 0 - B 0) + (C 1 - D 1) * (A 1 - B 1)) ^ 2 +
        ((A 0 - B 0) * (C 1 - D 1) - (A 1 - B 1) * (C 0 - D 0)) ^ 2 := by
    rw [hN]; ring
  have hcross : (A 0 - B 0) * (C 1 - D 1) - (A 1 - B 1) * (C 0 - D 0) = 0 := by linear_combination e
  have hz0 : z ≠ 0 := by
    intro h0
    rw [hz', div_eq_zero_iff] at h0
    rcases h0 with h0 | h0
    · rw [h0, hcross] at lagrange
      exact hv ((mul_eq_zero.mp (by simpa using lagrange)).resolve_right hu)
    · exact hu h0
  refine ⟨Units.mk0 z hz0, ext_coords ?_ ?_⟩ <;>
    simp only [Units.smul_mk0, vsub_eq_sub, PiLp.smul_apply, PiLp.sub_apply, smul_eq_mul, hz']
  · rw [div_mul_eq_mul_div, div_eq_iff hu]; linear_combination (A 1 - B 1) * hcross
  · rw [div_mul_eq_mul_div, div_eq_iff hu]; linear_combination (-(A 0 - B 0)) * hcross

theorem sbtw_of_check {a b c : Pt}
    (h : (a.denOk && b.denOk && c.denOk && differ T a c && onLine T a c b &&
      Elt.sign T (dot T b a c) == -1) = true) :
    Sbtw ℝ (toP2 T a) (toP2 T b) (toP2 T c) := by
  simp only [Bool.and_eq_true, beq_iff_eq] at h
  obtain ⟨⟨⟨⟨⟨ha, hb⟩, hc⟩, hac⟩, hon⟩, hs⟩ := h
  have hmem := mem_line_of_check hT (a := a) (b := c) (x := b) (by simp [ha, hb, hc, hac, hon])
  have hne := ne_of_check hT (p := a) (q := c) (by simp [ha, hc, hac])
  have ha' := ok_of_denOk ha
  have hb' := ok_of_denOk hb
  have hc' := ok_of_denOk hc
  have hpos : 0 < (dot T b a c).den := by
    obtain ⟨_, _⟩ := ha'; obtain ⟨_, _⟩ := hb'; obtain ⟨_, _⟩ := hc'; den_pos
  have hdot := Elt.sign_spec hT hpos
  rw [hs, denote_dot hT hb' ha' hc'] at hdot
  have hneg := hdot.neg_of
  obtain ⟨r, hr⟩ := mem_affineSpan_pair_iff_exists_lineMap_eq.mp hmem
  have hB0 : toP2 T b 0 = (1 - r) * toP2 T a 0 + r * toP2 T c 0 := by
    rw [← hr]; simp [AffineMap.lineMap_apply_module]
  have hB1 : toP2 T b 1 = (1 - r) * toP2 T a 1 + r * toP2 T c 1 := by
    rw [← hr]; simp [AffineMap.lineMap_apply_module]
  have hN : 0 < (toP2 T c 0 - toP2 T a 0) ^ 2 + (toP2 T c 1 - toP2 T a 1) ^ 2 := by
    rcases (sq_nonneg (toP2 T c 0 - toP2 T a 0)).lt_or_eq with h0 | h0
    · positivity
    rcases (sq_nonneg (toP2 T c 1 - toP2 T a 1)).lt_or_eq with h1 | h1
    · positivity
    exact absurd (ext_coords (by nlinarith) (by nlinarith)) hne
  have key : dot2 (toP2 T b) (toP2 T a) (toP2 T c) =
      -(r * (1 - r)) * ((toP2 T c 0 - toP2 T a 0) ^ 2 + (toP2 T c 1 - toP2 T a 1) ^ 2) := by
    unfold dot2; rw [hB0, hB1]; ring
  rw [key] at hneg
  have hr01 : 0 < r * (1 - r) := by nlinarith
  have h0 : 0 < r := by
    by_contra h0; nlinarith [not_lt.mp h0]
  have h1 : r < 1 := by
    by_contra h1; nlinarith [not_lt.mp h1]
  rw [sbtw_iff_mem_image_Ioo_and_ne]
  exact ⟨⟨r, ⟨h0, h1⟩, hr⟩, hne⟩

theorem angle_eq_of_check {a b c d e f : Pt}
    (h : (a.denOk && b.denOk && c.denOk && d.denOk && e.denOk && f.denOk &&
      angleEq T a b c d e f) = true) :
    EuclideanGeometry.angle (toP2 T a) (toP2 T b) (toP2 T c) =
      EuclideanGeometry.angle (toP2 T d) (toP2 T e) (toP2 T f) := by
  simp only [angleEq, Bool.and_eq_true, beq_iff_eq] at h
  obtain ⟨⟨⟨⟨⟨⟨ha, hb⟩, hc⟩, hd⟩, he⟩, hf⟩, ⟨⟨⟨⟨⟨hba, hbc⟩, hed⟩, hef⟩, hsg⟩, hz⟩⟩ := h
  have ha' := ok_of_denOk ha
  have hb' := ok_of_denOk hb
  have hc' := ok_of_denOk hc
  have hd' := ok_of_denOk hd
  have he' := ok_of_denOk he
  have hf' := ok_of_denOk hf
  have nba := ne_of_check hT (p := b) (q := a) (by simp [ha, hb, hba])
  have nbc := ne_of_check hT (p := b) (q := c) (by simp [hb, hc, hbc])
  have ned := ne_of_check hT (p := e) (q := d) (by simp [he, hd, hed])
  have nef := ne_of_check hT (p := e) (q := f) (by simp [he, hf, hef])
  obtain ⟨_, _⟩ := ha'; obtain ⟨_, _⟩ := hb'; obtain ⟨_, _⟩ := hc'
  obtain ⟨_, _⟩ := hd'; obtain ⟨_, _⟩ := he'; obtain ⟨_, _⟩ := hf'
  -- the two dot products and their signs
  have s1 := Elt.sign_spec hT (x := dot T b a c) (by den_pos)
  have s2 := Elt.sign_spec hT (x := dot T e d f) (by den_pos)
  rw [denote_dot hT ⟨‹_›, ‹_›⟩ ⟨‹_›, ‹_›⟩ ⟨‹_›, ‹_›⟩] at s1 s2
  rw [hsg] at s1
  -- d₁²·n₂ = d₂²·n₁
  have eq := Elt.denote_of_isZero (T := T) hz
  rw [Elt.denote_sub (by den_pos) (by den_pos), Elt.denote_mul hT, Elt.denote_mul hT,
    Elt.denote_mul hT, Elt.denote_mul hT, Elt.denote_mul hT, Elt.denote_mul hT,
    denote_dot hT ⟨‹_›, ‹_›⟩ ⟨‹_›, ‹_›⟩ ⟨‹_›, ‹_›⟩, denote_dot hT ⟨‹_›, ‹_›⟩ ⟨‹_›, ‹_›⟩ ⟨‹_›, ‹_›⟩,
    denote_dist2 hT ⟨‹_›, ‹_›⟩ ⟨‹_›, ‹_›⟩, denote_dist2 hT ⟨‹_›, ‹_›⟩ ⟨‹_›, ‹_›⟩,
    denote_dist2 hT ⟨‹_›, ‹_›⟩ ⟨‹_›, ‹_›⟩, denote_dist2 hT ⟨‹_›, ‹_›⟩ ⟨‹_›, ‹_›⟩] at eq
  generalize toP2 T a = A at *
  generalize toP2 T b = B at *
  generalize toP2 T c = C at *
  generalize toP2 T d = D at *
  generalize toP2 T e = E at *
  generalize toP2 T f = F at *
  unfold EuclideanGeometry.angle InnerProductGeometry.angle
  rw [inner_vsub_eq_dot2, inner_vsub_eq_dot2, ← dist_eq_norm_vsub, ← dist_eq_norm_vsub,
    ← dist_eq_norm_vsub, ← dist_eq_norm_vsub, dist_comm A B, dist_comm C B, dist_comm D E,
    dist_comm F E]
  congr 1
  have k1 : 0 < dist B A * dist B C := mul_pos (dist_pos.mpr nba) (dist_pos.mpr nbc)
  have k2 : 0 < dist E D * dist E F := mul_pos (dist_pos.mpr ned) (dist_pos.mpr nef)
  set d₁ := dot2 B A C
  set d₂ := dot2 E D F
  set q₁ := dist B A * dist B C
  set q₂ := dist E D * dist E F
  have hsq : (d₁ * q₂) ^ 2 = (d₂ * q₁) ^ 2 := by
    have : (d₁ * q₂) ^ 2 = d₁ * d₁ * (dist E D ^ 2 * dist E F ^ 2) := by ring
    rw [this]
    have : (d₂ * q₁) ^ 2 = d₂ * d₂ * (dist B A ^ 2 * dist B C ^ 2) := by ring
    rw [this]
    linarith
  have heq : d₁ * q₂ = d₂ * q₁ := by
    rcases s1 with ⟨hs, h1⟩ | ⟨hs, h1⟩ | ⟨hs, h1⟩ <;> rw [hs] at s2
    · have h2 := s2.pos
      exact (sq_eq_sq₀ (by positivity) (by positivity)).mp hsq
    · have h2 := s2.eq_zero
      rw [h1, h2]; ring
    · have h2 := s2.neg_of
      have := (sq_eq_sq₀ (by nlinarith) (by nlinarith)).mp
        (show (-(d₁ * q₂)) ^ 2 = (-(d₂ * q₁)) ^ 2 by rw [neg_sq, neg_sq, hsq])
      linarith
  rw [div_eq_div_iff k1.ne' k2.ne']
  linarith

end Bridge2

end StraightedgeCompass
