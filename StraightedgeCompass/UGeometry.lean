module

public import StraightedgeCompass.Geometry
public import StraightedgeCompass.Cert

/-!
# Geometry with variables

For universal statements, a point has polynomial coordinates `x / d` and
`y / d` in the variables (the parameters and the square roots of a chain,
see `StraightedgeCompass.Cert`). Each geometric relation of points is the
sign of a polynomial in which the denominators are cleared, up to positive
factors: `crossN a b x` is `cross2 A B X · a.d² · b.d · x.d`, `leftN` its
multiple with the sign of `cross2`, and so on. The lemmas below turn facts
about these polynomials into statements about the points, assuming that the
denominators do not vanish.

`along A B t` and `rotAbout O B t` place a free point on a line or a
circle with a parameter `t`.
-/

@[expose] public section

namespace StraightedgeCompass

/-- A point with coordinates `x / d`, `y / d`. -/
structure UPt where
  x : Poly
  y : Poly
  d : Poly
  deriving Repr

/-- The point at parameter `t` on the line `A B`: `A + t (B - A)`. -/
noncomputable def along (A B : P2) (t : ℝ) : P2 := AffineMap.lineMap A B t

/-- `B` turned about `O` by the angle `θ` with `tan (θ / 2) = t`: the point
at parameter `t` on the circle with centre `O` through `B` (`t = 0` is `B`
itself; every point but `2 O - B` has a parameter). -/
noncomputable def rotAbout (O B : P2) (t : ℝ) : P2 :=
  !₂[O 0 + ((1 - t ^ 2) * (B 0 - O 0) - 2 * t * (B 1 - O 1)) / (1 + t ^ 2),
    O 1 + (2 * t * (B 0 - O 0) + (1 - t ^ 2) * (B 1 - O 1)) / (1 + t ^ 2)]

theorem rotAbout_mem (O B : P2) (t : ℝ) : rotAbout O B t ∈ circleThrough O B := by
  simp only [Metric.mem_sphere]
  have h1 : (0 : ℝ) < 1 + t ^ 2 := by positivity
  rw [← sq_eq_sq₀ dist_nonneg dist_nonneg, dist_sq_coords, dist_sq_coords]
  simp only [rotAbout]
  change (O 0 + ((1 - t ^ 2) * (B 0 - O 0) - 2 * t * (B 1 - O 1)) / (1 + t ^ 2) - O 0) ^ 2 +
      (O 1 + (2 * t * (B 0 - O 0) + (1 - t ^ 2) * (B 1 - O 1)) / (1 + t ^ 2) - O 1) ^ 2 =
    (O 0 - B 0) ^ 2 + (O 1 - B 1) ^ 2
  field_simp
  ring

theorem along_mem (A B : P2) (t : ℝ) : along A B t ∈ lineThrough A B :=
  AffineMap.lineMap_mem_affineSpan_pair t A B

namespace UPt

open Poly

def dxN (a b : UPt) : Poly := sub (mul b.x a.d) (mul a.x b.d)
def dyN (a b : UPt) : Poly := sub (mul b.y a.d) (mul a.y b.d)
/-- `dist A B ² · (a.d b.d)²` -/
def dist2N (a b : UPt) : Poly := add (mul (dxN a b) (dxN a b)) (mul (dyN a b) (dyN a b))
/-- `cross2 A B X · a.d² b.d x.d` -/
def crossN (a b x : UPt) : Poly := sub (mul (dxN a b) (dyN a x)) (mul (dyN a b) (dxN a x))
/-- `dot2 A B C · a.d² b.d c.d` -/
def dotN (a b c : UPt) : Poly := add (mul (dxN a b) (dxN a c)) (mul (dyN a b) (dyN a c))
/-- `(dist X O ² - dist A O ²) · (o.d x.d a.d)²` -/
def circN (o a x : UPt) : Poly :=
  sub (mul (dist2N o x) (mul a.d a.d)) (mul (dist2N o a) (mul x.d x.d))
/-- `(dist A B ² - dist C D ²) · (a.d b.d c.d d.d)²` -/
def cmpN (a b c d : UPt) : Poly :=
  sub (mul (dist2N a b) (mul (mul c.d d.d) (mul c.d d.d)))
    (mul (dist2N c d) (mul (mul a.d b.d) (mul a.d b.d)))
/-- the sign of `cross2 A B X` -/
def leftN (a b x : UPt) : Poly := mul (crossN a b x) (mul b.d x.d)
/-- the sign of `Y 0 - X 0` -/
def lexXN (x y : UPt) : Poly := mul (dxN x y) (mul x.d y.d)
/-- the sign of `Y 1 - X 1` -/
def lexYN (x y : UPt) : Poly := mul (dyN x y) (mul x.d y.d)
/-- `crossDir2 A B C D · a.d b.d c.d d.d` -/
def crossDirN (a b c d : UPt) : Poly := sub (mul (dxN a b) (dyN c d)) (mul (dyN a b) (dxN c d))
/-- `dotDir2 A B C D · a.d b.d c.d d.d` -/
def dotDirN (a b c d : UPt) : Poly := add (mul (dxN a b) (dxN c d)) (mul (dyN a b) (dyN c d))
/-- the sign of `dot2 B A C` -/
def dotSignN (b a c : UPt) : Poly := mul (dotN b a c) (mul a.d c.d)
/-- zero when `∠ A B C` and `∠ D E F` have equal cosines up to sign -/
def angleN (a b c d e f : UPt) : Poly :=
  sub (mul (mul (dotN b a c) (dotN b a c)) (mul (dist2N e d) (dist2N e f)))
    (mul (mul (dotN e d f) (dotN e d f)) (mul (dist2N b a) (dist2N b c)))

/-- `A + t (B - A)` -/
def alongU (a b : UPt) (t : Poly) : UPt :=
  ⟨add (mul a.x b.d) (mul t (dxN a b)), add (mul a.y b.d) (mul t (dyN a b)), mul a.d b.d⟩

/-- `B` turned about `O`, see `rotAbout` -/
def rotU (o b : UPt) (t : Poly) : UPt :=
  let tt := mul t t
  let one := const 1
  let two := const 2
  let u := dxN o b
  let w := dyN o b
  ⟨add (mul (mul o.x b.d) (add one tt)) (sub (mul (sub one tt) u) (mul (mul two t) w)),
    add (mul (mul o.y b.d) (add one tt)) (add (mul (mul two t) u) (mul (sub one tt) w)),
    mul (mul o.d b.d) (add one tt)⟩

variable (vs : List ℝ)

noncomputable def toP2 (p : UPt) : P2 :=
  !₂[Poly.eval vs p.x / Poly.eval vs p.d, Poly.eval vs p.y / Poly.eval vs p.d]

@[simp] theorem toP2_zero (p : UPt) : toP2 vs p 0 = Poly.eval vs p.x / Poly.eval vs p.d := rfl
@[simp] theorem toP2_one (p : UPt) : toP2 vs p 1 = Poly.eval vs p.y / Poly.eval vs p.d := rfl

variable {vs}

section
variable {a b c d e f o x y : UPt}

theorem sub_x (ha : Poly.eval vs a.d ≠ 0) (hb : Poly.eval vs b.d ≠ 0) :
    toP2 vs b 0 - toP2 vs a 0 = Poly.eval vs (dxN a b) / (Poly.eval vs a.d * Poly.eval vs b.d) := by
  simp only [toP2_zero, dxN, eval_sub, eval_mul]; field_simp

theorem sub_y (ha : Poly.eval vs a.d ≠ 0) (hb : Poly.eval vs b.d ≠ 0) :
    toP2 vs b 1 - toP2 vs a 1 = Poly.eval vs (dyN a b) / (Poly.eval vs a.d * Poly.eval vs b.d) := by
  simp only [toP2_one, dyN, eval_sub, eval_mul]; field_simp

theorem dist_sq (ha : Poly.eval vs a.d ≠ 0) (hb : Poly.eval vs b.d ≠ 0) :
    dist (toP2 vs a) (toP2 vs b) ^ 2 =
      Poly.eval vs (dist2N a b) / (Poly.eval vs a.d * Poly.eval vs b.d) ^ 2 := by
  rw [dist_sq_coords, ← neg_sub (toP2 vs b 0), ← neg_sub (toP2 vs b 1), neg_sq, neg_sq,
    sub_x ha hb, sub_y ha hb]
  simp only [dist2N, eval_add, eval_mul]
  field_simp

theorem cross2_eq (ha : Poly.eval vs a.d ≠ 0) (hb : Poly.eval vs b.d ≠ 0)
    (hx : Poly.eval vs x.d ≠ 0) :
    cross2 (toP2 vs a) (toP2 vs b) (toP2 vs x) = Poly.eval vs (crossN a b x) /
      (Poly.eval vs a.d ^ 2 * Poly.eval vs b.d * Poly.eval vs x.d) := by
  unfold cross2
  rw [sub_x ha hb, sub_y ha hx, sub_y ha hb, sub_x ha hx]
  simp only [crossN, eval_sub, eval_mul]
  field_simp

theorem dot2_eq (ha : Poly.eval vs a.d ≠ 0) (hb : Poly.eval vs b.d ≠ 0)
    (hc : Poly.eval vs c.d ≠ 0) :
    dot2 (toP2 vs a) (toP2 vs b) (toP2 vs c) = Poly.eval vs (dotN a b c) /
      (Poly.eval vs a.d ^ 2 * Poly.eval vs b.d * Poly.eval vs c.d) := by
  unfold dot2
  rw [sub_x ha hb, sub_x ha hc, sub_y ha hb, sub_y ha hc]
  simp only [dotN, eval_add, eval_mul]
  field_simp

theorem crossDir2_eq (ha : Poly.eval vs a.d ≠ 0) (hb : Poly.eval vs b.d ≠ 0)
    (hc : Poly.eval vs c.d ≠ 0) (hd : Poly.eval vs d.d ≠ 0) :
    crossDir2 (toP2 vs a) (toP2 vs b) (toP2 vs c) (toP2 vs d) = Poly.eval vs (crossDirN a b c d) /
      (Poly.eval vs a.d * Poly.eval vs b.d * Poly.eval vs c.d * Poly.eval vs d.d) := by
  unfold crossDir2
  rw [sub_x ha hb, sub_y hc hd, sub_y ha hb, sub_x hc hd]
  simp only [crossDirN, eval_sub, eval_mul]
  field_simp

theorem dotDir2_eq (ha : Poly.eval vs a.d ≠ 0) (hb : Poly.eval vs b.d ≠ 0)
    (hc : Poly.eval vs c.d ≠ 0) (hd : Poly.eval vs d.d ≠ 0) :
    dotDir2 (toP2 vs a) (toP2 vs b) (toP2 vs c) (toP2 vs d) = Poly.eval vs (dotDirN a b c d) /
      (Poly.eval vs a.d * Poly.eval vs b.d * Poly.eval vs c.d * Poly.eval vs d.d) := by
  unfold dotDir2
  rw [sub_x ha hb, sub_x hc hd, sub_y ha hb, sub_y hc hd]
  simp only [dotDirN, eval_add, eval_mul]
  field_simp

/-! ### From facts to statements -/

theorem ne_of_dist2N (ha : Poly.eval vs a.d ≠ 0) (hb : Poly.eval vs b.d ≠ 0)
    (h : 0 < Poly.eval vs (dist2N a b)) : toP2 vs a ≠ toP2 vs b := by
  intro heq
  have h0 : dist (toP2 vs a) (toP2 vs b) ^ 2 = 0 := by rw [heq]; simp
  rw [dist_sq ha hb] at h0
  have : (Poly.eval vs a.d * Poly.eval vs b.d) ^ 2 ≠ 0 := by positivity
  rw [div_eq_zero_iff] at h0
  rcases h0 with h0 | h0
  · linarith
  · exact this h0

theorem eq_of_dN (ha : Poly.eval vs a.d ≠ 0) (hb : Poly.eval vs b.d ≠ 0)
    (hx : Poly.eval vs (dxN a b) = 0) (hy : Poly.eval vs (dyN a b) = 0) :
    toP2 vs a = toP2 vs b := by
  have h0 := sub_x ha hb
  have h1 := sub_y ha hb
  rw [hx, zero_div, sub_eq_zero] at h0
  rw [hy, zero_div, sub_eq_zero] at h1
  exact ext_coords h0.symm h1.symm

theorem mem_line_of_crossN (ha : Poly.eval vs a.d ≠ 0) (hb : Poly.eval vs b.d ≠ 0)
    (hx : Poly.eval vs x.d ≠ 0) (hab : 0 < Poly.eval vs (dist2N a b))
    (h : Poly.eval vs (crossN a b x) = 0) : toP2 vs x ∈ lineThrough (toP2 vs a) (toP2 vs b) := by
  apply mem_line_of_cross2 (ne_of_dist2N ha hb hab)
  rw [cross2_eq ha hb hx, h, zero_div]

theorem dist_eq_of_circN (ho : Poly.eval vs o.d ≠ 0) (ha : Poly.eval vs a.d ≠ 0)
    (hx : Poly.eval vs x.d ≠ 0) (h : Poly.eval vs (circN o a x) = 0) :
    dist (toP2 vs x) (toP2 vs o) = dist (toP2 vs a) (toP2 vs o) := by
  rw [← sq_eq_sq₀ dist_nonneg dist_nonneg, dist_comm (toP2 vs x), dist_comm (toP2 vs a),
    dist_sq ho hx, dist_sq ho ha]
  simp only [circN, eval_sub, eval_mul] at h
  field_simp
  linear_combination h

theorem mem_circle_of_circN (ho : Poly.eval vs o.d ≠ 0) (ha : Poly.eval vs a.d ≠ 0)
    (hx : Poly.eval vs x.d ≠ 0) (h : Poly.eval vs (circN o a x) = 0) :
    toP2 vs x ∈ circleThrough (toP2 vs o) (toP2 vs a) := by
  simp only [Metric.mem_sphere]
  rw [dist_eq_of_circN ho ha hx h, dist_comm]

/-- `dist A B ² - dist C D ²` has the sign of `cmpN a b c d`. -/
theorem dist_sq_sub (ha : Poly.eval vs a.d ≠ 0) (hb : Poly.eval vs b.d ≠ 0)
    (hc : Poly.eval vs c.d ≠ 0) (hd : Poly.eval vs d.d ≠ 0) :
    dist (toP2 vs a) (toP2 vs b) ^ 2 - dist (toP2 vs c) (toP2 vs d) ^ 2 =
      Poly.eval vs (cmpN a b c d) /
        (Poly.eval vs a.d * Poly.eval vs b.d * Poly.eval vs c.d * Poly.eval vs d.d) ^ 2 := by
  rw [dist_sq ha hb, dist_sq hc hd]
  simp only [cmpN, eval_sub, eval_mul]
  field_simp

theorem dist_eq_of_cmpN (ha : Poly.eval vs a.d ≠ 0) (hb : Poly.eval vs b.d ≠ 0)
    (hc : Poly.eval vs c.d ≠ 0) (hd : Poly.eval vs d.d ≠ 0) (h : Poly.eval vs (cmpN a b c d) = 0) :
    dist (toP2 vs a) (toP2 vs b) = dist (toP2 vs c) (toP2 vs d) := by
  have := dist_sq_sub ha hb hc hd
  rw [h, zero_div, sub_eq_zero] at this
  exact (sq_eq_sq₀ dist_nonneg dist_nonneg).mp this

theorem dist_ne_of_cmpN (ha : Poly.eval vs a.d ≠ 0) (hb : Poly.eval vs b.d ≠ 0)
    (hc : Poly.eval vs c.d ≠ 0) (hd : Poly.eval vs d.d ≠ 0) (h : Poly.eval vs (cmpN a b c d) ≠ 0) :
    dist (toP2 vs a) (toP2 vs b) ≠ dist (toP2 vs c) (toP2 vs d) := by
  intro heq
  have := dist_sq_sub ha hb hc hd
  rw [heq, sub_self, eq_comm, div_eq_zero_iff] at this
  rcases this with h0 | h0
  · exact h h0
  · exact absurd h0 (by positivity)

/-- `dist C D < dist A B` from `cmpN a b c d > 0`. -/
theorem dist_lt_of_cmpN (ha : Poly.eval vs a.d ≠ 0) (hb : Poly.eval vs b.d ≠ 0)
    (hc : Poly.eval vs c.d ≠ 0) (hd : Poly.eval vs d.d ≠ 0) (h : 0 < Poly.eval vs (cmpN a b c d)) :
    dist (toP2 vs c) (toP2 vs d) < dist (toP2 vs a) (toP2 vs b) := by
  have := dist_sq_sub ha hb hc hd
  have hpos : 0 < dist (toP2 vs a) (toP2 vs b) ^ 2 - dist (toP2 vs c) (toP2 vs d) ^ 2 := by
    rw [this]; positivity
  exact (sq_lt_sq₀ dist_nonneg dist_nonneg).mp (by linarith)

/-- `dist C D ≤ dist A B` from `cmpN a b c d ≥ 0`. -/
theorem dist_le_of_cmpN (ha : Poly.eval vs a.d ≠ 0) (hb : Poly.eval vs b.d ≠ 0)
    (hc : Poly.eval vs c.d ≠ 0) (hd : Poly.eval vs d.d ≠ 0) (h : 0 ≤ Poly.eval vs (cmpN a b c d)) :
    dist (toP2 vs c) (toP2 vs d) ≤ dist (toP2 vs a) (toP2 vs b) := by
  have := dist_sq_sub ha hb hc hd
  have hpos : 0 ≤ dist (toP2 vs a) (toP2 vs b) ^ 2 - dist (toP2 vs c) (toP2 vs d) ^ 2 := by
    rw [this]; positivity
  exact (sq_le_sq₀ dist_nonneg dist_nonneg).mp (by linarith)

/-- `cross2 A B X` has the sign of `leftN a b x`. -/
theorem cross2_eq_left (ha : Poly.eval vs a.d ≠ 0) (hb : Poly.eval vs b.d ≠ 0)
    (hx : Poly.eval vs x.d ≠ 0) :
    cross2 (toP2 vs a) (toP2 vs b) (toP2 vs x) = Poly.eval vs (leftN a b x) /
      (Poly.eval vs a.d * Poly.eval vs b.d * Poly.eval vs x.d) ^ 2 := by
  rw [cross2_eq ha hb hx]
  simp only [leftN, eval_mul]
  field_simp

theorem isLeft_of_leftN (ha : Poly.eval vs a.d ≠ 0) (hb : Poly.eval vs b.d ≠ 0)
    (hx : Poly.eval vs x.d ≠ 0) (h : 0 < Poly.eval vs (leftN a b x)) :
    IsLeft (toP2 vs a) (toP2 vs b) (toP2 vs x) := by
  unfold IsLeft; rw [cross2_eq_left ha hb hx]; positivity

theorem not_isLeft_of_leftN (ha : Poly.eval vs a.d ≠ 0) (hb : Poly.eval vs b.d ≠ 0)
    (hx : Poly.eval vs x.d ≠ 0) (h : 0 ≤ Poly.eval vs (Poly.neg (leftN a b x))) :
    ¬IsLeft (toP2 vs a) (toP2 vs b) (toP2 vs x) := by
  unfold IsLeft; rw [cross2_eq_left ha hb hx, Poly.eval_neg] at *
  rw [not_lt]
  apply div_nonpos_of_nonpos_of_nonneg (by linarith) (by positivity)

theorem lexLt_of_lexXN (hx : Poly.eval vs x.d ≠ 0) (hy : Poly.eval vs y.d ≠ 0)
    (h : 0 < Poly.eval vs (lexXN x y)) : LexLt (toP2 vs x) (toP2 vs y) := by
  left
  have := sub_x hx hy
  have hpos : 0 < toP2 vs y 0 - toP2 vs x 0 := by
    rw [this]
    simp only [lexXN, eval_mul] at h
    have : Poly.eval vs (dxN x y) / (Poly.eval vs x.d * Poly.eval vs y.d) =
        Poly.eval vs (dxN x y) * (Poly.eval vs x.d * Poly.eval vs y.d) /
          (Poly.eval vs x.d * Poly.eval vs y.d) ^ 2 := by field_simp
    rw [this]; positivity
  linarith

theorem lexLt_of_lexYN (hx : Poly.eval vs x.d ≠ 0) (hy : Poly.eval vs y.d ≠ 0)
    (h0 : Poly.eval vs (dxN x y) = 0) (h : 0 < Poly.eval vs (lexYN x y)) :
    LexLt (toP2 vs x) (toP2 vs y) := by
  right
  have hx0 := sub_x hx hy
  rw [h0, zero_div, sub_eq_zero] at hx0
  refine ⟨hx0.symm, ?_⟩
  have := sub_y hx hy
  have hpos : 0 < toP2 vs y 1 - toP2 vs x 1 := by
    rw [this]
    simp only [lexYN, eval_mul] at h
    have : Poly.eval vs (dyN x y) / (Poly.eval vs x.d * Poly.eval vs y.d) =
        Poly.eval vs (dyN x y) * (Poly.eval vs x.d * Poly.eval vs y.d) /
          (Poly.eval vs x.d * Poly.eval vs y.d) ^ 2 := by field_simp
    rw [this]; positivity
  linarith

/-- Two lines differ when a point of the second is off the first. -/
theorem line_ne_of_crossN (ha : Poly.eval vs a.d ≠ 0) (hb : Poly.eval vs b.d ≠ 0)
    (hc : Poly.eval vs c.d ≠ 0) (h : Poly.eval vs (crossN a b c) ≠ 0) :
    lineThrough (toP2 vs a) (toP2 vs b) ≠ lineThrough (toP2 vs c) (toP2 vs d) := by
  intro heq
  have hmem : toP2 vs c ∈ lineThrough (toP2 vs a) (toP2 vs b) := by
    rw [heq]; exact left_mem_affineSpan_pair ℝ _ _
  have := cross2_eq_zero_of_mem hmem
  rw [cross2_eq ha hb hc, div_eq_zero_iff] at this
  rcases this with h0 | h0
  · exact h h0
  · exact absurd h0 (by positivity)

theorem line_ne_of_crossN' (ha : Poly.eval vs a.d ≠ 0) (hb : Poly.eval vs b.d ≠ 0)
    (hd : Poly.eval vs d.d ≠ 0) (h : Poly.eval vs (crossN a b d) ≠ 0) :
    lineThrough (toP2 vs a) (toP2 vs b) ≠ lineThrough (toP2 vs c) (toP2 vs d) := by
  intro heq
  have hmem : toP2 vs d ∈ lineThrough (toP2 vs a) (toP2 vs b) := by
    rw [heq]; exact right_mem_affineSpan_pair ℝ _ _
  have := cross2_eq_zero_of_mem hmem
  rw [cross2_eq ha hb hd, div_eq_zero_iff] at this
  rcases this with h0 | h0
  · exact h h0
  · exact absurd h0 (by positivity)

theorem perp_of_dotDirN (ha : Poly.eval vs a.d ≠ 0) (hb : Poly.eval vs b.d ≠ 0)
    (hc : Poly.eval vs c.d ≠ 0) (hd : Poly.eval vs d.d ≠ 0)
    (h : Poly.eval vs (dotDirN a b c d) = 0) :
    (lineThrough (toP2 vs a) (toP2 vs b)).direction ⟂
      (lineThrough (toP2 vs c) (toP2 vs d)).direction := by
  have e := dotDir2_eq ha hb hc hd
  rw [h, zero_div] at e
  generalize toP2 vs a = A at *
  generalize toP2 vs b = B at *
  generalize toP2 vs c = C at *
  generalize toP2 vs d = D at *
  rw [direction_affineSpan, direction_affineSpan, vectorSpan_pair, vectorSpan_pair,
    Submodule.isOrtho_span]
  intro u hu v hv
  rw [Set.mem_singleton_iff] at hu hv
  subst hu hv
  simp only [PiLp.inner_apply, Fin.sum_univ_two, vsub_eq_sub, PiLp.sub_apply, RCLike.inner_apply,
    conj_trivial]
  unfold dotDir2 at e
  linear_combination e

theorem parallel_of_crossDirN (ha : Poly.eval vs a.d ≠ 0) (hb : Poly.eval vs b.d ≠ 0)
    (hc : Poly.eval vs c.d ≠ 0) (hd : Poly.eval vs d.d ≠ 0)
    (hab : 0 < Poly.eval vs (dist2N a b)) (hcd : 0 < Poly.eval vs (dist2N c d))
    (h : Poly.eval vs (crossDirN a b c d) = 0) :
    (lineThrough (toP2 vs a) (toP2 vs b)).Parallel (lineThrough (toP2 vs c) (toP2 vs d)) := by
  have hne1 := ne_of_dist2N ha hb hab
  have hne2 := ne_of_dist2N hc hd hcd
  have e := crossDir2_eq ha hb hc hd
  rw [h, zero_div] at e
  unfold crossDir2 at e
  generalize toP2 vs a = A at *
  generalize toP2 vs b = B at *
  generalize toP2 vs c = C at *
  generalize toP2 vs d = D at *
  rw [AffineSubspace.affineSpan_pair_parallel_iff_vectorSpan_eq, vectorSpan_pair, vectorSpan_pair,
    Submodule.span_singleton_eq_span_singleton]
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
  have hcross : (A 0 - B 0) * (C 1 - D 1) - (A 1 - B 1) * (C 0 - D 0) = 0 := by
    linear_combination e
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

/-- `dot2 B A C` has the sign of `dotSignN b a c`. -/
theorem dot2_eq_sign (hb : Poly.eval vs b.d ≠ 0) (ha : Poly.eval vs a.d ≠ 0)
    (hc : Poly.eval vs c.d ≠ 0) :
    dot2 (toP2 vs b) (toP2 vs a) (toP2 vs c) = Poly.eval vs (dotSignN b a c) /
      (Poly.eval vs b.d * Poly.eval vs a.d * Poly.eval vs c.d) ^ 2 := by
  rw [dot2_eq hb ha hc]
  simp only [dotSignN, eval_mul]
  field_simp

theorem sbtw_of_N (ha : Poly.eval vs a.d ≠ 0) (hb : Poly.eval vs b.d ≠ 0)
    (hc : Poly.eval vs c.d ≠ 0) (hac : 0 < Poly.eval vs (dist2N a c))
    (hon : Poly.eval vs (crossN a c b) = 0) (hneg : 0 < Poly.eval vs (Poly.neg (dotSignN b a c))) :
    Sbtw ℝ (toP2 vs a) (toP2 vs b) (toP2 vs c) := by
  have hmem := mem_line_of_crossN ha hc hb hac hon
  have hne := ne_of_dist2N ha hc hac
  have hdot : dot2 (toP2 vs b) (toP2 vs a) (toP2 vs c) < 0 := by
    rw [dot2_eq_sign hb ha hc]
    rw [Poly.eval_neg] at hneg
    apply div_neg_of_neg_of_pos (by linarith) (by positivity)
  generalize toP2 vs a = A at *
  generalize toP2 vs b = B at *
  generalize toP2 vs c = C at *
  obtain ⟨r, hr⟩ := mem_affineSpan_pair_iff_exists_lineMap_eq.mp hmem
  have hB0 : B 0 = (1 - r) * A 0 + r * C 0 := by
    rw [← hr]; simp [AffineMap.lineMap_apply_module]
  have hB1 : B 1 = (1 - r) * A 1 + r * C 1 := by
    rw [← hr]; simp [AffineMap.lineMap_apply_module]
  have hN : 0 < (C 0 - A 0) ^ 2 + (C 1 - A 1) ^ 2 := by
    rcases (sq_nonneg (C 0 - A 0)).lt_or_eq with h0 | h0
    · positivity
    rcases (sq_nonneg (C 1 - A 1)).lt_or_eq with h1 | h1
    · positivity
    exact absurd (ext_coords (by nlinarith) (by nlinarith)) hne
  have key : dot2 B A C = -(r * (1 - r)) * ((C 0 - A 0) ^ 2 + (C 1 - A 1) ^ 2) := by
    unfold dot2; rw [hB0, hB1]; ring
  rw [key] at hdot
  have hr01 : 0 < r * (1 - r) := by nlinarith
  have h0 : 0 < r := by
    by_contra h0; nlinarith [not_lt.mp h0]
  have h1 : r < 1 := by
    by_contra h1; nlinarith [not_lt.mp h1]
  rw [sbtw_iff_mem_image_Ioo_and_ne]
  exact ⟨⟨r, ⟨h0, h1⟩, hr⟩, hne⟩

/-- Two numbers of the same sign (both zero included). -/
def SameSign (x y : ℝ) : Prop := (0 < x ∧ 0 < y) ∨ (x = 0 ∧ y = 0) ∨ (x < 0 ∧ y < 0)

theorem angle_eq_of_N (ha : Poly.eval vs a.d ≠ 0) (hb : Poly.eval vs b.d ≠ 0)
    (hc : Poly.eval vs c.d ≠ 0) (hd : Poly.eval vs d.d ≠ 0) (he : Poly.eval vs e.d ≠ 0)
    (hf : Poly.eval vs f.d ≠ 0)
    (hba : 0 < Poly.eval vs (dist2N b a)) (hbc : 0 < Poly.eval vs (dist2N b c))
    (hed : 0 < Poly.eval vs (dist2N e d)) (hef : 0 < Poly.eval vs (dist2N e f))
    (hsign : SameSign (Poly.eval vs (dotSignN b a c)) (Poly.eval vs (dotSignN e d f)))
    (hz : Poly.eval vs (angleN a b c d e f) = 0) :
    EuclideanGeometry.angle (toP2 vs a) (toP2 vs b) (toP2 vs c) =
      EuclideanGeometry.angle (toP2 vs d) (toP2 vs e) (toP2 vs f) := by
  have nba := ne_of_dist2N hb ha hba
  have nbc := ne_of_dist2N hb hc hbc
  have ned := ne_of_dist2N he hd hed
  have nef := ne_of_dist2N he hf hef
  have d1 := dot2_eq_sign hb ha hc
  have d2 := dot2_eq_sign he hd hf
  have q1 := dist_sq hb ha
  have q2 := dist_sq hb hc
  have q3 := dist_sq he hd
  have q4 := dist_sq he hf
  -- d₁² n₂ = d₂² n₁ in terms of the polynomials
  have key : dot2 (toP2 vs b) (toP2 vs a) (toP2 vs c) ^ 2 *
      (dist (toP2 vs e) (toP2 vs d) ^ 2 * dist (toP2 vs e) (toP2 vs f) ^ 2) -
      dot2 (toP2 vs e) (toP2 vs d) (toP2 vs f) ^ 2 *
      (dist (toP2 vs b) (toP2 vs a) ^ 2 * dist (toP2 vs b) (toP2 vs c) ^ 2) =
      Poly.eval vs (angleN a b c d e f) / (Poly.eval vs b.d ^ 4 * Poly.eval vs a.d ^ 2 *
        Poly.eval vs c.d ^ 2 * Poly.eval vs e.d ^ 4 * Poly.eval vs d.d ^ 2 *
        Poly.eval vs f.d ^ 2) := by
    rw [dot2_eq hb ha hc, dot2_eq he hd hf, q1, q2, q3, q4]
    simp only [angleN, eval_sub, eval_mul]
    field_simp
  have eq : dot2 (toP2 vs b) (toP2 vs a) (toP2 vs c) ^ 2 *
      (dist (toP2 vs e) (toP2 vs d) ^ 2 * dist (toP2 vs e) (toP2 vs f) ^ 2) =
      dot2 (toP2 vs e) (toP2 vs d) (toP2 vs f) ^ 2 *
      (dist (toP2 vs b) (toP2 vs a) ^ 2 * dist (toP2 vs b) (toP2 vs c) ^ 2) := by
    rw [← sub_eq_zero, key, hz, zero_div]
  have s1 : SameSign (dot2 (toP2 vs b) (toP2 vs a) (toP2 vs c))
      (dot2 (toP2 vs e) (toP2 vs d) (toP2 vs f)) := by
    rw [d1, d2]
    have k1 : 0 < (Poly.eval vs b.d * Poly.eval vs a.d * Poly.eval vs c.d) ^ 2 := by positivity
    have k2 : 0 < (Poly.eval vs e.d * Poly.eval vs d.d * Poly.eval vs f.d) ^ 2 := by positivity
    rcases hsign with ⟨h1, h2⟩ | ⟨h1, h2⟩ | ⟨h1, h2⟩
    · exact Or.inl ⟨div_pos h1 k1, div_pos h2 k2⟩
    · exact Or.inr (Or.inl ⟨by rw [h1, zero_div], by rw [h2, zero_div]⟩)
    · exact Or.inr (Or.inr ⟨div_neg_of_neg_of_pos h1 k1, div_neg_of_neg_of_pos h2 k2⟩)
  -- the polynomial facts are used up
  clear key hz hsign d1 d2 q1 q2 q3 q4 hba hbc hed hef
  generalize toP2 vs a = A at *
  generalize toP2 vs b = B at *
  generalize toP2 vs c = C at *
  generalize toP2 vs d = D at *
  generalize toP2 vs e = E at *
  generalize toP2 vs f = F at *
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
    have : (d₁ * q₂) ^ 2 = d₁ ^ 2 * (dist E D ^ 2 * dist E F ^ 2) := by ring
    rw [this]
    have : (d₂ * q₁) ^ 2 = d₂ ^ 2 * (dist B A ^ 2 * dist B C ^ 2) := by ring
    rw [this, eq]
  have heq : d₁ * q₂ = d₂ * q₁ := by
    rcases s1 with ⟨h1, h2⟩ | ⟨h1, h2⟩ | ⟨h1, h2⟩
    · exact (sq_eq_sq₀ (by positivity) (by positivity)).mp hsq
    · rw [h1, h2]; ring
    · have := (sq_eq_sq₀ (by nlinarith) (by nlinarith)).mp
        (show (-(d₁ * q₂)) ^ 2 = (-(d₂ * q₁)) ^ 2 by rw [neg_sq, neg_sq, hsq])
      linarith
  rw [div_eq_div_iff k1.ne' k2.ne']
  linarith

/-! ### Free points -/

theorem toP2_alongU (ha : Poly.eval vs a.d ≠ 0) (hb : Poly.eval vs b.d ≠ 0) (t : Poly) :
    toP2 vs (alongU a b t) = along (toP2 vs a) (toP2 vs b) (Poly.eval vs t) := by
  apply ext_coords
  · simp only [along, AffineMap.lineMap_apply_module, PiLp.add_apply, PiLp.smul_apply,
      smul_eq_mul, toP2_zero, alongU, dxN, eval_add, eval_mul, eval_sub]
    field_simp; ring
  · simp only [along, AffineMap.lineMap_apply_module, PiLp.add_apply, PiLp.smul_apply,
      smul_eq_mul, toP2_one, alongU, dyN, eval_add, eval_mul, eval_sub]
    field_simp; ring

theorem toP2_rotU (ho : Poly.eval vs o.d ≠ 0) (hb : Poly.eval vs b.d ≠ 0) (t : Poly) :
    toP2 vs (rotU o b t) = rotAbout (toP2 vs o) (toP2 vs b) (Poly.eval vs t) := by
  have h1 : (1 : ℝ) + Poly.eval vs t ^ 2 ≠ 0 := by positivity
  apply ext_coords
  · change _ = toP2 vs o 0 + _ / _
    simp only [toP2_zero, toP2_one, rotU, dxN, dyN, eval_add, eval_mul, eval_sub, eval_const]
    field_simp; ring
  · change _ = toP2 vs o 1 + _ / _
    simp only [toP2_one, toP2_zero, rotU, dxN, dyN, eval_add, eval_mul, eval_sub, eval_const]
    field_simp; ring

/-- Lines differ when their directions are not parallel. -/
theorem line_ne_of_crossDirN (ha : Poly.eval vs a.d ≠ 0) (hb : Poly.eval vs b.d ≠ 0)
    (hc : Poly.eval vs c.d ≠ 0) (hd : Poly.eval vs d.d ≠ 0)
    (h : Poly.eval vs (crossDirN a b c d) ≠ 0) :
    lineThrough (toP2 vs a) (toP2 vs b) ≠ lineThrough (toP2 vs c) (toP2 vs d) := by
  intro heq
  apply h
  have e := crossDir2_eq ha hb hc hd
  have hc' : toP2 vs c ∈ lineThrough (toP2 vs a) (toP2 vs b) := by
    rw [heq]; exact left_mem_affineSpan_pair ℝ _ _
  have hd' : toP2 vs d ∈ lineThrough (toP2 vs a) (toP2 vs b) := by
    rw [heq]; exact right_mem_affineSpan_pair ℝ _ _
  have z1 := cross2_eq_zero_of_mem hc'
  have z2 := cross2_eq_zero_of_mem hd'
  have z : crossDir2 (toP2 vs a) (toP2 vs b) (toP2 vs c) (toP2 vs d) = 0 := by
    unfold crossDir2; unfold cross2 at z1 z2; linear_combination z2 - z1
  rw [z, eq_comm, div_eq_zero_iff] at e
  rcases e with e | e
  · exact e
  · exact absurd e (mul_ne_zero (mul_ne_zero (mul_ne_zero ha hb) hc) hd)

/-- Three points with a zero cross product are collinear. -/
theorem collinear_of_crossN (ha : Poly.eval vs a.d ≠ 0) (hb : Poly.eval vs b.d ≠ 0)
    (hc : Poly.eval vs c.d ≠ 0) (h : Poly.eval vs (crossN a b c) = 0) :
    Collinear ℝ {toP2 vs a, toP2 vs b, toP2 vs c} := by
  have e := cross2_eq ha hb hc
  rw [h, zero_div] at e
  generalize toP2 vs a = A at *
  generalize toP2 vs b = B at *
  generalize toP2 vs c = C at *
  by_cases hAB : A = B
  · subst hAB
    rw [Set.insert_eq_of_mem (Set.mem_insert _ _)]
    exact collinear_pair ℝ A C
  · apply collinear_of_mem_line (A := A) (B := B) (Set.mem_insert _ _)
    intro p hp
    simp only [Set.mem_insert_iff, Set.mem_singleton_iff] at hp
    rcases hp with rfl | rfl | rfl
    · exact left_mem_affineSpan_pair ℝ _ _
    · exact right_mem_affineSpan_pair ℝ _ _
    · exact mem_line_of_cross2 hAB e

theorem sameSign_pos {x y : ℝ} (hx : 0 < x) (hy : 0 < y) : SameSign x y := Or.inl ⟨hx, hy⟩

theorem sameSign_zero {x y : ℝ} (hx : x = 0) (hy : y = 0) : SameSign x y := Or.inr (Or.inl ⟨hx, hy⟩)

theorem sameSign_neg {p q : Poly} (hx : 0 < Poly.eval vs (Poly.neg p))
    (hy : 0 < Poly.eval vs (Poly.neg q)) : SameSign (Poly.eval vs p) (Poly.eval vs q) := by
  rw [Poly.eval_neg] at hx hy
  exact Or.inr (Or.inr ⟨by linarith, by linarith⟩)

end

/-! ### Points of the statement -/

variable {ps : List ℝ} {S : List (Poly × Poly)}

/-- A given point, with coordinates in the parameters: each coordinate is
`n / m` with `X · m = n · D` and `m · k = c · D` for a polynomial `k`, so
that `m ≠ 0` (a polynomial coordinate has `m = 1`). -/
theorem toP2_given {u : UPt} {x y nx mx ny my cx cy : ℝ} {kx ky : Poly}
    (hv : (Poly.varsBelow ps.length u.x && Poly.varsBelow ps.length u.y &&
      Poly.varsBelow ps.length u.d) = true)
    (hd : Fact.holds (extend ps S) ⟨u.d, .nonzero⟩)
    (hx : x = nx / mx) (hcx : cx ≠ 0) (hkx : mx * Poly.eval ps kx = cx * Poly.eval ps u.d)
    (hxx : Poly.eval ps u.x * mx = nx * Poly.eval ps u.d)
    (hy : y = ny / my) (hcy : cy ≠ 0) (hky : my * Poly.eval ps ky = cy * Poly.eval ps u.d)
    (hyy : Poly.eval ps u.y * my = ny * Poly.eval ps u.d) :
    toP2 (extend ps S) u = !₂[x, y] := by
  simp only [Bool.and_eq_true] at hv
  obtain ⟨⟨h1, h2⟩, h3⟩ := hv
  have hd' : Poly.eval ps u.d ≠ 0 := by
    have : Poly.eval (extend ps S) u.d ≠ 0 := hd
    rwa [eval_extend h3] at this
  have hmx : mx ≠ 0 := by
    intro h0; rw [h0, zero_mul] at hkx; exact mul_ne_zero hcx hd' hkx.symm
  have hmy : my ≠ 0 := by
    intro h0; rw [h0, zero_mul] at hky; exact mul_ne_zero hcy hd' hky.symm
  apply ext_coords
  · have : (!₂[x, y] : P2) 0 = x := rfl
    rw [this, toP2_zero, eval_extend h1, eval_extend h3, hx, div_eq_div_iff hd' hmx, hxx]
  · have : (!₂[x, y] : P2) 1 = y := rfl
    rw [this, toP2_one, eval_extend h2, eval_extend h3, hy, div_eq_div_iff hd' hmy, hyy]

/-- A free point on the line `A B`, at the parameter `t` (variable `i`). -/
theorem toP2_along_param {a b : UPt} {i : Nat} {t : ℝ}
    (hi : Poly.varsBelow ps.length (Poly.var i) = true) (ht : Poly.eval ps (Poly.var i) = t)
    (ha : Fact.holds (extend ps S) ⟨a.d, .nonzero⟩)
    (hb : Fact.holds (extend ps S) ⟨b.d, .nonzero⟩) :
    toP2 (extend ps S) (alongU a b (Poly.var i)) =
      AffineMap.lineMap (toP2 (extend ps S) a) (toP2 (extend ps S) b) t := by
  rw [toP2_alongU ha hb, eval_extend hi, ht]; rfl

/-- A free point on the circle with centre `O` through `B`, at the parameter
`t` (variable `i`). -/
theorem toP2_rot_param {o b : UPt} {i : Nat} {t : ℝ}
    (hi : Poly.varsBelow ps.length (Poly.var i) = true) (ht : Poly.eval ps (Poly.var i) = t)
    (ho : Fact.holds (extend ps S) ⟨o.d, .nonzero⟩)
    (hb : Fact.holds (extend ps S) ⟨b.d, .nonzero⟩) :
    toP2 (extend ps S) (rotU o b (Poly.var i)) =
      rotAbout (toP2 (extend ps S) o) (toP2 (extend ps S) b) t := by
  rw [toP2_rotU ho hb, eval_extend hi, ht]

end UPt

/-- Evaluates a literal polynomial and closes `Poly.eval [x, …] P = e` by
`ring`. -/
macro "poly_eval" : tactic =>
  `(tactic| ((simp only [Poly.eval, List.foldr_cons, List.foldr_nil, Mono.eval, List.headD_cons,
      List.tail_cons, List.headD_nil, List.tail_nil, UPt.x, UPt.y, UPt.d]) <;> (try push_cast) <;>
      ring))

/-! ### Facts by index -/

/-- A fact of a list whose polynomial equals `q` as a polynomial. -/
theorem AllHold.factAt {vs : List ℝ} {K : List Fact} (h : AllHold vs K) (i : Nat) (q : Poly)
    (r : Rel) (hc : (K[i]?.any fun f => Poly.beq f.p q && f.rel == r) = true) :
    Fact.holds vs ⟨q, r⟩ := by
  cases hk : K[i]? with
  | none => simp [hk] at hc
  | some f =>
    simp only [hk, Option.any_some, Bool.and_eq_true, beq_iff_eq] at hc
    have hf := h.get i hk
    obtain ⟨hpq, hrel⟩ := hc
    have he := Poly.eval_eq_of_beq hpq vs
    unfold Fact.holds at hf ⊢
    rw [← hrel, ← he]
    exact hf

end StraightedgeCompass
