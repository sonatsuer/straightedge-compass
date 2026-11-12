module

public import Mathlib.Data.Int.Init

/-!
# Exact arithmetic in a tower of square roots

A *tower* is a list of radicands `[ρₙ, …, ρ₁]` (top level first). The field it
describes is `ℚ(√ρ₁)(√ρ₂)…(√ρₙ)`, where each radicand `ρₖ` is an element of the
field below it. An element is an integer `Tree` over a positive denominator:

* `leaf n` is the integer `n`, at any depth;
* `node a b`, at a level with radicand `ρ`, is `a + b·√ρ`, where `a` and `b`
  are trees one level down.

Radicands are integer trees (the exporter rescales the generators so), so sums
and products of trees are trees again and no division is ever needed.

Every function recurses structurally on the radicand list, so that the kernel
can evaluate it (`decide +kernel`). At depth `0` a tree is read as its leftmost
leaf (`value0`), which makes every operation total on all trees.

The soundness of these operations is proved in `StraightedgeCompass.Tower.Sound`.
-/

@[expose] public section

namespace StraightedgeCompass

/-- An integer tree over a tower of square roots. -/
inductive Tree where
  | leaf (n : Int)
  | node (a b : Tree)
  deriving Repr, Inhabited

namespace Tree

/-- The value of a tree at depth `0`: its leftmost leaf. -/
def value0 : Tree → Int
  | leaf n => n
  | node a _ => value0 a

/-- Structurally zero. -/
def isZero : Tree → Bool
  | leaf n => n == 0
  | node a b => isZero a && isZero b

/-- Multiplication by an integer. -/
def scale (c : Int) : Tree → Tree
  | leaf n => leaf (c * n)
  | node a b => node (scale c a) (scale c b)

def neg (t : Tree) : Tree := scale (-1) t

/-- Addition at the depth given by the radicand list. -/
def add : List Tree → Tree → Tree → Tree
  | [], a, b => leaf (value0 a + value0 b)
  | _ :: _, leaf m, leaf n => leaf (m + n)
  | _ :: rs, leaf m, node b₀ b₁ => node (add rs (leaf m) b₀) b₁
  | _ :: rs, node a₀ a₁, leaf n => node (add rs a₀ (leaf n)) a₁
  | _ :: rs, node a₀ a₁, node b₀ b₁ => node (add rs a₀ b₀) (add rs a₁ b₁)

def sub (rs : List Tree) (a b : Tree) : Tree := add rs a (neg b)

/-- Multiplication: `(a₀ + a₁√ρ)(b₀ + b₁√ρ) = a₀b₀ + ρ·a₁b₁ + (a₀b₁ + a₁b₀)√ρ`. -/
def mul : List Tree → Tree → Tree → Tree
  | [], a, b => leaf (value0 a * value0 b)
  | _ :: _, leaf m, b => scale m b
  | _ :: _, node a₀ a₁, leaf n => scale n (node a₀ a₁)
  | ρ :: rs, node a₀ a₁, node b₀ b₁ =>
      node (add rs (mul rs a₀ b₀) (mul rs ρ (mul rs a₁ b₁)))
        (add rs (mul rs a₀ b₁) (mul rs a₁ b₀))

/-- The sign (`-1`, `0` or `1`) of a tree. When `a` and `b` have opposite
signs, `a + b√ρ` has the sign of `a² − ρb²` if `a > 0`, and the opposite sign
if `a < 0`. -/
def sign : List Tree → Tree → Int
  | [], t => Int.sign (value0 t)
  | _ :: _, leaf n => Int.sign n
  | ρ :: rs, node a b =>
      let sa := sign rs a
      let sb := sign rs b
      if sb = 0 then sa
      else if sa = 0 then sb
      else if sa = sb then sa
      else
        let n := sub rs (mul rs a a) (mul rs ρ (mul rs b b))
        if sa = 1 then sign rs n else -sign rs n

end Tree

/-- A tower is well formed when every radicand is positive in the tower below
it (checked by `Tree.sign`). Positivity, not just non-negativity, is needed:
with `ρ = 0` the element `0 + 1·√ρ` would be reported as non-zero. -/
def towerOk : List Tree → Bool
  | [] => true
  | ρ :: rs => (Tree.sign rs ρ == 1) && towerOk rs

/-- An element of the tower field: `num / den` with `den > 0`. -/
structure Elt where
  num : Tree
  den : Nat
  deriving Repr, Inhabited

namespace Elt

variable (T : List Tree)

def ofInt (n : Int) : Elt := ⟨.leaf n, 1⟩

def add (x y : Elt) : Elt :=
  ⟨Tree.add T (Tree.scale y.den x.num) (Tree.scale x.den y.num), x.den * y.den⟩

def neg (x : Elt) : Elt := ⟨Tree.neg x.num, x.den⟩

def sub (x y : Elt) : Elt := add T x (neg y)

def mul (x y : Elt) : Elt := ⟨Tree.mul T x.num y.num, x.den * y.den⟩

/-- The sign; the denominator is positive. -/
def sign (x : Elt) : Int := Tree.sign T x.num

def isZero (x : Elt) : Bool := Tree.isZero x.num

def denOk (x : Elt) : Bool := decide (0 < x.den)

end Elt

/-- A point with coordinates in the tower field. -/
structure Pt where
  x : Elt
  y : Elt
  deriving Repr, Inhabited

namespace Pt

variable (T : List Tree)

def denOk (p : Pt) : Bool := p.x.denOk && p.y.denOk

/-- The squared distance. -/
def dist2 (p q : Pt) : Elt :=
  let dx := Elt.sub T q.x p.x
  let dy := Elt.sub T q.y p.y
  Elt.add T (Elt.mul T dx dx) (Elt.mul T dy dy)

/-- `(q − p) × (r − p)`. -/
def cross (p q r : Pt) : Elt :=
  Elt.sub T (Elt.mul T (Elt.sub T q.x p.x) (Elt.sub T r.y p.y))
    (Elt.mul T (Elt.sub T q.y p.y) (Elt.sub T r.x p.x))

def same (p q : Pt) : Bool :=
  Elt.isZero (Elt.sub T p.x q.x) && Elt.isZero (Elt.sub T p.y q.y)

def differ (p q : Pt) : Bool :=
  Elt.sign T (Elt.sub T p.x q.x) != 0 || Elt.sign T (Elt.sub T p.y q.y) != 0

/-- `x` lies on the line through `a` and `b`. -/
def onLine (a b x : Pt) : Bool := Elt.isZero (cross T a b x)

/-- `x` lies on the circle with centre `o` through `a`. -/
def onCircle (o a x : Pt) : Bool := Elt.isZero (Elt.sub T (dist2 T x o) (dist2 T a o))

/-- `a`, `b`, `c` turn counterclockwise. -/
def ccw (a b c : Pt) : Bool := Elt.sign T (cross T a b c) == 1

/-- `|ab| ⋈ |cd|` decided by the sign of `|ab|² − |cd|²`. -/
def distSign (a b c d : Pt) : Int := Elt.sign T (Elt.sub T (dist2 T a b) (dist2 T c d))

/-- `(q − p) · (r − p)`. -/
def dot (p q r : Pt) : Elt :=
  Elt.add T (Elt.mul T (Elt.sub T q.x p.x) (Elt.sub T r.x p.x))
    (Elt.mul T (Elt.sub T q.y p.y) (Elt.sub T r.y p.y))

/-- `(b − a) × (d − c)`: zero when the directions are parallel. -/
def crossDir (a b c d : Pt) : Elt :=
  Elt.sub T (Elt.mul T (Elt.sub T b.x a.x) (Elt.sub T d.y c.y))
    (Elt.mul T (Elt.sub T b.y a.y) (Elt.sub T d.x c.x))

/-- `(b − a) · (d − c)`: zero when the directions are perpendicular. -/
def dotDir (a b c d : Pt) : Elt :=
  Elt.add T (Elt.mul T (Elt.sub T b.x a.x) (Elt.sub T d.x c.x))
    (Elt.mul T (Elt.sub T b.y a.y) (Elt.sub T d.y c.y))

/-- `∠abc = ∠def`: the cosines `dot / (|ba|·|bc|)` agree, compared without
square roots: equal signs of the dot products and `d₁²·n₂ = d₂²·n₁`. -/
def angleEq (a b c d e f : Pt) : Bool :=
  let d₁ := dot T b a c
  let d₂ := dot T e d f
  let n₁ := Elt.mul T (dist2 T b a) (dist2 T b c)
  let n₂ := Elt.mul T (dist2 T e d) (dist2 T e f)
  differ T b a && differ T b c && differ T e d && differ T e f &&
    Elt.sign T d₁ == Elt.sign T d₂ &&
    Elt.isZero (Elt.sub T (Elt.mul T (Elt.mul T d₁ d₁) n₂) (Elt.mul T (Elt.mul T d₂ d₂) n₁))

/-- `p < q` lexicographically (x first, then y). -/
def lexLt (p q : Pt) : Bool :=
  Elt.sign T (Elt.sub T p.x q.x) == -1 ||
    (Elt.isZero (Elt.sub T p.x q.x) && Elt.sign T (Elt.sub T p.y q.y) == -1)

end Pt

end StraightedgeCompass
