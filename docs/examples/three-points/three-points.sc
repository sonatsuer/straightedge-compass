title "Three points and a circle"
note "Three points lie on a circle exactly when they are not collinear. The perpendicular bisectors of $A B$ and $B C$ meet in the centre of that circle; when the points are collinear, the bisectors are parallel."

given A = (0, 0)
given B = (1, 0)
param p ~ 1/3
param q ~ 3/4
given C = (p, q)
show polygon A B C
note "Moving, rotating and scaling, $A = (0, 0)$, $B = (1, 0)$, and $C = (p, q)$ is any point."
mc = perpBisector(A, B)
ma = perpBisector(B, C)

snapshot

cases meet mc ma
one O:
  show circle O A
  assert dist O A == dist O B
  assert dist O A == dist O C
  note "The bisectors meet in $O$, which is equally far from $A$, $B$ and $C$: the centre of the circle through them."
none:
  sample q ~ 0
  assert collinear A B C
  note "The bisectors are parallel: $A$, $B$ and $C$ are collinear, and no circle passes through them."
same:
  sample p ~ 0, q ~ 0
  assert A == C
  note "The bisectors coincide when $C = A$."
end
