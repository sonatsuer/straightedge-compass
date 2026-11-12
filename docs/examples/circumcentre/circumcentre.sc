title "The circumcentre of a triangle"
note "The perpendicular bisectors of the sides of a triangle meet in one point, which is equally far from the three vertices."

given A = (0, 0)
given B = (1, 0)
param p ~ 1/3
param q ~ 3/4
given C = (p, q)
show polygon A B C
note "WLOG (by a similarity) $A = (0, 0)$ and $B = (1, 0)$; $C = (p, q)$ is arbitrary."

snapshot

mc = perpBisector(A, B)
ma = perpBisector(B, C)
O = meet mc ma
show circle O A
note "The bisectors of $A B$ and $B C$ meet, unless the triangle is degenerate."

snapshot

mb = perpBisector(C, A)
assert on O mb
assert dist O A == dist O B
assert dist O A == dist O C
