title "Claims with variables"
-- every predicate

given A = (0, 0)
given B = (1, 0)
param p ~ 1/3
param q ~ 3/4
given C = (p, q)
assume ccw A B C
M = midpoint(A, B)
N = midpoint(A, M)
ab = line A B
assert on M ab
assert between A M B
assert collinear A M B
assert collinear A N M B
assert ccw A B C
assert C != M
assert dist A M == dist M B
assert dist A M != dist A B
assert dist A M < dist A B
assert dist A M <= dist M B
assert dist A B > dist A M
assert dist A B >= dist A M
Mb = midpoint(C, A)
Mc = midpoint(A, B)
mid = line Mb Mc
bc = line B C
assert parallel mid bc
h = perpFrom(B, C, A)
assert perpendicular h bc
m = perpBisector(A, B)
X = meet m ab
assert X == M
P = point on m ~ (1/2, 1)
assert angle P A B == angle P B A
ma = perpBisector(B, C)
O = meet m ma
k = circle O A
D = point on k ~ (1, 1)
assert concyclic A B C D
assert angle A M P == angle P M B
given E = (-1, 0)
given F = (2, 0)
assert angle E A P == angle F B P
