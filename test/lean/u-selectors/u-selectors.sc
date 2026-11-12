title "Selectors with variables"
-- every selector, an ordered and an unordered pair, and free points on a line and a circle

given O = (0, 0)
given A = (1, 0)
u = circle O A
param p ~ 1/2
given B = (p, 1)
l = line O B
X = meet l u nearest B
Y = meet l u farthest B
Z = meet l u except X
V = meet l u nearest (0, 2)
W = meet l u farthest (0, 2)
L = meet l u left O A
R = meet l u right O A
k = line A B
T = meet k u except (1, 0)
S = meet k u except A
given H1 = (-1, 1/2)
given H2 = (1, 1/2)
h = line H1 H2
T1, T2 = meet h u
P = point on l ~ (1, 2)
Q = point on u ~ (0, -1)
assert dist O X == dist O A
assert X != Y
assert Z == Y
assert V == L
assert W == R
assert T == S
assert collinear O B P
assert on Q u
assert dist O T1 == dist O T2
