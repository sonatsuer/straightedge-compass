title "Thales' theorem"
note "An angle inscribed in a semicircle is a right angle."

given A = (-1, 0)
given B = (1, 0)
O = midpoint(A, B)
u = circle O B
note "The circle $u$ on the diameter $A B$."

snapshot

P = point on u ~ (1/2, 1)
show polygon A P B
note "$P$ is any point of $u$ (other than $A$ and $B$)."
a = line A P
b = line B P
assert perpendicular a b
note "So $angle A P B$ is a right angle."
