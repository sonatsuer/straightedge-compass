title "The regular pentagon by a Carlyle circle"

note "Write $p_1 = 2 cos(2 pi / 5)$ and $p_2 = 2 cos(4 pi / 5)$. Since $p_1 + p_2 = -1$ and $p_1 p_2 = -1$, they are the roots of $x^2 + x - 1 = 0$. The Carlyle circle of this equation has its diameter from $(0, 1)$ to $(-1, -1)$, so its centre is $(-1/2, 0)$, and it meets the $x$-axis at the roots."

given O = (0, 0)
given P0 = (1, 0)
note "The centre and the first vertex."

x = line O P0
u = circle O P0
B = meet x u except P0
y = perpBisector(B, P0)
A = meet y u left O P0
note "The axes; $B = (-1, 0)$ and $A = (0, 1)$."

snapshot

M = midpoint(O, B)
c = circle M A
V, W = meet x c
note "$M = (-1/2, 0)$, and $c$ is the Carlyle circle of $x^2 + x - 1 = 0$: $W = (p_1, 0)$ lies inside $u$ and $V = (p_2, 0)$ outside it."

snapshot

cw = transfer(W, O, A)
P4, P1 = meet cw u
note "The circle about $W$ with radius $|O A| = 1$ meets $u$ where $x = p_1 / 2 = cos(2 pi / 5)$: at $P_1$ and $P_4$. (The construction `transfer` copies a length with a compass that collapses when lifted, as in Euclid I.2.)"

cv = transfer(V, O, A)
P3, P2 = meet cv u
note "Likewise about $V$: $x = p_2 / 2 = cos(4 pi / 5)$, at $P_2$ and $P_3$."

show polygon P0 P1 P2 P3 P4

snapshot

assert dist P1 P2 == dist P0 P1
assert dist P2 P3 == dist P0 P1
assert dist P3 P4 == dist P0 P1
assert dist P4 P0 == dist P0 P1
note "The five sides are equal."

assert ccw P0 P1 P2
assert ccw P0 P1 P3
assert ccw P0 P1 P4
assert ccw P1 P2 P0
assert ccw P1 P2 P3
assert ccw P1 P2 P4
assert ccw P2 P3 P0
assert ccw P2 P3 P1
assert ccw P2 P3 P4
assert ccw P3 P4 P0
assert ccw P3 P4 P1
assert ccw P3 P4 P2
assert ccw P4 P0 P1
assert ccw P4 P0 P2
assert ccw P4 P0 P3
note "For every side, the other vertices lie to its left: the pentagon $P_0 P_1 P_2 P_3 P_4$ is convex. A convex polygon inscribed in a circle with equal sides is regular."
