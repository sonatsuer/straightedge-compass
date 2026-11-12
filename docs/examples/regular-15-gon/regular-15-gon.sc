title "The regular 15-gon (Euclid IV.16)"

note "Euclid inscribes an equilateral triangle and a regular pentagon with a common vertex: their sides cut off $1/3$ and $1/5$ of the circle, and $1/3 - 1/5 = 2/15$. Here the triangle's vertex at $120 degree$ and the pentagon's vertex at $144 degree$ are $24 degree = 2 pi / 15$ apart, so their chord is a side of the 15-gon."

-- The vertex after P and Q: the circle centred at Q through P meets the
-- circle u again.
def nextVertex(P, Q, u) -> R:
  k = circle Q P
  R = meet k u except P
end

given O = (0, 0)
given P0 = (1, 0)

x = line O P0
u = circle O P0
B = meet x u except P0
y = perpBisector(B, P0)
A = meet y u left O P0
note "The axes; $B = (-1, 0)$ and $A = (0, 1)$."

snapshot

ct = circle B O
T2, T1 = meet ct u
note "The circle about $B$ through $O$ meets $u$ at $120 degree$ and $240 degree$: $T_1$ and $T_2$ are vertices of the equilateral triangle on $P_0$."

M = midpoint(O, B)
c = circle M A
V, W = meet x c
cv = transfer(V, O, A)
Q3, Q2 = meet cv u
note "As for the pentagon, the Carlyle circle of $x^2 + x - 1 = 0$ gives $V = (2 cos(4 pi / 5), 0)$, and the circle about $V$ with radius $|O A|$ gives the pentagon's vertices $Q_2$ at $144 degree$ and $Q_3$ at $216 degree$."

snapshot

R0 = meet ct u left O P0
R1 = meet cv u left O P0
R2 = nextVertex(R0, R1, u)
R3 = nextVertex(R1, R2, u)
R4 = nextVertex(R2, R3, u)
R5 = nextVertex(R3, R4, u)
R6 = nextVertex(R4, R5, u)
R7 = nextVertex(R5, R6, u)
R8 = nextVertex(R6, R7, u)
R9 = nextVertex(R7, R8, u)
R10 = nextVertex(R8, R9, u)
R11 = nextVertex(R9, R10, u)
R12 = nextVertex(R10, R11, u)
R13 = nextVertex(R11, R12, u)
R14 = nextVertex(R12, R13, u)
R15 = nextVertex(R13, R14, u)
note "$R_0 = T_1$ and $R_1 = Q_2$; each further vertex comes from the circle about the last one through the one before."

show polygon R0 R1 R2 R3 R4 R5 R6 R7 R8 R9 R10 R11 R12 R13 R14

snapshot

assert R15 == R0
assert R10 == P0
note "Stepping the chord $R_0 R_1$ around closes up after 15 steps, and passes through $P_0$."

assert dist R1 R2 == dist R0 R1
assert dist R2 R3 == dist R0 R1
assert dist R3 R4 == dist R0 R1
assert dist R4 R5 == dist R0 R1
assert dist R5 R6 == dist R0 R1
assert dist R6 R7 == dist R0 R1
assert dist R7 R8 == dist R0 R1
assert dist R8 R9 == dist R0 R1
assert dist R9 R10 == dist R0 R1
assert dist R10 R11 == dist R0 R1
assert dist R11 R12 == dist R0 R1
assert dist R12 R13 == dist R0 R1
assert dist R13 R14 == dist R0 R1
assert dist R14 R0 == dist R0 R1

snapshot

assert ccw R0 R1 R2
assert ccw R0 R1 R3
assert ccw R0 R1 R4
assert ccw R0 R1 R5
assert ccw R0 R1 R6
assert ccw R0 R1 R7
assert ccw R0 R1 R8
assert ccw R0 R1 R9
assert ccw R0 R1 R10
assert ccw R0 R1 R11
assert ccw R0 R1 R12
assert ccw R0 R1 R13
assert ccw R0 R1 R14
note "All 15 sides are equal, and every other vertex lies to the left of $R_0 R_1$: the step angle is $2 pi / 15$, so the polygon is the convex regular 15-gon."
