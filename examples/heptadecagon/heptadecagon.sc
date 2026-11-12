title "The regular 17-gon by Carlyle circles"

note "In 1796 Gauss showed that $cos(2 pi / 17)$ can be computed with square roots alone. Write $c_k = 2 cos(2 pi k / 17)$. His periods are $eta_0 = c_1 + c_2 + c_4 + c_8$ and $eta_1 = c_3 + c_5 + c_6 + c_7$, the roots of $x^2 + x - 4 = 0$; $mu_0 = c_1 + c_4$ and $mu_1 = c_2 + c_8$, the roots of $x^2 - eta_0 x - 1 = 0$; $nu_0 = c_3 + c_5$ and $nu_1 = c_6 + c_7$, the roots of $x^2 - eta_1 x - 1 = 0$; and $c_1$ and $c_4$, the roots of $x^2 - mu_0 x + nu_0 = 0$."
note "The Carlyle circle of $x^2 - s x + p = 0$ is the circle with diameter from $(0, 1)$ to $(s, p)$. It meets the $x$-axis at the roots."

-- The vertex after P and Q: the circle centred at Q through P meets the
-- circle u again.
def nextVertex(P, Q, u) -> R:
  k = circle Q P
  R = meet k u except P
end

given O = (0, 0)
given P0 = (1, 0)
note "The centre and the first vertex. There are two given points, so the construction is general: any other choice of them gives a similar figure."

x = line O P0
u = circle O P0
P0' = meet x u except P0
y = perpBisector(P0', P0)
A = meet y u left O P0
note "The axes, and $A = (0, 1)$, the common end of all Carlyle diameters."

snapshot

H = midpoint(O, P0')
K = midpoint(O, H)
note "$K = (-1/4, 0)$."

snapshot

c1 = circle K A
V, W = meet x c1
note "$c_1$ is the Carlyle circle of $u^2 + u/2 - 1 = 0$, whose roots are $eta/2$: its diameter runs from $(0, 1)$ to $(-1/2, -1)$, so its centre is $K$. Hence $V = (eta_1/2, 0)$ and $W = (eta_0/2, 0)$."

snapshot

c2 = circle W A
M1, M0 = meet x c2
note "For $p = -1$ the Carlyle centre $(s/2, 0)$ lies on the axis. So $c_2$ is the Carlyle circle of $x^2 - eta_0 x - 1 = 0$, and $M_1 = (mu_1, 0)$, $M_0 = (mu_0, 0)$."

c3 = circle V A
N1, N0 = meet x c3
note "Likewise $c_3$ is the Carlyle circle of $x^2 - eta_1 x - 1 = 0$: $N_1 = (nu_1, 0)$ and $N_0 = (nu_0, 0)$."

snapshot

cn = circle O N0
N0y = meet y cn left O P0
v = perpAt(O, P0, M0)
h = perpAt(O, A, N0y)
B = meet v h
note "$B = (mu_0, nu_0)$, the far end of the last Carlyle diameter."

snapshot

c4 = circleDiameter(A, B)
X4, X1 = meet x c4
note "$c_4$ is the Carlyle circle of $x^2 - mu_0 x + nu_0 = 0$. So $X_4 = (c_4, 0)$ and $X_1 = (c_1, 0) = (2 cos(2 pi / 17), 0)$."

snapshot

G = midpoint(O, X1)
g = perpAt(O, P0, G)
P1 = meet g u left O P0
note "$G = (cos(2 pi / 17), 0)$, so $P_1 = (cos(2 pi / 17), sin(2 pi / 17))$ is the second vertex."

snapshot

P2 = nextVertex(P0, P1, u)
P3 = nextVertex(P1, P2, u)
P4 = nextVertex(P2, P3, u)
P5 = nextVertex(P3, P4, u)
P6 = nextVertex(P4, P5, u)
P7 = nextVertex(P5, P6, u)
P8 = nextVertex(P6, P7, u)
P9 = nextVertex(P7, P8, u)

snapshot

P10 = nextVertex(P8, P9, u)
P11 = nextVertex(P9, P10, u)
P12 = nextVertex(P10, P11, u)
P13 = nextVertex(P11, P12, u)
P14 = nextVertex(P12, P13, u)
P15 = nextVertex(P13, P14, u)
P16 = nextVertex(P14, P15, u)
P17 = nextVertex(P15, P16, u)
note "Each further vertex comes from the circle centred at the last vertex through the one before."

show polygon P0 P1 P2 P3 P4 P5 P6 P7 P8 P9 P10 P11 P12 P13 P14 P15 P16

snapshot

assert P17 == P0
note "Stepping the chord $P_0 P_1$ around the circle closes up after 17 steps."

assert dist P1 P2 == dist P0 P1
assert dist P2 P3 == dist P0 P1
assert dist P3 P4 == dist P0 P1
assert dist P4 P5 == dist P0 P1
assert dist P5 P6 == dist P0 P1
assert dist P6 P7 == dist P0 P1
assert dist P7 P8 == dist P0 P1
assert dist P8 P9 == dist P0 P1
assert dist P9 P10 == dist P0 P1
assert dist P10 P11 == dist P0 P1
assert dist P11 P12 == dist P0 P1
assert dist P12 P13 == dist P0 P1
assert dist P13 P14 == dist P0 P1
assert dist P14 P15 == dist P0 P1
assert dist P15 P16 == dist P0 P1
assert dist P16 P0 == dist P0 P1
note "All 17 sides are equal. All vertices lie on the circle $u$ by construction."

snapshot

assert ccw P0 P1 P2
assert ccw P0 P1 P3
assert ccw P0 P1 P4
assert ccw P0 P1 P5
assert ccw P0 P1 P6
assert ccw P0 P1 P7
assert ccw P0 P1 P8
assert ccw P0 P1 P9

snapshot

assert ccw P0 P1 P10
assert ccw P0 P1 P11
assert ccw P0 P1 P12
assert ccw P0 P1 P13
assert ccw P0 P1 P14
assert ccw P0 P1 P15
assert ccw P0 P1 P16
note "Every other vertex lies to the left of the side $P_0 P_1$. So the step angle is $2 pi / 17$, not $2 pi k / 17$ with $k >= 2$ (a star polygon would have vertices on both sides of $P_0 P_1$): the polygon is the convex regular 17-gon."
