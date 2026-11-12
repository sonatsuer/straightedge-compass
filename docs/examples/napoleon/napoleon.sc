title "Napoleon's theorem"
note "Erect equilateral triangles outwards on the sides of any triangle: their centres form an equilateral triangle."

given A = (0, 0)
given B = (1, 0)
param p ~ 1/3
param q ~ 3/4
given C = (p, q)
assume ccw A B C
note "WLOG $A = (0, 0)$, $B = (1, 0)$, and the triangle is counterclockwise ($q > 0$); a reflection gives the other orientation."

def apex(P, Q) -> X:
  c1 = circle P Q
  c2 = circle Q P
  X = meet c1 c2 right P Q
end

def centre(P, Q, R) -> N:
  m1 = perpBisector(P, Q)
  m2 = perpBisector(Q, R)
  N = meet m1 m2
end

show polygon A B C

snapshot

X = apex(B, C)
Y = apex(C, A)
Z = apex(A, B)
show polygon B X C
show polygon C Y A
show polygon A Z B
note "Outwards means to the right of each side, walking around the triangle counterclockwise."

snapshot

N1 = centre(B, C, X)
N2 = centre(C, A, Y)
N3 = centre(A, B, Z)
show polygon N1 N2 N3
assert dist N1 N2 == dist N2 N3
assert dist N2 N3 == dist N3 N1
note "The triangle $N_1 N_2 N_3$ is equilateral."
