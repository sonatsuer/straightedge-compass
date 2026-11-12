title "A square on a given segment (Euclid I.46)"

note "On a given straight line $A B$, describe a square."

given A = (0, 0)
given B = (1, 0)

ab = line A B
p = perpAt(B, A, A)
c = circle A B
D = meet p c left A B
note "The perpendicular to $A B$ at $A$ (Euclid I.11), and $D$ on it with $|A D| = |A B|$."

snapshot

cb = circle B A
cd = circle D A
C = meet cb cd except A
bc = line B C
dc = line D C
ad = line A D
note "The circles about $B$ and $D$ with radius $|A B|$ meet again at $C$."

show polygon A B C D

snapshot

assert dist B C == dist A B
assert dist C D == dist A B
assert dist D A == dist A B
note "The four sides are equal."

assert perpendicular ab ad
assert parallel ab dc
assert parallel ad bc
assert angle D A B == angle A B C
assert angle A B C == angle B C D
assert angle B C D == angle C D A
note "$A D$ is perpendicular to $A B$, the opposite sides are parallel, and the four angles are equal: they are right angles."

assert dist A C == dist B D
note "The diagonals are equal, as in every rectangle."

snapshot

ac = line A C
bd = line B D
M = meet ac bd
assert collinear A M C
assert between A M C
assert between B M D
assert dist M A == dist M B
assert concyclic A B C D
note "The diagonals meet at $M$, which lies between $A$ and $C$ and between $B$ and $D$; it is equally far from all four vertices, so they lie on one circle about $M$."
