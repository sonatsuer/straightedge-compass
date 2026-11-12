title "The nine-point circle"
note "The midpoints of the sides, the feet of the altitudes, and the midpoints between the orthocentre and the vertices lie on one circle."

given A = (0, 0)
given B = (1, 0)
param p ~ 1/3
param q ~ 3/4
given C = (p, q)
show polygon A B C
assume ccw A B C

snapshot

Ma = midpoint(B, C)
Mb = midpoint(C, A)
Mc = midpoint(A, B)

snapshot

ha = perpFrom(B, C, A)
hb = perpFrom(C, A, B)
hc = perpFrom(A, B, C)
bc = line B C
ca = line C A
ab = line A B
Ha = meet ha bc
Hb = meet hb ca
Hc = meet hc ab
H = meet ha hb

snapshot

Ea = midpoint(A, H)
Eb = midpoint(B, H)
Ec = midpoint(C, H)
show circle through Ma Mb Mc
assert concyclic Ma Mb Mc Ha Hb Hc Ea Eb Ec
