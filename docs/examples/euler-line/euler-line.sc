title "The Euler line"
note "In every triangle the circumcentre $O$, the centroid $G$ and the orthocentre $H$ are collinear."

given A = (0, 0)
given B = (1, 0)
param p ~ 1/3
param q ~ 3/4
given C = (p, q)
show polygon A B C
assume ccw A B C
note "WLOG $A = (0, 0)$, $B = (1, 0)$, $C = (p, q)$ with $q > 0$."

snapshot

mc = perpBisector(A, B)
ma = perpBisector(B, C)
O = meet mc ma
show circle O A

snapshot

Ma = midpoint(B, C)
Mb = midpoint(C, A)
da = line A Ma
db = line B Mb
G = meet da db

snapshot

ha = perpFrom(B, C, A)
hb = perpFrom(C, A, B)
H = meet ha hb

snapshot

e = line O H
assert collinear O G H
note "$G$ lies on the line $O H$: the Euler line."
