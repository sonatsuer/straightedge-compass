title "A rational substitution"
-- solving the equation for p gives p = −1 / (q − 1): C has rational coordinates
given A = (0, 0)
given B = (1, 0)
param p ~ 1/2
param q ~ 3
given C = (p, q)
given D = (q, 1)
bc = line B C
bd = line B D
assume perpendicular bc bd
assert dist A B == dist B A
