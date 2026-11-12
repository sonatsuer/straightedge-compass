title "Assumptions with variables"
-- inequalities, and an equation solved by substitution

given A = (0, 0)
given B = (1, 0)
param p ~ 2/3
param q ~ 1/2
given C = (p, q)
assume ccw A B C
assume dist A C > dist B C
assume dist A C >= dist B C
assume dist A C < dist A B
assume dist B C <= dist A B
assume C != B
assume dist A C != dist A B
assert dist A C > dist B C
assert dist A C <= dist A B
given V1 = (2/3, 0)
given V2 = (2/3, 1)
v = line V1 V2
assume on C v
assert dist A C > dist B C
