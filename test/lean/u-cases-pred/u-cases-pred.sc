title "Cases on predicates"
-- cases on a distance comparison (yes/no) and on an equation (yes: solved by substitution)

given A = (0, 0)
given B = (1, 0)
param p ~ 1/3
param q ~ 3/4
given C = (p, q)
assume ccw A B C

cases dist A C < dist A B
yes:
  assert dist A C < dist A B
no:
  sample p ~ 2, q ~ 1
  assert dist A C >= dist A B
end

given M1 = (1/2, 0)
given M2 = (1/2, 1)
m = line M1 M2
cases on C m
yes:
  sample p ~ 1/2
  assert dist C A == dist C B
no:
  assert C != A
end
