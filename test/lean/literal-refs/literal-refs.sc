title "Literal reference points"
-- selectors with a literal point instead of a named one

given A = (0, 0)
given B = (1, 0)
c1 = circle A B
c2 = circle B A
C = meet c1 c2 nearest (0, 1)
D = meet c1 c2 farthest (0, 1)
l = line A B
u = circle A C
E = meet l u except (1, 0)
assert dist A C == dist A B
assert C != D
assert dist A E == dist A B
