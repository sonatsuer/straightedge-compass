title "The regular hexagon (Euclid IV.15)"

note "The side of the regular hexagon equals the radius of its circle. Euclid draws, about both ends of a diameter, circles with the radius."

given O = (0, 0)
given P0 = (1, 0)

u = circle O P0
x = line O P0
P3 = meet x u except P0
note "$P_0 P_3$ is a diameter of $u$."

snapshot

c0 = circle P0 O
P5, P1 = meet c0 u
c3 = circle P3 O
P4, P2 = meet c3 u
note "The circles about $P_0$ and $P_3$ with the radius $|O P_0|$ meet $u$ in the remaining four vertices."

show polygon P0 P1 P2 P3 P4 P5

snapshot

assert dist P0 P1 == dist O P0
assert dist P1 P2 == dist O P0
assert dist P2 P3 == dist O P0
assert dist P3 P4 == dist O P0
assert dist P4 P5 == dist O P0
assert dist P5 P0 == dist O P0
note "Every side equals the radius."

snapshot

assert ccw P0 P1 P2
assert ccw P0 P1 P3
assert ccw P0 P1 P4
assert ccw P0 P1 P5
assert ccw P1 P2 P0
assert ccw P1 P2 P3
assert ccw P1 P2 P4
assert ccw P1 P2 P5
assert ccw P2 P3 P0
assert ccw P2 P3 P1
assert ccw P2 P3 P4
assert ccw P2 P3 P5
assert ccw P3 P4 P0
assert ccw P3 P4 P1
assert ccw P3 P4 P2
assert ccw P3 P4 P5
assert ccw P4 P5 P0
assert ccw P4 P5 P1
assert ccw P4 P5 P2
assert ccw P4 P5 P3
assert ccw P5 P0 P1
assert ccw P5 P0 P2
assert ccw P5 P0 P3
assert ccw P5 P0 P4
note "For every side, the other vertices lie to its left, so the hexagon is convex, and therefore regular."
