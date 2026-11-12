title "Tangents from a point"
note "From a point outside a circle there are two tangents, each perpendicular to its radius, and the two tangent segments are equal. From a point on the circle the only tangent touches at the point itself. From a point inside there is none."

given O = (0, 0)
given A = (1, 0)
u = circle O A
note "The unit circle $u$."
param p ~ 2
given P = (p, 0)
note "The point $P$ at distance $|p|$ from the centre. Rotating and scaling, every circle and point look like this."
k = circleDiameter(O, P)
note "By Thales, the points $T$ of the circle $k$ on the diameter $O P$ are those where $O T perp P T$: the points where a tangent from $P$ touches $u$."

snapshot

cases meet u k
two:
  T1 = meet u k left O P
  T2 = meet u k right O P
  show segment P T1
  show segment P T2
  show segment O T1
  show segment O T2
  t1 = line P T1
  r1 = line O T1
  assert perpendicular r1 t1
  assert dist P T1 == dist P T2
  note "$P$ is outside $u$: the tangents touch at $T_1$ and $T_2$, and $|P T_1| = |P T_2|$."
one T:
  sample p ~ 1
  assert T == P
  note "$P$ is on $u$: the only tangent touches at $P$ itself."
none:
  sample p ~ 1/2
  note "$P$ is inside $u$: there is no tangent."
end
