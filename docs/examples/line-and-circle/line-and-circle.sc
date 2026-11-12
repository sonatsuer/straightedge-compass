title "A line and a circle"
note "A line cuts a circle in two points, touches it in one, or misses it. When it cuts the circle, the centre lies on the perpendicular bisector of the chord. When it touches, the radius to the point of contact is perpendicular to the line."

given O = (0, 0)
given E = (1, 0)
u = circle O E
note "The unit circle $u$."
param h ~ 1/2
given A = (0, h)
given B = (1, h)
l = line A B
note "The line $l$ at height $h$. Rotating and scaling, every line and circle look like this."

snapshot

cases meet l u
two T1, T2:
  show segment T1 T2
  m = perpBisector(T1, T2)
  assert on O m
  note "$l$ cuts $u$ in $T_1$ and $T_2$: the centre $O$ lies on the perpendicular bisector of the chord."
one T:
  sample h ~ 1
  r = line O T
  assert perpendicular r l
  note "$l$ touches $u$ at $T$: the radius $O T$ is perpendicular to $l$."
none:
  sample h ~ 3/2
  note "$l$ misses $u$."
end
