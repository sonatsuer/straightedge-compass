title "Two circles of equal radius"
note "Two circles of radius 1 meet in two points when their centres are less than 2 apart, touch when the distance is 2, and miss each other when it is more. When the centres coincide, the circles are the same. The common chord is perpendicular to the line of centres, which bisects it."

given O = (0, 0)
given A = (1, 0)
u = circle O A
param d ~ 1
given C = (d, 0)
given D = (d, 1)
v = circle C D
note "Circles of radius 1 around $O$ and around $C$, whose distance is $|d|$."

snapshot

cases meet u v
two:
  T1 = meet u v left O C
  T2 = meet u v right O C
  show segment T1 T2
  oc = line O C
  t = line T1 T2
  assert perpendicular oc t
  M = midpoint(T1, T2)
  assert on M oc
  note "The common chord $T_1 T_2$ is perpendicular to the line of centres, and its midpoint lies on it."
one T:
  sample d ~ 2
  oc = line O C
  assert on T oc
  assert dist O T == dist C T
  note "The circles touch at the midpoint of the centres."
none:
  sample d ~ 3
  note "The circles miss each other."
same:
  sample d ~ 0
  assert O == C
  note "The centres coincide: the circles are the same."
end
