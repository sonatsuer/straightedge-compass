title "Cases with variables"
-- every form of cases, nested

given A = (0, 0)
given B = (1, 0)
param p ~ 1/3
param q ~ 3/4
given C = (p, q)

cases side C A B
left:
  assert ccw A B C
  cases compare dist A C, dist B C
  less:
    sample p ~ 1/4
    assert dist A C < dist B C
  equal:
    assert dist A C == dist B C
  greater:
    sample p ~ 3/4
    assert dist A C > dist B C
  end
on:
  assert collinear A B C
  cases C == A
  equal:
    assert C == A
  different:
    sample p ~ 2
    assert C != A
  end
right:
  sample q ~ -1
  assert ccw B A C
end

cases ccw A B C
yes:
  assert ccw A B C
no:
  sample q ~ -1/2
  assert dist A B == dist B A
end
