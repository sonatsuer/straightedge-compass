title "Euclid I.1: an equilateral triangle on a given segment"

note "On a given finite straight line $A B$, construct an equilateral triangle."

given A = (0, 0)
given B = (1, 0)

c1 = circle A B
note "The circle with centre $A$ through $B$: its points are at distance $|A B|$ from $A$."

c2 = circle B A
note "The circle with centre $B$ through $A$: its points are at distance $|B A|$ from $B$."

snapshot

C = meet c1 c2 left A B
note "$C$ lies on both circles. Of the two intersection points we take the one to the left of $A -> B$."

show polygon A B C

assert dist A C == dist A B
assert dist B C == dist A B
note "So $|C A| = |A B| = |B C|$, and $A B C$ is equilateral."
