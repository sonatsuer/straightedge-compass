# Straightedge and Compass

This is a simple command line tool for performing straightedge and compass constructions. See
the [Wikipedia page](https://en.wikipedia.org/wiki/Straightedge_and_compass_construction) if
you do not know what that means. The heavy lifting is done by the [Data.Real.Constructible](http://hackage.haskell.org/package/constructible-0.1.0.1/docs/Data-Real-Constructible.html)
library which allows you to do exact arithmetic in the constructible field.

## Usage

Here is a sample session.

```
Welcome to Straightedge and Compass!
Use CTRL + D to exit.
> intersection C (0,0) 1 and C (0,1) 1
Distinct V2 (1/2*sqrt 3) (1/2) V2 (-1/2*sqrt 3) (1/2)
> intersection C (0,0) 1 and C (0,1) 1 as $p1 $p2
Now the name p1 is assigned to
V2 (1/2*sqrt 3) (1/2)
Now the name p2 is assigned to
V2 (-1/2*sqrt 3) (1/2)
> display $p1
V2 (1/2*sqrt 3) (1/2)
> discard $p2
Object named p2 is discarded.
> display $p2
!!! Object with name p2 does not exist
> line $p1 and (3/2, -5)
Unique NonVertical {_slope = -11/2 - 11/6*sqrt 3, _yIntercept = 13/4 + 11/4*sqrt 3}
> line $p1 and (3/2, -5) as $l1
Now the name l1 is assigned to
NonVertical {_slope = -11/2 - 11/6*sqrt 3, _yIntercept = 13/4 + 11/4*sqrt 3}
> ^D
Exiting...
```

Some notes:
* Variable names start with a `$`.
* A point is introduced by the user as `(x,y)` where `x` and `y` are rational numbers.
* A line is introduced by the user by a defining equation which is either `x = a` or `y = m * x + c` where `a`, `m` and `x` are rational.
* A circle with center `(x,y)` and radius `r` is introduced by the user as `C (x,y) r` where `x`, `y` and `r` are rational.
* The `line` constructor expects two points.
* The `intersection` constructor expects one of the following: two circles, two lines, a line and a circle, two circles.
* The `circleFrom` constructor (not used in the session) expects a center point and a radius.

## Possible Improvements

This was a fast side project and I do not know if I will have time to make develop it
any further. So I am not opening these as Github issues:

* **Graphics:** This is a geometric tool without any visual feedback. It would be nice to fix that.
  One can render the constructions in Gloss, for instance, or compile everything with GHCJS and
  put it on a webpage with canvas graphics.

* **Text output:** Currently the user sees the derived show instances of objects. It looks ugly.

* **Predicates:** For the moment we cannot really prove, for instance, that an equilateral triangle we construct *is* actually equilateral. Adding predicates like equidistance or point membership would allow us to formally verify the constructions.

* **Tests:** There is none at the moment.



