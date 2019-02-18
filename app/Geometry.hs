{-# LANGUAGE
    DeriveFunctor
#-}

module Geometry where

import qualified Data.Real.Constructible as C
import           Linear

data Result a
  = None
  | Unique a
  | Distinct a a
  | Infinite
  deriving (Functor, Show)

type Point = V2 C.Construct

data Line
  = Vertical { _xIntercept :: C.Construct }
    -- Lines of the form x = c
  | NonVertical { _slope :: C.Construct, _yIntercept :: C.Construct }
    -- Lines of the form y = mx + c
  deriving Show

data Circle = Circle
  { _center :: Point
  , _radiusSquared :: C.Construct
  } deriving Show

-- Constructions
lineThroughPoints :: Point -> Point -> Result Line
lineThroughPoints p1@(V2 x1 y1) p2@(V2 x2 y2)
  | p1 == p2
    = Infinite
  | x1 == x2
    = Unique $ Vertical x1
  | otherwise
    = let m = (y2 - y1) / (x2 - x1)
       in Unique $ NonVertical m (y1 - m * x1)

-- A circle of radius zero is allowed. May change this later.
circleFromCenterAndPoint :: Point -> Point -> Result Circle
circleFromCenterAndPoint p1 p2 = Unique $ Circle p1 (qd p1 p2)

intersectionOfLines :: Line -> Line -> Result Point
intersectionOfLines l1 l2 = case (l1, l2) of
  (Vertical c1, Vertical c2) ->
    if c1 == c2 then Infinite else None
  (Vertical c1, NonVertical m2 c2) ->
    Unique $ V2 c1 (m2 * c1 + c2)
  (NonVertical m1 c1, Vertical c2) ->
    Unique $ V2 c2 (m1 * c2 + c1)
  (NonVertical m1 c1, NonVertical m2 c2)
    | m1 == m2 && c1 == c2
      -> Infinite
    | m1 == m2
      -> None
    | otherwise
      -> let x = (c2 - c1) / (m1 - m2)
          in Unique $ V2 x (m1 * x + c1)

intersectionOfLineAndCircle :: Line -> Circle -> Result Point
intersectionOfLineAndCircle l (Circle (V2 a b) rsq) = case l of
  Vertical c1 ->
    let disc = 4 * (rsq - (c1 - a) ^ 2)
     in if disc < 0 then None
        else if disc == 0 then Unique (V2 c1 b)
                          else Distinct (V2 c1 (b + (1/2) * sqrt disc))
                                        (V2 c1 (b - (1/2) * sqrt disc))
  NonVertical m1 c1 ->
    let quadA = m1 ^ 2 + 1
        quadB = m1 * c1 - m1 * b - a
        quadC = b ^ 2 + a ^ 2 - rsq - 2 * c1 * b + c1 ^ 2
        disc = quadB ^ 2 - 4 * quadA * quadC
     in if disc < 0 then None
        else if disc == 0 then let x = -quadB / (2 * quadA)
                                in Unique (V2 x (m1 * x + c1))
                          else let x1 = (-quadB - sqrt disc) / (2 * quadA)
                                   x2 = (-quadB + sqrt disc) / (2 * quadA)
                                in Distinct (V2 x1 (m1 * x1 + c1))
                                            (V2 x2 (m1 * x2 + c1))

intersectionOfCircles :: Circle -> Circle -> Result Point
intersectionOfCircles (Circle c1@(V2 x1 y1) rsq1) (Circle c2@(V2 x2 y2) rsq2)
  | c1 == c2 &&  rsq1 == rsq2
    = Infinite
  | c1 == c2
    = None
  | otherwise
    = let distsq = qd c1 c2
          disc = 2 * (rsq1 + rsq2) / distsq - (rsq1 - rsq2)^2 / distsq^2 - 1
          p = (1/2) *^ (c1 + c2) + ((rsq1 - rsq2) / (2 * distsq)) *^ (c2 - c1)
          q = V2 (y2 - y1) (x1 - x2)
       in if disc < 0 then None
          else if disc == 0 then Unique p
                            else Distinct (p + (1/2) * sqrt disc *^ q)
                                          (p - (1/2) * sqrt disc *^ q)