{-# LANGUAGE
    DeriveFunctor
#-}

module Geometry where

import qualified Data.Real.Constructible as C
import           Linear.V2

data Result a
  = None
  | Unique a
  | Distinct a a
  | Infinite
  deriving (Functor, Show)

type Point = V2 C.Construct

data Line
  = Vertical { _xIntercept :: C.Construct }
  | NonVertical { _slope :: C.Construct, _yIntercept :: C.Construct }

data Circle = Circle
  { _center :: Point
  , _radiusSquared :: C.Construct
  }
