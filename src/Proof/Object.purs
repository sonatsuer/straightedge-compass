-- | Geometric objects that names in a script refer to.
module Proof.Object
  ( Obj(..)
  , Kind(..)
  , kindOf
  , kindName
  ) where

import Prelude

import Geometry (Circle, Line, Point)

data Obj = OPoint Point | OLine Line | OCircle Circle

data Kind = KPoint | KLine | KCircle

derive instance Eq Kind

kindOf :: Obj -> Kind
kindOf = case _ of
  OPoint _ -> KPoint
  OLine _ -> KLine
  OCircle _ -> KCircle

kindName :: Kind -> String
kindName = case _ of
  KPoint -> "a point"
  KLine -> "a line"
  KCircle -> "a circle"
