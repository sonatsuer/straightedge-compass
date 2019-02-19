module Command where

import           Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import           Geometry

data Object
  = ObjPoint Point
  | ObjLine Line
  | ObjCircle Circle
  deriving Show

type Name = String

data Input a
  = Reference Name
  | Raw a

data Construction
  = LineLineIntersection (Input Line) (Input Line)
  | LineCircleIntersection (Input Line) (Input Circle)
  | CircleCircleIntersection (Input Circle) (Input Circle)
  | LineThroughPoints (Input Point) (Input Point)
  | CircleFromPoints (Input Point) (Input Point)

data Capture
  = Single String
  | Pair String String

data Command
  = Construct Construction
  | Name Construction Capture
  | Discard Name
  | Show Name

type Parser = MP.Parsec Void String

commandParser :: Parser Command
commandParser = undefined