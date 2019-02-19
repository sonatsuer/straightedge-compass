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

data Command
  = Construct Object
  | Assign Object Name
  | Discard Name
  | Show Name

type Parser = MP.Parsec Void String

commandParser :: Parser Command
commandParser = undefined