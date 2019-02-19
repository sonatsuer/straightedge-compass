{-# LANGUAGE
    GADTs
  , ExplicitForAll
  , LambdaCase
#-}

module Command where

import           Data.Void (Void)
import qualified Text.Megaparsec as MP
import           Geometry

type Name = String

data RawObject a where
  RawPoint :: Point -> RawObject Point
  RawLine :: Line -> RawObject Line
  RawCircle :: Circle -> RawObject Circle

instance Show (RawObject a) where
  show = \case
    RawPoint point -> show point
    RawLine line -> show line
    RawCircle circle -> show circle

data Input a
  = Reference Name
  | Raw (RawObject a)

data Construction a where
  LineLineIntersection
    :: Input Line -> Input Line -> Construction (Result Point)
  LineCircleIntersection
    :: Input Line -> Input Circle -> Construction (Result Point)
  CircleCircleIntersection
    :: Input Circle -> Input Circle -> Construction (Result Point)
  LineThroughPoints
    :: Input Point -> Input Point -> Construction (Result Line)
  CircleFromPoints
    :: Input Point -> Input Point -> Construction (Result Circle)

data Capture
  = Single String
  | Pair String String

data Command where
  Construct :: forall a . Construction (Result a) -> Command
  NameResult :: forall a . Construction (Result a) -> Capture -> Command
  NameObject :: forall a . RawObject a -> Name -> Command
  Discard :: Name -> Command
  Show :: Name -> Command

type Parser = MP.Parsec Void String

commandParser :: Parser Command
commandParser = undefined