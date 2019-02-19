{-# LANGUAGE
    GADTs
  , ExplicitForAll
#-}

module Command where

import           Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import           Geometry

type Name = String

data Input a
  = Reference Name
  | Raw a

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
  Construct :: forall a . Construction a -> Command
  Name :: forall a . Construction a -> Capture -> Command
  Discard :: Name -> Command
  Show :: Name -> Command

type Parser = MP.Parsec Void String

commandParser :: Parser Command
commandParser = undefined