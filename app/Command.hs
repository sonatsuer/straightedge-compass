{-# LANGUAGE
    GADTs
  , ExplicitForAll
  , LambdaCase
#-}

module Command where

import           Control.Monad (when)
import           Data.Functor (void)
import           Data.List.NonEmpty (some1, toList)
import           Data.Ratio
import           Data.Real.Constructible (Construct)
import           Data.Void (Void)
import           Linear (V2(..))
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
-------------------------------------------------------------------------------
import           Geometry (Point, Line(..), Circle(..), Result)

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

data AnyConstruction = forall a . AnyConstruction (Construction (Result a))

data Capture
  = Single String
  | Pair String String

data Command where
  Construct :: forall a . Construction (Result a) -> Command
  NameResult :: forall a . Construction (Result a) -> Capture -> Command
  NameObject :: forall a . RawObject a -> Name -> Command
  Discard :: Name -> Command
  Show :: Name -> Command

-------------------------------------------------------------------------------
-- Lexer and Parser

type Parser = Parsec Void String

spaceEof :: Parser ()
spaceEof = eof <|> space

finish :: Parser a -> Parser a
finish p = do
  val <- p
  space >> eof
  return val

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceEof

reserved :: String -> Parser ()
reserved str = void $ lexeme $ string str

charSpace :: Char -> Parser ()
charSpace c = char c >> space

parens :: Parser a -> Parser a
parens = between (charSpace '(') (char ')')

fractionP :: Parser (Ratio Integer)
fractionP = do
  x <- L.decimal
  void $ char '/'
  y <- L.decimal
  when (y == 0) (fail "Division by zero!")
  return $ x % y

-- This parses rational numbers and casts them as Construct
numberP :: Parser Construct
numberP = lexeme $ L.signed spaceEof $
      fromRational <$> try fractionP
  <|> fromIntegral <$> (L.decimal :: Parser Integer)

-- These are the reserved words.
intersectionOf, linePassingThrough, circleFrom, as, display, discard, separator :: Parser ()
intersectionOf = reserved "intersection"
linePassingThrough = reserved "line"
circleFrom = reserved "circleFrom"
as = reserved "as"
display = reserved "display"
discard = reserved "discard"
separator = reserved "and"

commandParser :: Parser Command
commandParser =
      try nameResultP
  <|> try nameObjectP
  <|> try constructP
  <|> try discardP
  <|> displayP

constructionP :: Parser AnyConstruction
constructionP =
      try (mkConstrP intersectionOf inputLineP inputLineP LineLineIntersection)
  <|> try (mkConstrP intersectionOf inputLineP inputCircleP LineCircleIntersection)
  <|> try (mkConstrP intersectionOf inputCircleP inputLineP (flip LineCircleIntersection))
  <|> try (mkConstrP intersectionOf inputCircleP inputCircleP CircleCircleIntersection)
  <|> try (mkConstrP linePassingThrough inputPointP inputPointP LineThroughPoints)
  <|> (mkConstrP circleFrom inputPointP inputPointP CircleFromPoints)
  where
    mkConstrP
      :: Parser ()
      -> Parser (Input a)
      -> Parser (Input b)
      -> (Input a -> Input b -> Construction (Result c))
      -> Parser AnyConstruction
    mkConstrP constructionNameP inp1P inp2P wrapper = do
      constructionNameP
      inp1 <- inp1P
      separator
      inp2 <- inp2P
      return $ AnyConstruction $ wrapper inp1 inp2

constructP :: Parser Command
constructP = finish $ do
  AnyConstruction constr <- constructionP
  return $ Construct constr

nameResultP :: Parser Command
nameResultP = finish $ do
  AnyConstruction constr <- constructionP
  as
  cap <- captureP
  return $ NameResult constr cap

captureP :: Parser Capture
captureP = try pairP <|> singleP
  where
    singleP =
      Single <$> lexeme referenceP
    pairP =
      Pair <$> lexeme referenceP <*> lexeme referenceP

nameObjectP :: Parser Command
nameObjectP =
      try (toCommandP $ RawPoint <$> pointP)
  <|> try (toCommandP $ RawLine <$> lineP)
  <|> toCommandP (RawCircle <$> circleP)
  where
    toCommandP :: Parser (RawObject a) -> Parser Command
    toCommandP objP = finish $ do
      rawObj <- objP
      as
      name <- referenceP
      return $ NameObject rawObj name


discardP :: Parser Command
discardP = finish $ discard >> Discard <$> referenceP

displayP :: Parser Command
displayP = finish $ display >> Show <$> referenceP

referenceP :: Parser Name
referenceP = lexeme $ char '$' >> toList <$> some1 alphaNumChar

pointP :: Parser Point
pointP = lexeme $ parens $ do
  x <- numberP
  charSpace ','
  y <- numberP
  return $ V2 x y

lineP :: Parser Line
lineP = verticalP <|> nonVerticalP
  where
    -- x = c
    verticalP = do
      charSpace 'x'
      charSpace '='
      c <- numberP
      return $ Vertical c
    -- y = m * x + c
    nonVerticalP = do
      charSpace 'y'
      charSpace '='
      m <- numberP
      charSpace '*'
      charSpace 'x'
      charSpace '+'
      c <- numberP
      return $ NonVertical m c

circleP :: Parser Circle
circleP = lexeme $ do
  charSpace 'C'
  center <- pointP
  rad <- numberP
  return $ Circle center (rad * rad)

toInput :: Parser (RawObject a) -> Parser (Input a)
toInput rawObjP = lexeme $
      try (Reference <$> referenceP)
  <|> Raw <$> rawObjP

inputPointP :: Parser (Input Point)
inputPointP = toInput $ RawPoint <$> pointP

inputLineP :: Parser (Input Line)
inputLineP = toInput $ RawLine <$> lineP

inputCircleP :: Parser (Input Circle)
inputCircleP = toInput $ RawCircle <$> circleP
