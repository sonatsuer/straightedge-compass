-- | Parser for proof scripts.
-- |
-- | Scripts are line oriented: each non-blank line is one statement. `--`
-- | starts a comment (outside string literals). Lines are parsed one at a
-- | time, so an error always points at a single line, and `def … end` blocks
-- | are grouped afterwards.
module Proof.Parser
  ( ParseError
  , parseScript
  , parseStatement
  , Item(..)
  , parseItem
  , keywords
  ) where

import Prelude

import Constructible.Rational as Q
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.CodePoint.Unicode (isAlpha, isDecDigit)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..), codePointFromChar, joinWith, split, trim)
import Data.String.CodePoints (fromCodePointArray)
import Data.String.CodeUnits as CU
import Data.Tuple (Tuple(..))
import Parsing (Parser, Position(..), fail, parseErrorMessage, parseErrorPosition, runParser)
import Parsing.Combinators (notFollowedBy, optionMaybe, sepBy1, try, (<?>))
import Parsing.String (char, eof, satisfy, satisfyCodePoint, string)
import Parsing.String.Basic (skipSpaces, takeWhile1)
import Proof.Syntax (Branch, CaseForm(..), Cmp(..), Coord(..), Expr(..), Label(..), Located, Macro, Name, PredF(..), Pred, RatPoint, Ref(..), SampleItem, SampleValue(..), Script, Selector(..), ShowItem(..), Stmt(..), labelText, labelsOf)

-- | A parse error at a line and column (both 1-based).
type ParseError = { line :: Int, column :: Int, message :: String }

-- | One line of a script.
data Item
  = IStmt Stmt
  | ITitle String
  | IDef Name (Array Name) (Array Name)
  | IEnd
  | ICases CaseForm
  | ILabel Label (Array Name)

keywords :: Array String
keywords =
  [ "given"
  , "line"
  , "circle"
  , "meet"
  , "except"
  , "nearest"
  , "farthest"
  , "left"
  , "right"
  , "assert"
  , "note"
  , "title"
  , "def"
  , "end"
  , "on"
  , "dist"
  , "collinear"
  , "concyclic"
  , "parallel"
  , "perpendicular"
  , "angle"
  , "ccw"
  , "between"
  , "snapshot"
  , "param"
  , "assume"
  , "sample"
  , "cases"
  , "point"
  , "side"
  , "compare"
  , "show"
  ]

-------------------------------------------------------------------------------
-- Lexical level

lexeme :: forall a. Parser String a -> Parser String a
lexeme p = p <* skipSpaces

symbol :: String -> Parser String Unit
symbol s = void (lexeme (string s)) <?> ("'" <> s <> "'")

identCodePoint :: Parser String Char
identCodePoint = satisfy \c -> c == '_' || c == '\'' || isAlpha (codePointFromChar c) || isDecDigit (codePointFromChar c)

keyword :: String -> Parser String Unit
keyword kw = lexeme (try (string kw *> notFollowedBy identCodePoint)) <?> kw

ident :: Parser String Name
ident = lexeme (try go) <?> "a name"
  where
  go = do
    first <- satisfyCodePoint (\c -> c == codePointFromChar '_' || isAlpha c)
    rest <- Array.many (satisfyCodePoint (\c -> c == codePointFromChar '_' || c == codePointFromChar '\'' || isAlpha c || isDecDigit c))
    let
      name = fromCodePointArray (Array.cons first rest)
    if Array.elem name keywords then fail ("'" <> name <> "' is a keyword") else pure name

rational :: Parser String Q.Rational
rational = lexeme (try go) <?> "a number (integer or p/q)"
  where
  digits = takeWhile1 isDecDigit
  go = do
    neg <- optionMaybe (char '-')
    n <- digits
    d <- optionMaybe (char '/' *> digits)
    let
      text = (if isJust neg then "-" else "") <> n <> case d of
        Nothing -> ""
        Just ds -> "/" <> ds
    case Q.parse text of
      Just q -> pure q
      Nothing -> fail "invalid number (zero denominator?)"

ratPoint :: Parser String RatPoint
ratPoint = do
  symbol "("
  x <- rational
  symbol ","
  y <- rational
  symbol ")"
  pure { x, y }

-- | A string in double quotes. Only `\"`, `\\` and `\n` are escapes; any
-- | other backslash is kept, so Typst escapes such as `\*` survive in notes.
stringLit :: Parser String String
stringLit = lexeme (char '"' *> go []) <?> "a string in double quotes"
  where
  go acc = do
    c <- satisfy (const true) <?> "closing '\"'"
    case c of
      '"' -> pure (CU.fromCharArray acc)
      '\\' -> do
        e <- satisfy (const true) <?> "a character after '\\'"
        case e of
          'n' -> go (Array.snoc acc '\n')
          '"' -> go (Array.snoc acc '"')
          '\\' -> go (Array.snoc acc '\\')
          _ -> go (acc <> [ '\\', e ])
      _ -> go (Array.snoc acc c)

coord :: Parser String Coord
coord = (CRat <$> rational) <|> (CParam <$> ident) <?> "a number or a variable"

-- | `name ~ value`, the value a number or a point.
sampleItem :: Parser String SampleItem
sampleItem = do
  name <- ident
  symbol "~"
  value <- (SVPoint <$> ratPoint) <|> (SVNum <$> rational)
  pure { name, value }

samples :: Parser String (Array SampleItem)
samples = Array.fromFoldable <$> sepBy1 sampleItem (symbol ",")

-------------------------------------------------------------------------------
-- Statements

ref :: Parser String Ref
ref = (RefLit <$> ratPoint) <|> (RefName <$> ident)

selector :: Parser String Selector
selector =
  (keyword "except" *> (Except <$> ref))
    <|> (keyword "nearest" *> (Nearest <$> ref))
    <|> (keyword "farthest" *> (Farthest <$> ref))
    <|> (keyword "left" *> (LeftOf <$> ident <*> ident))
    <|> (keyword "right" *> (RightOf <$> ident <*> ident))

expr :: Parser String Expr
expr =
  (keyword "line" *> (ELine <$> ident <*> ident))
    <|> (keyword "circle" *> (ECircle <$> ident <*> ident))
    <|> (keyword "meet" *> (EMeet <$> ident <*> ident <*> optionMaybe selector))
    <|> (keyword "point" *> keyword "on" *> (EPointOn <$> ident <*> optionMaybe (symbol "~" *> ratPoint)))
    <|> call
      <?> "line, circle, meet or a macro call"
  where
  call = do
    f <- ident
    symbol "("
    args <- (Array.fromFoldable <$> sepBy1 ident (symbol ",")) <|> pure []
    symbol ")"
    pure (ECall f args)

cmp :: Parser String Cmp
cmp =
  (symbol "==" $> CEq)
    <|> (symbol "!=" $> CNe)
    <|> (symbol "<=" $> CLe)
    <|> (symbol ">=" $> CGe)
    <|> (symbol "<" $> CLt)
    <|> (symbol ">" $> CGt)
      <?> "a comparison (==, !=, <, <=, >, >=)"

names1 :: Parser String (Array Name)
names1 = NEA.toArray <$> NEA.some ident

pred :: Parser String Pred
pred =
  (keyword "on" *> (On <$> ident <*> ident))
    <|> distP
    <|> (keyword "collinear" *> (Collinear <$> names1))
    <|> (keyword "concyclic" *> (Concyclic <$> names1))
    <|> (keyword "parallel" *> (Parallel <$> ident <*> ident))
    <|> (keyword "perpendicular" *> (Perpendicular <$> ident <*> ident))
    <|> angleP
    <|> (keyword "ccw" *> (Ccw <$> ident <*> ident <*> ident))
    <|> (keyword "between" *> (Between <$> ident <*> ident <*> ident))
    <|> pointCmp
      <?> "a predicate"
  where
  distP = do
    keyword "dist"
    a <- ident
    b <- ident
    c <- cmp
    keyword "dist"
    d <- ident
    e <- ident
    pure (DistCmp c a b d e)
  angleP = do
    keyword "angle"
    a <- ident
    b <- ident
    c <- ident
    symbol "=="
    keyword "angle"
    AngleEq a b c <$> ident <*> ident <*> ident
  pointCmp = do
    p <- ident
    c <- cmp
    q <- ident
    case c of
      CEq -> pure (PointEq p q)
      CNe -> pure (PointNe p q)
      _ -> fail "points can only be compared with == or !="

stmt :: Parser String Stmt
stmt =
  givenP
    <|> (keyword "snapshot" *> (Snapshot <$> (samples <|> pure [])))
    <|> (keyword "sample" *> (Sample <$> samples))
    <|> (keyword "param" *> (Param <$> (Array.fromFoldable <$> sepBy1 paramDecl (symbol ","))))
    <|> (keyword "assume" *> (Assume <$> pred))
    <|> (keyword "show" *> (Show <$> showItem))
    <|> (keyword "assert" *> (Assert <$> pred))
    <|> (keyword "note" *> (Note <$> stringLit))
    <|> binding
      <?> "a statement"
  where
  binding = do
    ns <- Array.fromFoldable <$> sepBy1 ident (symbol ",")
    symbol "="
    Bind ns <$> expr
  paramDecl = do
    name <- ident
    symbol "~"
    sample <- rational
    pure { name, sample }
  givenP = do
    keyword "given"
    n <- ident
    symbol "="
    symbol "("
    x <- coord
    symbol ","
    y <- coord
    symbol ")"
    pure case x, y of
      CRat a, CRat b -> Given n { x: a, y: b }
      _, _ -> GivenParam n x y

-- | `segment A B`, `polygon A B C …`, `circle O A`, `circle through A B C`.
-- | The words are not reserved: they are only special after `show`.
showItem :: Parser String ShowItem
showItem =
  (word "segment" *> (ShowSegment <$> ident <*> ident))
    <|> polygonP
    <|> (keyword "circle" *> ((word "through" *> (ShowThrough <$> ident <*> ident <*> ident)) <|> (ShowCircle <$> ident <*> ident)))
      <?> "segment A B, polygon A B C …, circle O A or circle through A B C"
  where
  word w = lexeme (try (string w *> notFollowedBy identCodePoint)) <?> w
  polygonP = do
    word "polygon"
    ps <- names1
    when (Array.length ps < 3) (fail "a polygon needs at least three points")
    pure (ShowPolygon ps)

caseForm :: Parser String CaseForm
caseForm =
  (keyword "meet" *> (CaseMeet <$> ident <*> ident))
    <|> (keyword "side" *> (CaseSide <$> ident <*> ident <*> ident))
    <|> compareP
    <|> (CasePred <$> pred)
      <?> "meet a b, side P A B, compare dist A B, dist C D, or a predicate"
  where
  compareP = do
    keyword "compare"
    keyword "dist"
    a <- ident
    b <- ident
    symbol ","
    keyword "dist"
    CaseCompare a b <$> ident <*> ident

-- | A branch label such as `two T1, T2:` or `none:`.
labelP :: Parser String Item
labelP = try do
  l <- word
  label <- case l of
    "none" -> pure LNone
    "one" -> pure LOne
    "two" -> pure LTwo
    "same" -> pure LSame
    "left" -> pure LLeft
    "on" -> pure LOn
    "right" -> pure LRight
    "less" -> pure LLess
    "equal" -> pure LEqual
    "greater" -> pure LGreater
    "different" -> pure LDifferent
    "yes" -> pure LYes
    "no" -> pure LNo
    _ -> fail "a branch label"
  names <- Array.many (ident <* optionMaybe (symbol ","))
  symbol ":"
  pure (ILabel label names)
  where
  word = lexeme (fromCodePointArray <$> Array.some (satisfyCodePoint isAlpha))

item :: Parser String Item
item = do
  skipSpaces
  i <- (keyword "title" *> (ITitle <$> stringLit))
    <|> defP
    <|> (keyword "end" $> IEnd)
    <|> (keyword "cases" *> (ICases <$> caseForm) <* optionMaybe (symbol ":"))
    <|> labelP
    <|> (IStmt <$> stmt)
  eof <?> "end of line"
  pure i
  where
  defP = do
    keyword "def"
    name <- ident
    symbol "("
    params <- (Array.fromFoldable <$> sepBy1 ident (symbol ",")) <|> pure []
    symbol ")"
    symbol "->"
    results <- Array.fromFoldable <$> sepBy1 ident (symbol ",")
    void (optionMaybe (symbol ":"))
    pure (IDef name params results)

-- | Parses one line (without its comment).
parseItem :: Int -> String -> Either ParseError Item
parseItem lineNo text = case runParser text item of
  Right i -> Right i
  Left err ->
    let
      Position pos = parseErrorPosition err
    in
      Left { line: lineNo, column: pos.column, message: parseErrorMessage err }

-- | Parses a single statement, e.g. one typed into the console.
parseStatement :: String -> Either ParseError Stmt
parseStatement text = case parseItem 1 (stripComment text) of
  Right (IStmt s) -> Right s
  Right _ -> Left { line: 1, column: 1, message: "expected a statement" }
  Left e -> Left e

-------------------------------------------------------------------------------
-- Whole scripts

-- | Removes a `--` comment that is not inside a string literal.
stripComment :: String -> String
stripComment s = go 0 false
  where
  chars = CU.toCharArray s
  n = Array.length chars
  at i = Array.index chars i
  go i inString
    | i >= n = s
    | otherwise = case at i, inString of
        Just '"', _ -> go (i + 1) (not inString)
        Just '\\', true -> go (i + 2) true
        Just '-', false | at (i + 1) == Just '-' -> CU.take i s
        _, _ -> go (i + 1) inString

-- | An open `cases` block: its form and the branches so far (statements go
-- | to the last one).
type Frame = { form :: CaseForm, line :: Int, text :: String, branches :: Array Branch }

type Building =
  { title :: Maybe String
  , macros :: Array Macro
  , body :: Array (Located Stmt)
  , open :: Maybe Macro
  , frames :: Array Frame
  }

-- | Parses a whole script, stopping at the first error.
parseScript :: String -> Either ParseError Script
parseScript src = do
  let
    lines = Array.mapWithIndex (\i l -> Tuple (i + 1) (trim (stripComment (dropCR l)))) (split (Pattern "\n") src)
    nonBlank = Array.filter (\(Tuple _ l) -> l /= "") lines
  final <- foldl step (Right { title: Nothing, macros: [], body: [], open: Nothing, frames: [] }) nonBlank
  case Array.last final.frames, final.open of
    Just f, _ -> Left { line: f.line, column: 1, message: "cases has no matching end" }
    _, Just m -> Left { line: m.line, column: 1, message: "def " <> m.name <> " has no matching end" }
    _, _ -> Right { title: final.title, macros: final.macros, body: final.body }
  where
  dropCR l = case CU.stripSuffix (Pattern "\r") l of
    Just l' -> l'
    Nothing -> l
  step acc (Tuple lineNo text) = do
    b <- acc
    i <- parseItem lineNo text
    let
      located v = { line: lineNo, text, value: v }
      err msg = Left { line: lineNo, column: 1, message: msg }
      inFrame = not (Array.null b.frames)
    case i, b.open of
      ITitle _, _ | inFrame -> err "title inside cases"
      ITitle t, Nothing
        | isJust b.title -> err "the title is already set"
        | otherwise -> Right b { title = Just t }
      ITitle _, Just _ -> err "title inside a macro definition"
      IDef _ _ _, _ | inFrame -> err "def inside cases"
      IDef name params results, Nothing ->
        if isJust (Array.find (\m -> m.name == name) b.macros) then err ("macro " <> name <> " is already defined")
        else Right b { open = Just { name, params, results, body: [], line: lineNo } }
      IDef _ _ _, Just m -> err ("def inside the definition of " <> m.name <> " (missing end?)")
      ICases _, Just m -> err ("cases inside the definition of " <> m.name <> " (cases are top level only)")
      ICases form, Nothing -> Right b { frames = Array.snoc b.frames { form, line: lineNo, text, branches: [] } }
      ILabel label names, _ -> case Array.unsnoc b.frames of
        Nothing -> err (labelText label <> ": a branch label outside cases")
        Just { init, last }
          | not (Array.elem label (labelsOf last.form)) ->
              err (labelText label <> " is not a branch of this cases (expected " <> joinWith ", " (map labelText (labelsOf last.form)) <> ")")
          | isJust (Array.find (\br -> br.label == label) last.branches) -> err ("the branch " <> labelText label <> " appears twice")
          | otherwise -> case label, Array.length names of
              LOne, 1 -> push init last
              LOne, _ -> err "one: bind exactly one name, as in `one T:`"
              LTwo, 0 -> push init last
              LTwo, 2 -> push init last
              LTwo, _ -> err "two: bind two names or none, as in `two T1, T2:`"
              _, 0 -> push init last
              _, _ -> err (labelText label <> ": this branch binds no names")
        where
        push init last =
          Right b { frames = Array.snoc init last { branches = Array.snoc last.branches { label, names, line: lineNo, text, body: [] } } }
      IEnd, _ -> case Array.unsnoc b.frames of
        Just { init, last }
          | Array.null last.branches -> err "cases without branches"
          | otherwise -> append (b { frames = init }) { line: last.line, text: last.text, value: Cases last.form last.branches }
        Nothing -> case b.open of
          Nothing -> err "end without def or cases"
          Just m -> Right b { macros = Array.snoc b.macros m, open = Nothing }
      IStmt s, Just m -> case s of
        Given _ _ -> err ("given inside the definition of " <> m.name)
        GivenParam _ _ _ -> err ("given inside the definition of " <> m.name)
        Snapshot _ -> err ("snapshot inside the definition of " <> m.name <> " (snapshots are top level only)")
        Param _ -> err ("param inside the definition of " <> m.name)
        Assume _ -> err ("assume inside the definition of " <> m.name)
        Sample _ -> err ("sample inside the definition of " <> m.name)
        Bind _ (EPointOn _ _) -> err ("a free point inside the definition of " <> m.name <> " (constructions have no free choices)")
        Show _ -> err ("show inside the definition of " <> m.name <> " (show is top level only)")
        _ -> Right b { open = Just m { body = Array.snoc m.body (located s) } }
      IStmt s, Nothing -> append b (located s)
    where
    append b' st = case Array.unsnoc b'.frames of
      Just { init, last } -> case Array.unsnoc last.branches of
        Just { init: bi, last: br } ->
          Right b' { frames = Array.snoc init last { branches = Array.snoc bi br { body = Array.snoc br.body st } } }
        Nothing -> Left { line: st.line, column: 1, message: "expected a branch label after cases, such as `two T1, T2:`" }
      Nothing -> Right b' { body = Array.snoc b'.body st }
