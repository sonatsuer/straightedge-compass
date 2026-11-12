-- | The abstract syntax of proof scripts.
-- |
-- | A script is a sequence of statements, one per line:
-- |
-- |   title "Euclid I.1"
-- |   given A = (0, 0)
-- |   c1 = circle A B
-- |   C = meet c1 c2 nearest (0, 1)
-- |   M = midpoint(A, B)
-- |   assert dist A C == dist A B
-- |   note "free text (Typst markup), attached to the step above"
-- |   snapshot
-- |
-- | plus macro definitions `def name(params) -> results:` … `end`, and for
-- | universal statements:
-- |
-- |   param p ~ 1/3                 -- a real variable, sampled at 1/3
-- |   given C = (p, q)              -- coordinates may be variables
-- |   P = point on u ~ (0, 1)       -- a free point on a line or circle
-- |   assume ccw A B C              -- a hypothesis
-- |   cases meet u k                -- an exhaustive split
-- |   two T1, T2:                   --   one branch per outcome
-- |     …
-- |   none:
-- |     …
-- |   end
-- |   sample p ~ 2/3                -- other sample values from here on
-- |   snapshot p ~ 2/3              -- a page break, and other samples
-- |
-- | and, for the figure only:
-- |
-- |   show segment A B
-- |   show polygon A B C
-- |   show circle O A               -- centre O, through A
-- |   show circle through A B C
module Proof.Syntax
  ( Name
  , RatPoint
  , Ref(..)
  , Selector(..)
  , Expr(..)
  , Cmp(..)
  , PredF(..)
  , Pred
  , Stmt(..)
  , Located
  , Macro
  , Script
  , Coord(..)
  , SampleValue(..)
  , SampleItem
  , CaseForm(..)
  , Label(..)
  , Branch
  , ParamDecl
  , ShowItem(..)
  , showItemText
  , labelText
  , labelsOf
  , showCaseForm
  , showSample
  , showStmt
  , showExpr
  , showPred
  , showSelector
  , showRatPoint
  , showCmp
  ) where

import Prelude

import Constructible.Rational (Rational)
import Data.Foldable (class Foldable)
import Data.Traversable (class Traversable)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)

type Name = String

-- | A point with rational coordinates, written `(x, y)`.
type RatPoint = { x :: Rational, y :: Rational }

-- | A point used only to choose between intersections: a name or a literal.
data Ref = RefName Name | RefLit RatPoint

-- | Picks one of two intersection points.
data Selector
  = Except Ref -- ^ the point other than this one
  | Nearest Ref -- ^ the point closer to this one
  | Farthest Ref -- ^ the point farther from this one
  | LeftOf Name Name -- ^ the point to the left of the directed line A → B
  | RightOf Name Name -- ^ the point to the right of the directed line A → B

data Expr
  = ELine Name Name -- ^ the line through two points
  | ECircle Name Name -- ^ the circle with a centre through a point
  | EMeet Name Name (Maybe Selector) -- ^ intersection points of two objects
  | ECall Name (Array Name) -- ^ a macro call
  | EPointOn Name (Maybe RatPoint) -- ^ a free point on a line or circle, sampled near a point

-- | A coordinate of a given point: a rational or a variable.
data Coord = CRat Rational | CParam Name

-- | A sample value: a number (for a variable) or a position (for a free
-- | point on an object).
data SampleValue = SVNum Rational | SVPoint RatPoint

derive instance Eq SampleValue

type SampleItem = { name :: Name, value :: SampleValue }

type ParamDecl = { name :: Name, sample :: Rational }

-- | Drawing only: something to show in the figure from here on.
data ShowItem
  = ShowSegment Name Name -- ^ the segment AB
  | ShowPolygon (Array Name) -- ^ the closed polygon through the points
  | ShowCircle Name Name -- ^ the circle with centre O through A
  | ShowThrough Name Name Name -- ^ the circle through three points

-- | What a `cases` statement splits on.
data CaseForm
  = CaseMeet Name Name -- ^ none / one X / two [X, Y] / same
  | CaseSide Name Name Name -- ^ side P A B: left / on / right
  | CaseCompare Name Name Name Name -- ^ compare dist A B, dist C D: less / equal / greater
  | CasePred (PredF Name) -- ^ P == Q: equal / different; other predicates: yes / no

data Label
  = LNone
  | LOne
  | LTwo
  | LSame
  | LLeft
  | LOn
  | LRight
  | LLess
  | LEqual
  | LGreater
  | LDifferent
  | LYes
  | LNo

derive instance Eq Label

-- | One branch of a `cases` statement.
type Branch =
  { label :: Label
  , names :: Array Name -- bound by `one X` and `two X, Y`
  , line :: Int
  , text :: String
  , body :: Array (Located Stmt)
  }

data Cmp = CEq | CNe | CLt | CLe | CGt | CGe

-- | A predicate over objects referred to by `a` (names in scripts, object
-- | ids after evaluation).
data PredF a
  = On a a -- ^ a point lies on a line or circle
  | PointEq a a
  | PointNe a a
  | DistCmp Cmp a a a a -- ^ dist A B ⋈ dist C D
  | Collinear (Array a)
  | Concyclic (Array a)
  | Parallel a a
  | Perpendicular a a
  | AngleEq a a a a a a -- ^ angle A B C == angle D E F
  | Ccw a a a -- ^ A, B, C turn counterclockwise
  | Between a a a -- ^ B lies strictly between A and C

derive instance Functor PredF
derive instance Foldable PredF
derive instance Traversable PredF

type Pred = PredF Name

data Stmt
  = Given Name RatPoint
  | Bind (Array Name) Expr
  | Assert Pred
  | Note String
  -- | A PDF page ends here (top level only), with new sample values.
  | Snapshot (Array SampleItem)
  | Param (Array ParamDecl)
  | GivenParam Name Coord Coord
  | Assume Pred
  | Sample (Array SampleItem)
  | Cases CaseForm (Array Branch)
  -- | Drawing only: no object, no step.
  | Show ShowItem

-- | A statement with its source line (1-based) and text.
type Located a = { line :: Int, text :: String, value :: a }

type Macro =
  { name :: Name
  , params :: Array Name
  , results :: Array Name
  , body :: Array (Located Stmt)
  , line :: Int
  }

type Script =
  { title :: Maybe String
  , macros :: Array Macro
  , body :: Array (Located Stmt)
  }

-------------------------------------------------------------------------------
-- Rendering back to source form

showRatPoint :: RatPoint -> String
showRatPoint p = "(" <> show p.x <> ", " <> show p.y <> ")"

showRef :: Ref -> String
showRef (RefName n) = n
showRef (RefLit p) = showRatPoint p

showSelector :: Selector -> String
showSelector = case _ of
  Except r -> "except " <> showRef r
  Nearest r -> "nearest " <> showRef r
  Farthest r -> "farthest " <> showRef r
  LeftOf a b -> "left " <> a <> " " <> b
  RightOf a b -> "right " <> a <> " " <> b

showExpr :: Expr -> String
showExpr = case _ of
  ELine a b -> "line " <> a <> " " <> b
  ECircle o p -> "circle " <> o <> " " <> p
  EMeet a b sel -> "meet " <> a <> " " <> b <> case sel of
    Nothing -> ""
    Just s -> " " <> showSelector s
  ECall f args -> f <> "(" <> joinWith ", " args <> ")"
  EPointOn o near -> "point on " <> o <> case near of
    Nothing -> ""
    Just p -> " ~ " <> showRatPoint p

showCmp :: Cmp -> String
showCmp = case _ of
  CEq -> "=="
  CNe -> "!="
  CLt -> "<"
  CLe -> "<="
  CGt -> ">"
  CGe -> ">="

showPred :: forall a. (a -> String) -> PredF a -> String
showPred sh = map sh >>> case _ of
  On p x -> "on " <> p <> " " <> x
  PointEq p q -> p <> " == " <> q
  PointNe p q -> p <> " != " <> q
  DistCmp c a b d e -> "dist " <> a <> " " <> b <> " " <> showCmp c <> " dist " <> d <> " " <> e
  Collinear ps -> "collinear " <> joinWith " " ps
  Concyclic ps -> "concyclic " <> joinWith " " ps
  Parallel l m -> "parallel " <> l <> " " <> m
  Perpendicular l m -> "perpendicular " <> l <> " " <> m
  AngleEq a b c d e f -> "angle " <> a <> " " <> b <> " " <> c <> " == angle " <> d <> " " <> e <> " " <> f
  Ccw a b c -> "ccw " <> a <> " " <> b <> " " <> c
  Between a b c -> "between " <> a <> " " <> b <> " " <> c

labelText :: Label -> String
labelText = case _ of
  LNone -> "none"
  LOne -> "one"
  LTwo -> "two"
  LSame -> "same"
  LLeft -> "left"
  LOn -> "on"
  LRight -> "right"
  LLess -> "less"
  LEqual -> "equal"
  LGreater -> "greater"
  LDifferent -> "different"
  LYes -> "yes"
  LNo -> "no"

-- | The branches a form has, in order.
labelsOf :: CaseForm -> Array Label
labelsOf = case _ of
  CaseMeet _ _ -> [ LTwo, LOne, LNone, LSame ]
  CaseSide _ _ _ -> [ LLeft, LOn, LRight ]
  CaseCompare _ _ _ _ -> [ LLess, LEqual, LGreater ]
  CasePred (PointEq _ _) -> [ LEqual, LDifferent ]
  CasePred _ -> [ LYes, LNo ]

showCaseForm :: CaseForm -> String
showCaseForm = case _ of
  CaseMeet a b -> "meet " <> a <> " " <> b
  CaseSide p a b -> "side " <> p <> " " <> a <> " " <> b
  CaseCompare a b c d -> "compare dist " <> a <> " " <> b <> ", dist " <> c <> " " <> d
  CasePred p -> showPred identity p

showSample :: SampleItem -> String
showSample i = i.name <> " ~ " <> case i.value of
  SVNum q -> show q
  SVPoint p -> showRatPoint p

showCoord :: Coord -> String
showCoord = case _ of
  CRat q -> show q
  CParam n -> n

showStmt :: Stmt -> String
showStmt = case _ of
  Given n p -> "given " <> n <> " = " <> showRatPoint p
  Bind ns e -> joinWith ", " ns <> " = " <> showExpr e
  Assert p -> "assert " <> showPred identity p
  Note s -> "note " <> show s
  Snapshot [] -> "snapshot"
  Snapshot items -> "snapshot " <> joinWith ", " (map showSample items)
  Param ps -> "param " <> joinWith ", " (map (\d -> d.name <> " ~ " <> show d.sample) ps)
  GivenParam n x y -> "given " <> n <> " = (" <> showCoord x <> ", " <> showCoord y <> ")"
  Assume p -> "assume " <> showPred identity p
  Sample items -> "sample " <> joinWith ", " (map showSample items)
  Cases f _ -> "cases " <> showCaseForm f
  Show item -> "show " <> showItemText item

showItemText :: ShowItem -> String
showItemText = case _ of
  ShowSegment a b -> "segment " <> a <> " " <> b
  ShowPolygon ps -> "polygon " <> joinWith " " ps
  ShowCircle o a -> "circle " <> o <> " " <> a
  ShowThrough a b c -> "circle through " <> a <> " " <> b <> " " <> c
