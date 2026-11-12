-- | Towers of quadratic extensions over the rational functions ℚ(x):
-- | `Constructible.Field` with ℚ(x) as the base field.
-- |
-- | Arithmetic within one tower is pure and exact. Over ℚ(x) the sign of an
-- | element is a function of the parameters, so every operation that needs
-- | one runs in `PM`, which asks an oracle (the sign engine, which knows the
-- | hypotheses in force) and fails with a diagnosis when the sign is not
-- | determined. Those operations are: comparisons and signs, the
-- | non-negative choice of a square root of a square (√(f²) = ±f), and the
-- | identification of radicals when two towers are joined.
module Constructible.ParamField
  ( PC
  , Sgn(..)
  , Oracle
  , PM
  , runPM
  , failPM
  , askOracle
  , fromRatFun
  , fromRational
  , fromInt
  , var
  , isZero
  , toRatFun
  , depth
  , add
  , sub
  , mul
  , div
  , neg
  , eq
  , sign
  , sqrt
  , sqrtAny
  , PDecon(..)
  , deconstruct
  , baseElements
  , joinAll
  , radicands
  , generator
  , evalAt
  , Ex
  , ex
  , run
  ) where

import Prelude hiding (add, div, mul, sub)

import Constructible.Field as C
import Constructible.Rational (Rational)
import Constructible.Rational as Q
import Constructible.RatFun (RatFun)
import Constructible.RatFun as RF
import Control.Monad.Except (ExceptT, catchError, runExceptT, throwError)
import Control.Monad.Reader (Reader, ask, runReader)
import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Partial.Unsafe (unsafeCrashWith)
import Unsafe.Reference (unsafeRefEq)

-- | What is known about the sign of an element on the current domain.
data Sgn = Pos | Neg | Zero | NonNeg | NonPos | Unknown

derive instance Eq Sgn

-- | The sign engine: the sign of a base element, and an explanation for
-- | error messages when it is not determined.
type Oracle = { sign :: RatFun -> Sgn, explain :: RatFun -> String }

type PM = ExceptT String (Reader Oracle)

runPM :: forall a. Oracle -> PM a -> Either String a
runPM o m = runReader (runExceptT m) o

failPM :: forall a. String -> PM a
failPM = throwError

askOracle :: PM Oracle
askOracle = ask

data Field = KF | Sqrt Field Elt

data Elt = K RatFun | Zero' | E Elt Elt

-- | An element of a tower over ℚ(x), with its field.
data PC = PC Field Elt

shapeError :: forall a. String -> a
shapeError fn = unsafeCrashWith ("Constructible.ParamField." <> fn <> ": element does not match its field")

-------------------------------------------------------------------------------
-- Arithmetic within one field (pure)

zeroK :: Field -> Elt
zeroK KF = K zero
zeroK (Sqrt _ _) = Zero'

isZeroK :: Field -> Elt -> Boolean
isZeroK KF (K a) = RF.isZero a
isZeroK (Sqrt _ _) Zero' = true
isZeroK (Sqrt _ _) (E _ _) = false
isZeroK _ _ = shapeError "isZeroK"

sqrtElt :: Field -> Elt -> Elt -> Elt
sqrtElt k a b
  | isZeroK k a && isZeroK k b = Zero'
  | otherwise = E a b

sqrtLift :: Field -> Elt -> Elt
sqrtLift k a = sqrtElt k a (zeroK k)

fromBaseK :: Field -> RatFun -> Elt
fromBaseK KF a = K a
fromBaseK (Sqrt k _) a = sqrtLift k (fromBaseK k a)

addK :: Field -> Elt -> Elt -> Elt
addK KF (K a) (K b) = K (a + b)
addK (Sqrt _ _) Zero' a = a
addK (Sqrt _ _) a Zero' = a
addK (Sqrt k _) (E a b) (E c d) = sqrtElt k (addK k a c) (addK k b d)
addK _ _ _ = shapeError "addK"

negateK :: Field -> Elt -> Elt
negateK KF (K a) = K (negate a)
negateK (Sqrt _ _) Zero' = Zero'
negateK (Sqrt k _) (E a b) = E (negateK k a) (negateK k b)
negateK _ _ = shapeError "negateK"

subK :: Field -> Elt -> Elt -> Elt
subK k a b = addK k a (negateK k b)

mulK :: Field -> Elt -> Elt -> Elt
mulK KF (K a) (K b) = K (a * b)
mulK (Sqrt _ _) Zero' _ = Zero'
mulK (Sqrt _ _) _ Zero' = Zero'
mulK (Sqrt k r) (E a b) (E c d) =
  sqrtElt k (addK k (mulK k a c) (mulK k r (mulK k b d))) (addK k (mulK k a d) (mulK k b c))
mulK _ _ _ = shapeError "mulK"

sqK :: Field -> Elt -> Elt
sqK k a = mulK k a a

-- | The inverse of a nonzero element.
recipK :: Field -> Elt -> Elt
recipK KF (K a) = K (recip a)
recipK (Sqrt k r) (E a b) =
  let
    c = recipK k (subK k (sqK k a) (mulK k r (sqK k b)))
  in
    E (mulK k a c) (mulK k (negateK k b) c)
recipK _ _ = shapeError "recipK"

eqK :: Field -> Elt -> Elt -> Boolean
eqK KF (K a) (K b) = a == b
eqK (Sqrt _ _) Zero' Zero' = true
eqK (Sqrt k _) (E a b) (E c d) = eqK k a c && eqK k b d
eqK (Sqrt _ _) _ _ = false
eqK _ _ _ = shapeError "eqK"

-------------------------------------------------------------------------------
-- Signs and square roots (ask the oracle)

-- | Both signs known: the sign of the sum a + b·√r (√r > 0), when it
-- | follows from them alone.
combine :: Sgn -> Sgn -> Maybe Sgn
combine sa sb = case sa, sb of
  s, Zero -> Just s
  Zero, s -> Just s
  Pos, Pos -> Just Pos
  Pos, NonNeg -> Just Pos
  NonNeg, Pos -> Just Pos
  NonNeg, NonNeg -> Just NonNeg
  Neg, Neg -> Just Neg
  Neg, NonPos -> Just Neg
  NonPos, Neg -> Just Neg
  NonPos, NonPos -> Just NonPos
  _, _ -> Nothing

sgnK :: Field -> Elt -> PM Sgn
sgnK KF (K a)
  | RF.isZero a = pure Zero
  | otherwise = case RF.toRational a of
      Just q -> pure (if Q.signum q > 0 then Pos else Neg)
      Nothing -> (\o -> o.sign a) <$> askOracle
sgnK (Sqrt _ _) Zero' = pure Zero
sgnK (Sqrt k r) (E a b) = do
  sa <- sgnK k a
  sb <- sgnK k b
  case combine sa sb of
    Just s -> pure s
    Nothing ->
      -- a + b√r = norm / (a − b√r) with norm = a² − r·b²; when a − b√r has
      -- a known strict sign, the sign of the sum follows from the norm.
      let
        norm = subK k (sqK k a) (mulK k r (sqK k b))
        same = case sa, sb of
          Pos, Neg -> Just true
          Pos, NonPos -> Just true
          NonNeg, Neg -> Just true
          Neg, Pos -> Just false
          Neg, NonNeg -> Just false
          NonPos, Pos -> Just false
          _, _ -> Nothing
      in
        case same of
          Nothing -> pure Unknown
          Just s -> do
            sn <- sgnK k norm
            pure (if s then sn else flipSgn sn)
sgnK _ _ = shapeError "sgnK"

flipSgn :: Sgn -> Sgn
flipSgn = case _ of
  Pos -> Neg
  Neg -> Pos
  NonNeg -> NonPos
  NonPos -> NonNeg
  s -> s

-- | The sign of a base element must be determined (weakly) for choosing a
-- | square root; fails with the oracle's explanation otherwise.
weakSign :: String -> RatFun -> PM Boolean
weakSign what s = do
  o <- askOracle
  case o.sign s of
    Pos -> pure true
    NonNeg -> pure true
    Zero -> pure true
    Neg -> pure false
    NonPos -> pure false
    Unknown -> failPM (what <> ": " <> o.explain s)

-- | The non-negative square root within the field, if there is one.
sqrtK :: Field -> Elt -> PM (Maybe Elt)
sqrtK = rootK false

-- | A square root within the field, if there is one; with `anySign` its
-- | sign is not chosen (so no sign needs to be known). The formula
-- | c + (b/2c)·√r is a root of a + b√r for any choices of the inner roots.
rootK :: Boolean -> Field -> Elt -> PM (Maybe Elt)
rootK anySign KF (K a) = case RF.sqrt a of
  Nothing -> pure Nothing
  Just s
    | anySign -> pure (Just (K s))
    | otherwise -> do
        nonneg <- weakSign "to choose a square root, the sign is needed" s
        pure (Just (K (if nonneg then s else negate s)))
rootK _ (Sqrt _ _) Zero' = pure (Just Zero')
rootK anySign (Sqrt k r) (E a b)
  | isZeroK k b = do
      mc <- rootK anySign k a
      case mc of
        Just c -> pure (Just (sqrtLift k c))
        Nothing -> map (E (zeroK k)) <$> rootK anySign k (mulK k a (recipK k r))
  | otherwise = do
      mn <- rootK anySign k (subK k (sqK k a) (mulK k r (sqK k b)))
      case mn of
        Nothing -> pure Nothing
        Just n -> do
          let
            half = fromBaseK k (RF.fromRational (Q.fromInts 1 2))
            p = mulK k half (addK k a n)
            q = mulK k half b
          mc <- rootK anySign k p
          case mc of
            Just c | not (isZeroK k c) -> pure (Just (E c (mulK k q (recipK k c))))
            _ -> do
              md <- rootK anySign k (mulK k p (recipK k r))
              pure case md of
                Just d | not (isZeroK k d) -> Just (E (mulK k q (recipK k d)) d)
                _ -> Nothing
rootK _ _ _ = shapeError "sqrtK"

-------------------------------------------------------------------------------
-- Joining towers

type Join = { field :: Field, f1 :: Elt -> Elt, f2 :: Elt -> Elt }

sameField :: Field -> Field -> Boolean
sameField k1 k2
  | unsafeRefEq k1 k2 = true
  | otherwise = case k1, k2 of
      KF, KF -> true
      Sqrt a r, Sqrt b s -> sameField a b && (unsafeRefEq r s || eqK a r s)
      _, _ -> false

fieldDepth :: Field -> Int
fieldDepth KF = 0
fieldDepth (Sqrt k _) = 1 + fieldDepth k

liftFrom :: Int -> Field -> Elt -> Elt
liftFrom 0 _ a = a
liftFrom _ KF a = a
liftFrom n (Sqrt k _) a = sqrtLift k (liftFrom (n - 1) k a)

ancestor :: Int -> Field -> Field
ancestor 0 k = k
ancestor _ KF = KF
ancestor n (Sqrt k _) = ancestor (n - 1) k

joinK :: Field -> Field -> PM Join
joinK = joinWith false

-- | With `formal`, a radical that cannot be identified (its sign is not
-- | determined) is adjoined as a new one: the tower may then be redundant,
-- | which is harmless for certificates.
joinWith :: Boolean -> Field -> Field -> PM Join
joinWith formal k1 k2
  | sameField k1 k2 = pure { field: k1, f1: identity, f2: identity }
  | otherwise =
      let
        d1 = fieldDepth k1
        d2 = fieldDepth k2
      in
        if d1 > d2 && sameField (ancestor (d1 - d2) k1) k2 then
          pure { field: k1, f1: identity, f2: liftFrom (d1 - d2) k1 }
        else if d2 > d1 && sameField (ancestor (d2 - d1) k2) k1 then
          pure { field: k2, f1: liftFrom (d2 - d1) k2, f2: identity }
        else joinGeneral formal k1 k2

joinGeneral :: Boolean -> Field -> Field -> PM Join
joinGeneral _ KF k = pure { field: k, f1: fromBase, f2: identity }
  where
  fromBase (K a) = fromBaseK k a
  fromBase _ = shapeError "joinK"
joinGeneral _ k KF = pure { field: k, f1: identity, f2: fromBase }
  where
  fromBase (K a) = fromBaseK k a
  fromBase _ = shapeError "joinK"
joinGeneral formal k1 (Sqrt k2 r) = do
  j <- joinWith formal k1 k2
  let
    k = j.field
    r' = j.f2 r
  ms <-
    if formal then case findRadical k r' of
      Just g -> pure (Just g)
      Nothing -> sqrtK k r' `catchError` (\_ -> pure Nothing)
    else sqrtK k r'
  pure case ms of
    Nothing ->
      let
        f2' Zero' = Zero'
        f2' (E a b) = E (j.f2 a) (j.f2 b)
        f2' _ = shapeError "joinK"
      in
        { field: Sqrt k r', f1: sqrtLift k <<< j.f1, f2: f2' }
    Just s ->
      let
        f2' Zero' = zeroK k
        f2' (E a b) = addK k (j.f2 a) (mulK k (j.f2 b) s)
        f2' _ = shapeError "joinK"
      in
        { field: k, f1: j.f1, f2: f2' }

-- | The generator √rᵢ of k (as an element of k) whose radicand is r.
findRadical :: Field -> Elt -> Maybe Elt
findRadical k r = go 0
  where
  n = fieldDepth k
  go i
    | i >= n = Nothing
    | otherwise = case ancestor (n - i - 1) k of
        Sqrt below ri ->
          if eqK k (liftFrom (n - i - 1) k (sqrtLift below ri)) r then
            Just (liftFrom (n - i - 1) k (E (zeroK below) (fromBaseK below one)))
          else go (i + 1)
        KF -> Nothing

demote :: Field -> Elt -> PC
demote KF a = PC KF a
demote (Sqrt _ _) Zero' = PC KF (K zero)
demote f@(Sqrt k _) e@(E a b)
  | isZeroK k b = demote k a
  | otherwise = PC f e
demote _ _ = shapeError "demote"

binary :: (Field -> Elt -> Elt -> Elt) -> PC -> PC -> PM PC
binary op (PC k1 a1) (PC k2 a2) = do
  j <- joinK k1 k2
  pure (demote j.field (op j.field (j.f1 a1) (j.f2 a2)))

-------------------------------------------------------------------------------
-- The number type

fromRatFun :: RatFun -> PC
fromRatFun a = PC KF (K a)

fromRational :: Rational -> PC
fromRational = fromRatFun <<< RF.fromRational

fromInt :: Int -> PC
fromInt = fromRational <<< Q.fromInt

-- | The parameter with the given index.
var :: Int -> PC
var = fromRatFun <<< RF.var

isZero :: PC -> Boolean
isZero (PC k a) = isZeroK k a

-- | The element as a rational function, when it involves no radical.
toRatFun :: PC -> Maybe RatFun
toRatFun (PC KF (K a)) = Just a
toRatFun _ = Nothing

depth :: PC -> Int
depth (PC k _) = fieldDepth k

add :: PC -> PC -> PM PC
add = binary addK

sub :: PC -> PC -> PM PC
sub = binary subK

mul :: PC -> PC -> PM PC
mul = binary mulK

div :: PC -> PC -> PM PC
div x (PC k b)
  | isZeroK k b = failPM "division by zero"
  | otherwise = binary mulK x (PC k (recipK k b))

neg :: PC -> PC
neg (PC k a) = PC k (negateK k a)

-- | Exact equality as functions of the parameters.
eq :: PC -> PC -> PM Boolean
eq (PC k1 a1) (PC k2 a2) = do
  j <- joinK k1 k2
  pure (eqK j.field (j.f1 a1) (j.f2 a2))

sign :: PC -> PM Sgn
sign (PC k a) = sgnK k a

-- | The non-negative square root. The caller makes sure that the argument
-- | is non-negative on the domain.
sqrt :: PC -> PM PC
sqrt (PC k a) = do
  ms <- sqrtK k a
  pure case ms of
    Just b -> demote k b
    Nothing -> PC (Sqrt k a) (E (zeroK k) (fromBaseK k one))

-- | Some square root (of either sign): in the field when there is one, else
-- | the non-negative root adjoined. For the two intersection points F ± √Δ·d
-- | the sign does not matter.
sqrtAny :: PC -> PM PC
sqrtAny (PC k a) = do
  ms <- rootK true k a
  pure case ms of
    Just b -> demote k b
    Nothing -> PC (Sqrt k a) (E (zeroK k) (fromBaseK k one))

-- | Either a rational function, or a + b√r over a simpler field.
data PDecon = PBase RatFun | PQuad PC PC PC

deconstruct :: PC -> PDecon
deconstruct (PC k0 x0) = go k0 x0
  where
  go KF (K a) = PBase a
  go (Sqrt _ _) Zero' = PBase zero
  go (Sqrt k r) (E a b)
    | isZeroK k b = go k a
    | otherwise = PQuad (PC k a) (PC k b) (PC k r)
  go _ _ = shapeError "deconstruct"

-- | Every rational function the element is built from (coefficients and
-- | radicands), for collecting denominators and variables.
baseElements :: PC -> Array RatFun
baseElements x = case deconstruct x of
  PBase a -> [ a ]
  PQuad a b r -> baseElements a <> baseElements b <> baseElements r

-- | The elements in one common tower (the same field for all).
joinAll :: Array PC -> PM (Array PC)
joinAll xs = do
  field <- foldM (\acc (PC k _) -> _.field <$> joinWith true acc k) KF xs
  traverse
    ( \(PC k a) -> do
        j <- joinWith true field k
        pure (PC j.field (j.f2 a))
    )
    xs

-- | The radicands of the element's tower, from the bottom up, each in the
-- | field below it.
radicands :: PC -> Array PC
radicands (PC k0 _) = go k0
  where
  go KF = []
  go (Sqrt k r) = Array.snoc (go k) (PC k r)

-- | The square root of the i-th radicand (from 0, at the bottom) as an
-- | element of the field of x.
generator :: PC -> Int -> PC
generator (PC k _) i =
  let
    n = fieldDepth k
  in
    case ancestor (n - i - 1) k of
      Sqrt below _ -> PC k (liftFrom (n - i - 1) k (E (zeroK below) (fromBaseK below one)))
      KF -> shapeError "generator"

-- | The value at a rational point of the parameters (Nothing where a
-- | denominator vanishes or a radicand is negative).
evalAt :: Array (Maybe Rational) -> PC -> Maybe C.Construct
evalAt vals x = case deconstruct x of
  PBase a -> C.fromRational <$> RF.evalAt a vals
  PQuad a b r -> do
    va <- evalAt vals a
    vb <- evalAt vals b
    vr <- evalAt vals r
    s <- C.sqrt vr
    pure (va + vb * s)

-------------------------------------------------------------------------------
-- Formulas: arithmetic on computations, so that geometry reads naturally

newtype Ex = Ex (PM PC)

ex :: PC -> Ex
ex = Ex <<< pure

run :: Ex -> PM PC
run (Ex m) = m

instance Semiring Ex where
  add (Ex a) (Ex b) = Ex do
    x <- a
    y <- b
    add x y
  mul (Ex a) (Ex b) = Ex do
    x <- a
    y <- b
    mul x y
  zero = ex (fromInt 0)
  one = ex (fromInt 1)

instance Ring Ex where
  sub (Ex a) (Ex b) = Ex do
    x <- a
    y <- b
    sub x y

instance CommutativeRing Ex

instance DivisionRing Ex where
  recip (Ex a) = Ex do
    x <- a
    div (fromInt 1) x

instance EuclideanRing Ex where
  degree _ = 1
  div (Ex a) (Ex b) = Ex do
    x <- a
    y <- b
    div x y
  mod _ _ = zero
