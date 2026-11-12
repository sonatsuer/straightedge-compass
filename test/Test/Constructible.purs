module Test.Constructible (spec, Expr(..), genExpr, evalExact, evalNumber) where

import Prelude

import Constructible.Field (Construct)
import Constructible.Field as C
import Constructible.Rational as Q
import Control.Lazy (defer)
import Data.Array ((..))
import Data.Array.NonEmpty (cons')
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromJust)
import Data.Number as Number
import Effect.Aff (Aff)
import Data.Ord (abs) as Ord
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (Result, (<?>))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt, oneOf, sized)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.QuickCheck (quickCheck')

sqrt :: Construct -> Construct
sqrt x = unsafePartial (fromJust (C.sqrt x))

n :: Int -> Construct
n = C.fromInt

frac :: Int -> Int -> Construct
frac = C.fromInts

pow :: Construct -> Int -> Construct
pow x k = foldl (\acc _ -> acc * x) one (1 .. k)

-------------------------------------------------------------------------------
-- Random expressions, evaluated exactly and in floating point

data Expr
  = Lit Int Int
  | Root Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr

instance Show Expr where
  show (Lit p q) = show p <> "/" <> show q
  show (Root e) = "sqrt|" <> show e <> "|"
  show (Add a b) = "(" <> show a <> " + " <> show b <> ")"
  show (Sub a b) = "(" <> show a <> " - " <> show b <> ")"
  show (Mul a b) = "(" <> show a <> " * " <> show b <> ")"

-- | Sizes are capped: every root of an unrelated radicand adds a tower level,
-- | and merging unrelated towers costs time exponential in their depth.
genExpr :: Gen Expr
genExpr = sized (go <<< min 8)
  where
  lit = Lit <$> chooseInt (-9) 9 <*> chooseInt 1 4
  go size
    | size <= 1 = lit
    | otherwise = oneOf $ cons' lit
        [ Root <$> defer \_ -> go (size / 2)
        , Add <$> defer (\_ -> go (size / 2)) <*> defer (\_ -> go (size / 2))
        , Sub <$> defer (\_ -> go (size / 2)) <*> defer (\_ -> go (size / 2))
        , Mul <$> defer (\_ -> go (size / 2)) <*> defer (\_ -> go (size / 2))
        ]

newtype E = E Expr

instance Arbitrary E where
  arbitrary = E <$> genExpr

instance Show E where
  show (E e) = show e

-- | Square roots are taken of absolute values, so every expression is defined.
evalExact :: Expr -> Construct
evalExact = case _ of
  Lit p q -> frac p q
  Root e -> sqrt (C.abs (evalExact e))
  Add a b -> evalExact a + evalExact b
  Sub a b -> evalExact a - evalExact b
  Mul a b -> evalExact a * evalExact b

evalNumber :: Expr -> Number
evalNumber = case _ of
  Lit p q -> Q.toNumber (Q.fromInts p q)
  Root e -> Number.sqrt (Ord.abs (evalNumber e))
  Add a b -> evalNumber a + evalNumber b
  Sub a b -> evalNumber a - evalNumber b
  Mul a b -> evalNumber a * evalNumber b

-- | An upper bound for the magnitudes met while evaluating, to scale the
-- | floating-point tolerance (cancellation loses digits relative to this).
magnitude :: Expr -> Number
magnitude = case _ of
  Lit p q -> Ord.abs (Q.toNumber (Q.fromInts p q))
  Root e -> Number.sqrt (magnitude e)
  Add a b -> magnitude a + magnitude b
  Sub a b -> magnitude a + magnitude b
  Mul a b -> magnitude a * magnitude b

-------------------------------------------------------------------------------

spec :: Spec Unit
spec = describe "Constructible.Field" do

  describe "examples from the Haskell documentation" do
    it "Fibonacci numbers through the golden ratio" do
      let
        phi = (one + sqrt (n 5)) / n 2
        psi = (one - sqrt (n 5)) / n 2
      map (\k -> show (pow phi k - pow psi k)) (1 .. 10) `shouldEqual`
        [ "sqrt 5"
        , "sqrt 5"
        , "2*sqrt 5"
        , "3*sqrt 5"
        , "5*sqrt 5"
        , "8*sqrt 5"
        , "13*sqrt 5"
        , "21*sqrt 5"
        , "34*sqrt 5"
        , "55*sqrt 5"
        ]

    it "40 digits of pi by the Gauss-Legendre (AGM) iteration" do
      let
        f r =
          { a: (r.a + r.b) / n 2
          , b: sqrt (r.a * r.b)
          , t: r.t - r.p * pow ((r.a - r.b) / n 2) 2
          , p: n 2 * r.p
          }
        r4 = f (f (f (f { a: one, b: one / sqrt (n 2), t: frac 1 4, p: one })))
        piApprox = pow (r4.a + r4.b) 2 / (n 4 * r4.t)
        -- floor (piApprox * 10^40) = digits, i.e. digits <= piApprox * 10^40 < digits + 1
        digits = C.fromRational (unsafePartial (fromJust (Q.parse "31415926535897932384626433832795028841971")))
        scaled = piApprox * C.fromRational (Q.pow (Q.fromInt 10) 40)
      (digits <= scaled && scaled < digits + one) `shouldEqual` true

    it "Gauss: cos(2pi/17) from the period equations" do
      let
        qf p q =
          let
            d = sqrt (p * p - n 4 * q)
          in
            { plus: (p + d) / n 2, minus: (p - d) / n 2 }
        vw = qf (n (-1)) (n (-4))
        x = (qf vw.plus (n (-1))).plus
        y = (qf vw.minus (n (-1))).plus
        z = (qf x y).plus
      show (z / n 2) `shouldEqual`
        "-1/16 + 1/16*sqrt 17 + 1/8*sqrt (17/2 - 1/2*sqrt 17) + 1/4*sqrt (17/4 + 3/4*sqrt 17 - (3/4 + 1/4*sqrt 17)*sqrt (17/2 - 1/2*sqrt 17))"

    it "Gauss: (cos + i sin)^17 = 1 for the 17-gon angle" do
      let
        qf p q = (p + sqrt (p * p - n 4 * q)) / n 2
        qf' p q = (p - sqrt (p * p - n 4 * q)) / n 2
        v = qf (n (-1)) (n (-4))
        w = qf' (n (-1)) (n (-4))
        c = qf (qf v (n (-1))) (qf w (n (-1))) / n 2
        s = sqrt (one - c * c)
        cmul a b = { re: a.re * b.re - a.im * b.im, im: a.re * b.im + a.im * b.re }
        p17 = foldl (\acc _ -> cmul acc { re: c, im: s }) { re: one, im: zero } (1 .. 17)
      p17.re `shouldEqual` one
      p17.im `shouldEqual` zero

  -- The Haskell documentation's example (14 square roots, 12 independent
  -- primes) builds a tower of depth 12, which takes minutes; these check the
  -- same conjugation trick at small depth. Naive doubles get 5 and 7 digits.
  describe "toNumber avoids cancellation" do
    it "sqrt (10^12 + 1) - 10^6" do
      closeTo 4.99999999999875e-7 (C.toNumber (sqrt (n 1000000 * n 1000000 + one) - n 1000000))
    it "9602 - (sqrt 2 + sqrt 3)^8 = (sqrt 3 - sqrt 2)^8" do
      closeTo 1.0414497092753332e-4 (C.toNumber (n 9602 - pow (sqrt (n 2) + sqrt (n 3)) 8))
    it "sqrt 2 + sqrt 3 - sqrt (5 + 2 sqrt 6) is exactly 0" do
      C.toNumber (sqrt (n 2) + sqrt (n 3) - sqrt (n 5 + n 2 * sqrt (n 6))) `shouldEqual` 0.0

  describe "perfect squares do not grow the tower" do
    it "sqrt 8 = 2 sqrt 2" do
      sqrt (n 8) `shouldEqual` (n 2 * sqrt (n 2))
      -- The product is rational, so it drops back to Q.
      C.toRational (sqrt (n 8) * sqrt (n 2)) `shouldEqual` Just (Q.fromInt 4)
      C.depth (sqrt (n 8) * sqrt (n 2)) `shouldEqual` 0
    it "sqrt (3 + 2 sqrt 2) = 1 + sqrt 2" do
      let
        x = sqrt (n 3 + n 2 * sqrt (n 2))
      x `shouldEqual` (one + sqrt (n 2))
      C.depth x `shouldEqual` 1
    it "sqrt (7 - 4 sqrt 3) = 2 - sqrt 3 (the positive root)" do
      sqrt (n 7 - n 4 * sqrt (n 3)) `shouldEqual` (n 2 - sqrt (n 3))
    it "sqrt 2 * sqrt 3 = sqrt 6" do
      (sqrt (n 2) * sqrt (n 3)) `shouldEqual` sqrt (n 6)
      C.depth (sqrt (n 2) * sqrt (n 3)) `shouldEqual` 2
      C.depth (sqrt (n 2) * sqrt (n 3) * sqrt (n 6)) `shouldEqual` 0
    it "negative numbers have no square root" do
      (show <$> C.sqrt (n (-1))) `shouldEqual` Nothing

  describe "closed forms" do
    it "cos 72° = (sqrt 5 - 1)/4 satisfies cos 144° = 2cos² 72° - 1 = -(1 + sqrt 5)/4" do
      let
        c = (sqrt (n 5) - one) / n 4
      (n 2 * c * c - one) `shouldEqual` (negate (one + sqrt (n 5)) / n 4)
    it "2cos(2pi/17) is a root of its minimal polynomial" do
      let
        qf p q = (p + sqrt (p * p - n 4 * q)) / n 2
        qf' p q = (p - sqrt (p * p - n 4 * q)) / n 2
        v = qf (n (-1)) (n (-4))
        w = qf' (n (-1)) (n (-4))
        z = qf (qf v (n (-1))) (qf w (n (-1)))
        coeffs = [ 1, 1, -7, -6, 15, 10, -10, -4, 1 ]
        value = foldl (\acc k -> acc * z + n k) zero coeffs
      value `shouldEqual` zero

  describe "properties" do
    let
      ex (E e) = evalExact e
    it "addition is associative" $ quickCheck' 200 \a b c ->
      (ex a + ex b) + ex c == ex a + (ex b + ex c)
    it "multiplication distributes over addition" $ quickCheck' 200 \a b c ->
      ex a * (ex b + ex c) == ex a * ex b + ex a * ex c
    it "x - x = 0" $ quickCheck' 200 \a -> ex a - ex a == zero
    it "x / x = 1 for x /= 0" $ quickCheck' 200 \a ->
      let
        x = ex a
      in
        C.isZero x || x / x == one
    it "sqrt (x²) = |x|" $ quickCheck' 200 \a ->
      let
        x = ex a
      in
        sqrt (x * x) == C.abs x
    it "(sqrt |x|)² = |x|" $ quickCheck' 200 \a ->
      let
        x = C.abs (ex a)
        r = sqrt x
      in
        r * r == x
    it "toNumber agrees with floating-point evaluation" $ quickCheck' 300 \(E e) -> agrees e
    it "compare agrees with floating point when values are far apart" $ quickCheck' 300 \a b ->
      let
        x = ex a
        y = ex b
        dx = C.toNumber x
        dy = C.toNumber y
      in
        Ord.abs (dx - dy) < 1.0e-6 || compare x y == compare dx dy

closeTo :: Number -> Number -> Aff Unit
closeTo expected actual =
  when (Ord.abs (actual - expected) > 1.0e-14 * Ord.abs expected) do
    fail ("expected " <> show expected <> ", got " <> show actual)

agrees :: Expr -> Result
agrees e =
  let
    exact = C.toNumber (evalExact e)
    float = evalNumber e
    tol = 1.0e-9 * max 1.0 (magnitude e)
  in
    Ord.abs (exact - float) <= tol
      <?> ("expression " <> show e <> ": exact " <> show exact <> ", float " <> show float)
