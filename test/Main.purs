module Test.Main where

import Prelude
import Data.Either (Either(..))
import Data.Int (pow) as Int
import Data.Newtype (class Newtype, unwrap)
import Data.Number.Approximate ((~=))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Macro.DSL (run) as DSL
import Macro.DSL.Core (Numeric(..)) as DSL
import Math (log, pow) as Math
import Test.QuickCheck ((<?>))
import Test.QuickCheck (Result) as QuickCheck
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (suchThat)
import Test.Spec (it, describe)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

newtype NonNegative a
  = NonNegative a

instance newtypeNonNegative :: Newtype (NonNegative a) a

instance arbNonNegative :: (Ord a, Semiring a, Arbitrary a) => Arbitrary (NonNegative a) where
  arbitrary = NonNegative <$> arbitrary `suchThat` (_ >= zero)

instance showNonNegative :: Show a => Show (NonNegative a) where
  show = show <<< unwrap

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "sakura-calculator DSL Parser" do
          describe "single value" do
            it "should parse digit string to int" do
              quickCheck \(i :: Int) -> DSL.run (show i) == Right (DSL.Integer i)
            it "should parse digit and dot string to float" do
              quickCheck \(i :: Number) -> DSL.run (show i) `eq'` Right (DSL.Float i)
          describe "arithmetic operators" do
            it "should return the right total" do
              quickCheck $ propArithmeticOperator DSL.Integer (+) \i j -> show i <> " + " <> show j
              quickCheck $ propArithmeticOperator DSL.Float (+) \i j -> show i <> " + " <> show j
            it "should return the right difference" do
              quickCheck $ propArithmeticOperator DSL.Integer (-) \i j -> show i <> " - " <> show j
              quickCheck $ propArithmeticOperator DSL.Float (-) \i j -> show i <> " - " <> show j
            it "should return the right product" do
              quickCheck $ propArithmeticOperator DSL.Integer (*) \i j -> show i <> " * " <> show j
              quickCheck $ propArithmeticOperator DSL.Float (*) \i j -> show i <> " * " <> show j
            it "should return the right quotient" do
              quickCheck $ propArithmeticOperator DSL.Integer (/) \i j -> show i <> " / " <> show j
              quickCheck $ propArithmeticOperator DSL.Float (/) \i j -> show i <> " / " <> show j
            it "should return the right remainder" do
              quickCheck $ propArithmeticOperator DSL.Integer mod \i j -> show i <> " % " <> show j
              quickCheck $ propArithmeticOperator DSL.Float mod \i j -> show i <> " % " <> show j
            it "should return the right power" do
              quickCheck $ propArithmeticOperator DSL.Integer Int.pow \i j -> show i <> " ^ " <> show j
              quickCheck $ propArithmeticOperator DSL.Float Math.pow \i j -> show i <> " ^ " <> show j
          describe "parentheses" do
            it "should parse single value in parentheses" do
              quickCheck \(i :: Int) -> DSL.run ("(" <> show i <> ")") == Right (DSL.Integer i)
              quickCheck \(i :: Number) -> DSL.run ("(" <> show i <> ")") `eq'` Right (DSL.Float i)
            it "should parse expression in parentheses" do
              quickCheck $ propArithmeticOperator DSL.Integer (+) \i j -> "(" <> show i <> " + " <> show j <> ")"
              quickCheck $ propArithmeticOperator DSL.Integer (-) \i j -> "(" <> show i <> " - " <> show j <> ")"
              quickCheck $ propArithmeticOperator DSL.Integer (*) \i j -> "(" <> show i <> " * " <> show j <> ")"
              quickCheck $ propArithmeticOperator DSL.Integer (/) \i j -> "(" <> show i <> " / " <> show j <> ")"
              quickCheck $ propArithmeticOperator DSL.Integer mod \i j -> "(" <> show i <> " % " <> show j <> ")"
              quickCheck $ propArithmeticOperator DSL.Integer Int.pow \i j -> "(" <> show i <> " ^ " <> show j <> ")"
              quickCheck $ propArithmeticOperator DSL.Float (+) \i j -> "(" <> show i <> " + " <> show j <> ")"
              quickCheck $ propArithmeticOperator DSL.Float (-) \i j -> "(" <> show i <> " - " <> show j <> ")"
              quickCheck $ propArithmeticOperator DSL.Float (*) \i j -> "(" <> show i <> " * " <> show j <> ")"
              quickCheck $ propArithmeticOperator DSL.Float (/) \i j -> "(" <> show i <> " / " <> show j <> ")"
              quickCheck $ propArithmeticOperator DSL.Float mod \i j -> "(" <> show i <> " % " <> show j <> ")"
              quickCheck $ propArithmeticOperator DSL.Float Math.pow \i j -> "(" <> show i <> " ^ " <> show j <> ")"
            it "should be evaluated from inside the nested parentheses" do
              DSL.run "(1 + 2) / 3" `shouldEqual` Right (DSL.Integer 1)
              DSL.run "3 / (1 + 2)" `shouldEqual` Right (DSL.Integer 1)
              DSL.run "((1 + 2) / 3)" `shouldEqual` Right (DSL.Integer 1)
              DSL.run "(3 / (1 + 2))" `shouldEqual` Right (DSL.Integer 1)
              DSL.run "- (3 / (- (1 + 2)))" `shouldEqual` Right (DSL.Integer 1)
              DSL.run "(1.0 + 2.0) / 3.0" `shouldEqual` Right (DSL.Float 1.0)
              DSL.run "3.0 / (1.0 + 2.0)" `shouldEqual` Right (DSL.Float 1.0)
              DSL.run "((1.0 + 2.0) / 3.0)" `shouldEqual` Right (DSL.Float 1.0)
              DSL.run "(3.0 / (1.0 + 2.0))" `shouldEqual` Right (DSL.Float 1.0)
              DSL.run "- (3.0 / (- (1.0 + 2.0)))" `shouldEqual` Right (DSL.Float 1.0)
          describe "Polymorphism of numeric literal" do
            it "should return float as a result of integer and float" do
              DSL.run "(-1) + 2.0" `shouldEqual` Right (DSL.Float 1.0)
              DSL.run "3.0 - 2" `shouldEqual` Right (DSL.Float 1.0)
              DSL.run "2 / 2.0" `shouldEqual` Right (DSL.Float 1.0)
              DSL.run "1.0 * 1" `shouldEqual` Right (DSL.Float 1.0)
          describe "built-in functions" do
            it "should return the right absolute value" do
              DSL.run "abs 1" `shouldEqual` Right (DSL.Integer 1)
              DSL.run "abs 1.0" `shouldEqual` Right (DSL.Float 1.0)
              DSL.run "abs (-1)" `shouldEqual` Right (DSL.Integer 1)
              DSL.run "abs (-1.0)" `shouldEqual` Right (DSL.Float 1.0)
              DSL.run "abs ((-1) + 3)" `shouldEqual` Right (DSL.Integer 2)
              DSL.run "abs (2 - 1.0)" `shouldEqual` Right (DSL.Float 1.0)
            it "should return the right square root" do
              DSL.run "sqrt 9" `shouldEqual` Right (DSL.Float 3.0)
              DSL.run "sqrt 9.0" `shouldEqual` Right (DSL.Float 3.0)
              DSL.run "sqrt (4 + 5)" `shouldEqual` Right (DSL.Float 3.0)
              DSL.run "sqrt (3 * 3)" `shouldEqual` Right (DSL.Float 3.0)
            it "should return the right logarithm" do
              DSL.run "log 1" `shouldEqual` Right (DSL.Float (Math.log 1.0))
              DSL.run "log 1.0" `shouldEqual` Right (DSL.Float (Math.log 1.0))
              DSL.run "log (2 - 1)" `shouldEqual` Right (DSL.Float (Math.log 1.0))
              DSL.run "log (3 / 3)" `shouldEqual` Right (DSL.Float (Math.log 1.0))
              DSL.run "log ((-1) + 2)" `shouldEqual` Right (DSL.Float (Math.log 1.0))
          describe "built-in array aggrigation functions" do
            it "should return the right total" do
              DSL.run "sum [1, 2, 3]" `shouldEqual` Right (DSL.Integer 6)
              DSL.run "sum [1, 2.0, 3]" `shouldEqual` Right (DSL.Float 6.0)
              DSL.run "sum [1.0, 2.0, 3.0]" `shouldEqual` Right (DSL.Float 6.0)
              DSL.run "sum [1, 2.0, 1 + 2]" `shouldEqual` Right (DSL.Float 6.0)
            it "should return the right average" do
              DSL.run "avg [1, 2, 3]" `shouldEqual` Right (DSL.Integer 2)
              DSL.run "avg [1, 2.0, 3]" `shouldEqual` Right (DSL.Float 2.0)
              DSL.run "avg [1.0, 2.0, 3.0]" `shouldEqual` Right (DSL.Float 2.0)
              DSL.run "avg [1, 2.0, 1 + 2]" `shouldEqual` Right (DSL.Float 2.0)

propArithmeticOperator ::
  forall a.
  EuclideanRing a =>
  (a -> DSL.Numeric) ->
  (a -> a -> a) ->
  (NonNegative a -> NonNegative a -> String) ->
  NonNegative a ->
  NonNegative a ->
  QuickCheck.Result
propArithmeticOperator valType op mkSrc i j =
  let
    src = mkSrc i j

    actual = DSL.run src

    expected = Right $ valType (unwrap i `op` unwrap j)
  in
    actual `eq'` expected <?> (src <> " -> " <> show actual <> " ≠ " <> show expected)

class Eq' a where
  eq' :: a -> a -> Boolean

instance eqValue' :: Eq' DSL.Numeric where
  eq' (DSL.Integer i) (DSL.Integer j) = i == j
  eq' (DSL.Float i) (DSL.Float j) = i ~= j
  eq' _ _ = false

instance eqString' :: Eq' String where
  eq' = (==)

instance eqEither' :: (Eq' a, Eq' b) => Eq' (Either a b) where
  eq' (Left i) (Left j) = i `eq'` j
  eq' (Right i) (Right j) = i `eq'` j
  eq' _ _ = false
