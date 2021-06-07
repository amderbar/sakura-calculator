module Test.Main where

import Prelude
import Data.Either (Either(..))
import Data.Int (pow) as Int
import Data.Newtype (class Newtype, unwrap)
import Data.Number.Approximate ((~=))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Macro.DSL (Value(..), run) as DSL
import Math (pow) as Math
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
              quickCheck \(i :: Int) -> DSL.run (show i) == Right (DSL.IntValue i)
            it "should parse digit and dot string to float" do
              quickCheck \(i :: Number) -> DSL.run (show i) `eq'` Right (DSL.FloatValue i)
          describe "arithmetic operators" do
            it "should return the right total" do
              quickCheck $ propArithmeticOperator DSL.IntValue (+) \i j -> show i <> " + " <> show j
              quickCheck $ propArithmeticOperator DSL.FloatValue (+) \i j -> show i <> " + " <> show j
            it "should return the right difference" do
              quickCheck $ propArithmeticOperator DSL.IntValue (-) \i j -> show i <> " - " <> show j
              quickCheck $ propArithmeticOperator DSL.FloatValue (-) \i j -> show i <> " - " <> show j
            it "should return the right product" do
              quickCheck $ propArithmeticOperator DSL.IntValue (*) \i j -> show i <> " * " <> show j
              quickCheck $ propArithmeticOperator DSL.FloatValue (*) \i j -> show i <> " * " <> show j
            it "should return the right quotient" do
              quickCheck $ propArithmeticOperator DSL.IntValue (/) \i j -> show i <> " / " <> show j
              quickCheck $ propArithmeticOperator DSL.FloatValue (/) \i j -> show i <> " / " <> show j
            it "should return the right remainder" do
              quickCheck $ propArithmeticOperator DSL.IntValue mod \i j -> show i <> " % " <> show j
              quickCheck $ propArithmeticOperator DSL.FloatValue mod \i j -> show i <> " % " <> show j
            it "should return the right power" do
              quickCheck $ propArithmeticOperator DSL.IntValue Int.pow \i j -> show i <> " ^ " <> show j
              quickCheck $ propArithmeticOperator DSL.FloatValue Math.pow \i j -> show i <> " ^ " <> show j
          describe "parentheses" do
            it "should parse single value in parentheses" do
              quickCheck \(i :: Int) -> DSL.run ("(" <> show i <> ")") == Right (DSL.IntValue i)
              quickCheck \(i :: Number) -> DSL.run ("(" <> show i <> ")") `eq'` Right (DSL.FloatValue i)
            it "should parse expression in parentheses" do
              quickCheck $ propArithmeticOperator DSL.IntValue (+) \i j -> "(" <> show i <> " + " <> show j <> ")"
              quickCheck $ propArithmeticOperator DSL.IntValue (-) \i j -> "(" <> show i <> " - " <> show j <> ")"
              quickCheck $ propArithmeticOperator DSL.IntValue (*) \i j -> "(" <> show i <> " * " <> show j <> ")"
              quickCheck $ propArithmeticOperator DSL.IntValue (/) \i j -> "(" <> show i <> " / " <> show j <> ")"
              quickCheck $ propArithmeticOperator DSL.IntValue mod \i j -> "(" <> show i <> " % " <> show j <> ")"
              quickCheck $ propArithmeticOperator DSL.IntValue Int.pow \i j -> "(" <> show i <> " ^ " <> show j <> ")"
              quickCheck $ propArithmeticOperator DSL.FloatValue (+) \i j -> "(" <> show i <> " + " <> show j <> ")"
              quickCheck $ propArithmeticOperator DSL.FloatValue (-) \i j -> "(" <> show i <> " - " <> show j <> ")"
              quickCheck $ propArithmeticOperator DSL.FloatValue (*) \i j -> "(" <> show i <> " * " <> show j <> ")"
              quickCheck $ propArithmeticOperator DSL.FloatValue (/) \i j -> "(" <> show i <> " / " <> show j <> ")"
              quickCheck $ propArithmeticOperator DSL.FloatValue mod \i j -> "(" <> show i <> " % " <> show j <> ")"
              quickCheck $ propArithmeticOperator DSL.FloatValue Math.pow \i j -> "(" <> show i <> " ^ " <> show j <> ")"
            it "should be evaluated from inside the nested parentheses" do
              DSL.run "(1 + 2) / 3" `shouldEqual` Right (DSL.IntValue 1)
              DSL.run "3 / (1 + 2)" `shouldEqual` Right (DSL.IntValue 1)
              DSL.run "((1 + 2) / 3)" `shouldEqual` Right (DSL.IntValue 1)
              DSL.run "(3 / (1 + 2))" `shouldEqual` Right (DSL.IntValue 1)
              DSL.run "- (3 / (- (1 + 2)))" `shouldEqual` Right (DSL.IntValue 1)
              DSL.run "(1.0 + 2.0) / 3.0" `shouldEqual` Right (DSL.FloatValue 1.0)
              DSL.run "3.0 / (1.0 + 2.0)" `shouldEqual` Right (DSL.FloatValue 1.0)
              DSL.run "((1.0 + 2.0) / 3.0)" `shouldEqual` Right (DSL.FloatValue 1.0)
              DSL.run "(3.0 / (1.0 + 2.0))" `shouldEqual` Right (DSL.FloatValue 1.0)
              DSL.run "- (3.0 / (- (1.0 + 2.0)))" `shouldEqual` Right (DSL.FloatValue 1.0)
          describe "Polymorphism of numeric literal" do
            it "should return float as a result of integer and float" do
              DSL.run "(-1) + 2.0" `shouldEqual` Right (DSL.FloatValue 1.0)
              DSL.run "3.0 - 2" `shouldEqual` Right (DSL.FloatValue 1.0)
              DSL.run "2 / 2.0" `shouldEqual` Right (DSL.FloatValue 1.0)
              DSL.run "1.0 * 1" `shouldEqual` Right (DSL.FloatValue 1.0)

propArithmeticOperator ::
  forall a.
  EuclideanRing a =>
  (a -> DSL.Value) ->
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

instance eqValue' :: Eq' DSL.Value where
  eq' (DSL.IntValue i) (DSL.IntValue j) = i == j
  eq' (DSL.FloatValue i) (DSL.FloatValue j) = i ~= j
  eq' _ _ = false

instance eqString' :: Eq' String where
  eq' = (==)

instance eqEither' :: (Eq' a, Eq' b) => Eq' (Either a b) where
  eq' (Left i) (Left j) = i `eq'` j
  eq' (Right i) (Right j) = i `eq'` j
  eq' _ _ = false
