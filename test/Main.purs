module Test.Main where

import Prelude
import Data.Either (Either(..))
import Data.Int (pow)
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Macro.DSL.Parser (parseDSL)
import Test.QuickCheck ((<?>))
import Test.QuickCheck (Result) as QuickCheck
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (suchThat)
import Test.Spec (it, describe)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

newtype NonNegative
  = NonNegative Int

derive instance newtypeNonNegative :: Newtype NonNegative _

instance arbNonNegative :: Arbitrary NonNegative where
  arbitrary = NonNegative <$> arbitrary `suchThat` (_ >= 0)

instance showNonNegative :: Show NonNegative where
  show = show <<< unwrap

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "sakura-calculator DSL Parser" do
          describe "single value" do
            it "should parse digit string to int" do
              quickCheck \(i :: Int) -> parseDSL (show i) == Right i
          describe "arithmetic operators" do
            it "should return the right total" do
              quickCheck $ propArithmeticOperator (+) \i j -> show i <> " + " <> show j
            it "should return the right difference" do
              quickCheck $ propArithmeticOperator (-) \i j -> show i <> " - " <> show j
            it "should return the right product" do
              quickCheck $ propArithmeticOperator (*) \i j -> show i <> " * " <> show j
            it "should return the right quotient" do
              quickCheck $ propArithmeticOperator (/) \i j -> show i <> " / " <> show j
            it "should return the right remainder" do
              quickCheck $ propArithmeticOperator mod \i j -> show i <> " % " <> show j
            it "should return the right power" do
              quickCheck $ propArithmeticOperator pow \i j -> show i <> " ^ " <> show j
          describe "parentheses" do
            it "should parse single value in parentheses" do
              quickCheck \(i :: Int) -> parseDSL ("(" <> show i <> ")") == Right i
            it "should parse expression in parentheses" do
              quickCheck $ propArithmeticOperator (+) \i j -> "(" <> show i <> " + " <> show j <> ")"
              quickCheck $ propArithmeticOperator (-) \i j -> "(" <> show i <> " - " <> show j <> ")"
              quickCheck $ propArithmeticOperator (*) \i j -> "(" <> show i <> " * " <> show j <> ")"
              quickCheck $ propArithmeticOperator (/) \i j -> "(" <> show i <> " / " <> show j <> ")"
              quickCheck $ propArithmeticOperator mod \i j -> "(" <> show i <> " % " <> show j <> ")"
              quickCheck $ propArithmeticOperator pow \i j -> "(" <> show i <> " ^ " <> show j <> ")"
            it "should be evaluated from inside the nested parentheses" do
              parseDSL "(1 + 2) / 3" `shouldEqual` Right 1
              parseDSL "3 / (1 + 2)" `shouldEqual` Right 1
              parseDSL "((1 + 2) / 3)" `shouldEqual` Right 1
              parseDSL "(3 / (1 + 2))" `shouldEqual` Right 1
              parseDSL "- (3 / (- (1 + 2)))" `shouldEqual` Right 1

propArithmeticOperator :: (Int -> Int -> Int) -> (NonNegative -> NonNegative -> String) -> NonNegative -> NonNegative -> QuickCheck.Result
propArithmeticOperator op mkSrc i j =
  let
    src = mkSrc i j

    actual = parseDSL src

    expected = Right (unwrap i `op` unwrap j)
  in
    actual == expected <?> (src <> " -> " <> show actual <> " ≠ " <> show expected)
