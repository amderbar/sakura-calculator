module Macro.DSL (run, eval, showResult) where

import Prelude
import Data.Array (length)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Int (ceil, floor, pow, round) as Int
import Data.Ord (abs)
import Data.Traversable (traverse)
import Macro.DSL.Core (BiltinFunction(..), Expression(..), Numeric(..), Operator(..), toNumber)
import Macro.DSL.Parser (parseDSL)
import Math (log, pow, sqrt) as Math

run :: String -> Either String Numeric
run src = do
  ast <- case parseDSL src of
    Right ret -> pure ret
    Left err -> Left (show err)
  eval ast

eval :: Expression -> Either String Numeric
eval (ValueExpr v) = pure v

eval (FuncApplyExpr f) = case f of
  Sum v -> sum <$> traverse eval v
  Avg v -> do
    total <- eval (FuncApplyExpr (Sum v))
    pure $ total / (Integer $ length v)
  Abs v -> abs <$> eval v
  Round v -> intFunc Int.round <$> eval v
  Floor v -> intFunc Int.floor <$> eval v
  Ceil v -> intFunc Int.ceil <$> eval v
  Sqrt v -> floatFunc Math.sqrt <$> eval v
  Log v -> floatFunc Math.log <$> eval v
  where
  intFunc :: (Number -> Int) -> Numeric -> Numeric
  intFunc func = case _ of
    n@(Integer _) -> n
    (Float n) -> Integer (func n)

  floatFunc :: (Number -> Number) -> Numeric -> Numeric
  floatFunc func = Float <<< func <<< toNumber

eval (BinOpExpr op expr1 expr2) = do
  ret1 <- eval expr1
  ret2 <- eval expr2
  pure (ret1 `o` ret2)
  where
  o = case op of
    Add -> (+)
    Sub -> (-)
    Mul -> (*)
    Div -> div
    Mod -> mod
    Pow -> pow

  pow (Integer a) (Integer b) = Integer (a `Int.pow` b)

  pow a b = Float (toNumber a `Math.pow` toNumber b)

showResult :: Numeric -> String
showResult = case _ of
  (Integer i) -> show i
  (Float n) -> show n
