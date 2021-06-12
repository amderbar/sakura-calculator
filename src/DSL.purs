module Macro.DSL (run, eval, showResult) where

import Prelude

import Data.Array (length)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Int (pow) as Int
import Data.Int (toNumber)
import Data.Traversable (traverse)
import Macro.DSL.Core (BiltinFunction(..), Expression(..), Numeric(..), Operator(..))
import Macro.DSL.Parser (parseDSL)
import Math (log, pow) as Math

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
  Log v -> do
    m <- eval v >>= case _ of
      Integer i -> pure (toNumber i)
      Float n -> pure n
    pure (Float $ Math.log m)

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

  pow (Integer a) (Float b) = Float (toNumber a `Math.pow` b)

  pow (Float a) (Integer b) = Float (a `Math.pow` toNumber b)

  pow (Float a) (Float b) = Float (a `Math.pow` b)

showResult :: Numeric -> String
showResult (Integer i) = show i

showResult (Float n) = show n
