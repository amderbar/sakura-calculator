module Macro.DSL (run, eval, showResult) where

import Prelude
import Data.Array (length)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Int (pow) as Int
import Data.Ord (abs)
import Data.Traversable (traverse)
import Macro.DSL.Core (toNumber, BiltinFunction(..), Expression(..), Numeric(..), Operator(..))
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
  Sqrt v -> floatFunc Math.sqrt v
  Log v -> floatFunc Math.log v
  where
  floatFunc :: (Number -> Number) -> Expression -> Either String Numeric
  floatFunc func e = do
    m <- toNumber <$> eval e
    pure (Float $ func m)

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
showResult (Integer i) = show i

showResult (Float n) = show n
