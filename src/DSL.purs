module Macro.DSL (run, eval, module Macro.DSL.Core) where

import Prelude
import Data.Either (Either(..))
import Data.Int (pow) as Int
import Data.Int (toNumber)
import Macro.DSL.Core (Expression(..), Operator(..), Value(..))
import Macro.DSL.Parser (parseDSL)
import Math (pow) as Math
import Text.Parsing.Parser (parseErrorMessage)

run :: String -> Either String Value
run src = do
  ast <- case parseDSL src of
    Right ret -> pure ret
    Left err -> Left (parseErrorMessage err)
  eval ast

eval :: Expression -> Either String Value
eval (ValueExpr v) = pure v

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
  pow (IntValue a) (IntValue b) = IntValue (a `Int.pow` b)
  pow (IntValue a) (FloatValue b) = FloatValue (toNumber a `Math.pow` b)
  pow (FloatValue a) (IntValue b) = FloatValue (a `Math.pow` toNumber b)
  pow (FloatValue a) (FloatValue b) = FloatValue (a `Math.pow` b)
