module Macro.DSL where

import Prelude
import Data.Either (Either(..))
import Data.Int (pow)
import Macro.DSL.Core (Expression(..), Operator(..), Value(..))
import Macro.DSL.Parser (parseDSL)
import Text.Parsing.Parser (parseErrorMessage)

run :: String -> Either String Int
run src = do
  ast <- case parseDSL src of
    Right ret -> pure ret
    Left err -> Left (parseErrorMessage err)
  eval ast

eval :: Expression -> Either String Int
eval (ValueExpr (IntValue i)) = pure i

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
