module Macro.DSL.Core where

import Prelude hiding (div, mod)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

-- 値
data Value
  = IntValue Int

derive instance eqValue :: Eq Value

derive instance genericValue :: Generic Value _

instance showValue :: Show Value where
  show = genericShow

-- 演算子
data Operator
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | Pow

derive instance eqOperator :: Eq Operator

derive instance genericOperator :: Generic Operator _

instance showOperator :: Show Operator where
  show = genericShow

-- 式
data Expression
  = ValueExpr Value
  | BinOpExpr Operator Expression Expression

derive instance eqExpr :: Eq Expression

instance showExpression :: Show Expression where
  show (ValueExpr v) = "ValueExpr " <> show v
  show (BinOpExpr o e1 e2) = "BinOpExpr " <> show o <> " " <> show e1 <> " " <> show e2

int :: Int -> Expression
int = ValueExpr <<< IntValue

add :: Expression -> Expression -> Expression
add = BinOpExpr Add

sub :: Expression -> Expression -> Expression
sub = BinOpExpr Sub

mul :: Expression -> Expression -> Expression
mul = BinOpExpr Mul

div :: Expression -> Expression -> Expression
div = BinOpExpr Div

mod :: Expression -> Expression -> Expression
mod = BinOpExpr Mod

pow :: Expression -> Expression -> Expression
pow = BinOpExpr Pow
