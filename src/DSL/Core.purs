module Macro.DSL.Core where

import Prelude hiding (div, mod)

import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.Show.Generic (genericShow)
import Prelude (div, mod) as Pre

-- 値
data Value
  = IntValue Int
  | FloatValue Number

derive instance eqValue :: Eq Value

derive instance genericValue :: Generic Value _

instance showValue :: Show Value where
  show = genericShow

instance semiringValue :: Semiring Value where
  add (IntValue a) (IntValue b) = IntValue (a + b)
  add (IntValue a) (FloatValue b) = FloatValue (toNumber a + b)
  add (FloatValue a) (IntValue b) = FloatValue (a + toNumber b)
  add (FloatValue a) (FloatValue b) = FloatValue (a + b)

  zero = IntValue 0

  mul (IntValue a) (IntValue b) = IntValue (a * b)
  mul (IntValue a) (FloatValue b) = FloatValue (toNumber a * b)
  mul (FloatValue a) (IntValue b) = FloatValue (a * toNumber b)
  mul (FloatValue a) (FloatValue b) = FloatValue (a * b)

  one = IntValue 1

instance ringValue :: Ring Value where
  sub (IntValue a) (IntValue b) = IntValue (a - b)
  sub (IntValue a) (FloatValue b) = FloatValue (toNumber a - b)
  sub (FloatValue a) (IntValue b) = FloatValue (a - toNumber b)
  sub (FloatValue a) (FloatValue b) = FloatValue (a - b)

instance commutativeRingValue :: CommutativeRing Value

instance euclideanRingValue :: EuclideanRing Value where
  degree (IntValue a) = degree a
  degree (FloatValue a) = degree a

  div (IntValue a) (IntValue b) = IntValue (a `Pre.div` b)
  div (IntValue a) (FloatValue b) = FloatValue (toNumber a `Pre.div` b)
  div (FloatValue a) (IntValue b) = FloatValue (a `Pre.div` toNumber b)
  div (FloatValue a) (FloatValue b) = FloatValue (a `Pre.div` b)

  mod (IntValue a) (IntValue b) = IntValue (a `Pre.mod` b)
  mod (IntValue a) (FloatValue b) = FloatValue (toNumber a `Pre.mod` b)
  mod (FloatValue a) (IntValue b) = FloatValue (a `Pre.mod` toNumber b)
  mod (FloatValue a) (FloatValue b) = FloatValue (a `Pre.mod` b)

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

float :: Number -> Expression
float = ValueExpr <<< FloatValue

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
