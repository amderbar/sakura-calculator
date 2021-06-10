module Macro.DSL.Core where

import Prelude hiding (div, mod)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.Show.Generic (genericShow)
import Prelude (div, mod) as Pre

-- 値
data Numeric
  = Integer Int
  | Float Number

derive instance eqNumeric :: Eq Numeric

derive instance genericNumeric :: Generic Numeric _

instance showNumeric :: Show Numeric where
  show = genericShow

instance semiringNumeric :: Semiring Numeric where
  add (Integer a) (Integer b) = Integer (a + b)
  add (Integer a) (Float b) = Float (toNumber a + b)
  add (Float a) (Integer b) = Float (a + toNumber b)
  add (Float a) (Float b) = Float (a + b)
  zero = Integer 0
  mul (Integer a) (Integer b) = Integer (a * b)
  mul (Integer a) (Float b) = Float (toNumber a * b)
  mul (Float a) (Integer b) = Float (a * toNumber b)
  mul (Float a) (Float b) = Float (a * b)
  one = Integer 1

instance ringNumeric :: Ring Numeric where
  sub (Integer a) (Integer b) = Integer (a - b)
  sub (Integer a) (Float b) = Float (toNumber a - b)
  sub (Float a) (Integer b) = Float (a - toNumber b)
  sub (Float a) (Float b) = Float (a - b)

instance commutativeRingNumeric :: CommutativeRing Numeric

instance euclideanRingNumeric :: EuclideanRing Numeric where
  degree (Integer a) = degree a
  degree (Float a) = degree a
  div (Integer a) (Integer b) = Integer (a `Pre.div` b)
  div (Integer a) (Float b) = Float (toNumber a `Pre.div` b)
  div (Float a) (Integer b) = Float (a `Pre.div` toNumber b)
  div (Float a) (Float b) = Float (a `Pre.div` b)
  mod (Integer a) (Integer b) = Integer (a `Pre.mod` b)
  mod (Integer a) (Float b) = Float (toNumber a `Pre.mod` b)
  mod (Float a) (Integer b) = Float (a `Pre.mod` toNumber b)
  mod (Float a) (Float b) = Float (a `Pre.mod` b)

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

-- 組み込み関数
data BiltinFunction
  = Sum
  | Avg

derive instance eqBiltinFunction :: Eq BiltinFunction

derive instance genericBiltinFunction :: Generic BiltinFunction _

instance showBiltinFunction :: Show BiltinFunction where
  show = genericShow

-- 式
data Expression
  = ValueExpr Numeric
  | AggExpr BiltinFunction (Array Expression)
  | BinOpExpr Operator Expression Expression

derive instance eqExpr :: Eq Expression

instance showExpression :: Show Expression where
  show (ValueExpr v) = "ValueExpr " <> show v
  show (AggExpr f v) = "AggExpr " <> show f <> " " <> show v
  show (BinOpExpr o e1 e2) = "BinOpExpr " <> show o <> " " <> show e1 <> " " <> show e2

int :: Int -> Expression
int = ValueExpr <<< Integer

numeric :: Either Int Number -> Expression
numeric = ValueExpr <<< case _ of
  Left i -> Integer i
  Right n -> Float n

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
