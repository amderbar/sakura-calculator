module Macro.DSL.Core where

import Prelude hiding (div, mod)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber) as Int
import Data.Show.Generic (genericShow)
import Prelude (div, mod) as Pre

-- 値
data Numeric
  = Integer Int
  | Float Number

toNumber :: Numeric -> Number
toNumber = case _ of
  Integer i -> Int.toNumber i
  Float n -> n

derive instance eqNumeric :: Eq Numeric

instance ordNumeric :: Ord Numeric where
  compare (Integer a) (Integer b) = compare a b
  compare a b = compare (toNumber a) (toNumber b)

derive instance genericNumeric :: Generic Numeric _

instance showNumeric :: Show Numeric where
  show = genericShow

instance semiringNumeric :: Semiring Numeric where
  add (Integer a) (Integer b) = Integer (a + b)
  add a b = Float (toNumber a + toNumber b)
  zero = Integer 0
  mul (Integer a) (Integer b) = Integer (a * b)
  mul a b = Float (toNumber a * toNumber b)
  one = Integer 1

instance ringNumeric :: Ring Numeric where
  sub (Integer a) (Integer b) = Integer (a - b)
  sub a b = Float (toNumber a - toNumber b)

instance commutativeRingNumeric :: CommutativeRing Numeric

instance euclideanRingNumeric :: EuclideanRing Numeric where
  degree (Integer a) = degree a
  degree (Float a) = degree a
  div (Integer a) (Integer b) = Integer (a `Pre.div` b)
  div a b = Float (toNumber a `Pre.div` toNumber b)
  mod (Integer a) (Integer b) = Integer (a `Pre.mod` b)
  mod a b = Float (toNumber a `Pre.mod` toNumber b)

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
data BiltinFunction a
  = Sum (Array a)
  | Avg (Array a)
  | Abs a
  | Sqrt a
  | Log a -- the natural logarithm

derive instance eqBiltinFunction :: Eq a => Eq (BiltinFunction a)

derive instance genericBiltinFunction :: Generic (BiltinFunction a) _

instance showBiltinFunction :: Show a => Show (BiltinFunction a) where
  show = genericShow

-- 式
data Expression
  = ValueExpr Numeric
  | FuncApplyExpr (BiltinFunction Expression)
  | BinOpExpr Operator Expression Expression

derive instance eqExpr :: Eq Expression

instance showExpression :: Show Expression where
  show (ValueExpr v) = "ValueExpr " <> show v
  show (FuncApplyExpr f) = "FuncApplyExpr " <> show f
  show (BinOpExpr o e1 e2) = "BinOpExpr " <> show o <> " " <> show e1 <> " " <> show e2

int :: Int -> Expression
int = ValueExpr <<< Integer

numeric :: Either Int Number -> Expression
numeric =
  ValueExpr
    <<< case _ of
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
