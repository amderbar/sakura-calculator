module Macro.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Macro.DSL as DSL
import Macro.Sakura.Editor as Editor

main :: Effect Unit
main = Editor.runMacro do
  selected <- Editor.getSelectedString
  case DSL.run selected of
    Right ret -> Editor.insText (showValue ret)
    Left err -> Editor.errorMsg err
  where
  showValue :: DSL.Value -> String
  showValue (DSL.IntValue i) = show i
  showValue (DSL.FloatValue n) = show n
