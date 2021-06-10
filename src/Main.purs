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
    Right ret -> Editor.insText (DSL.showResult ret)
    Left err -> Editor.errorMsg err
