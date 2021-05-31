module Macro.Main where

import Prelude
import Effect (Effect)
import Macro.Sakura.Editor as Editor

main :: Effect Unit
main = do
  selected <- Editor.getSelectedString
  Editor.insText (selected <> "test")
