module Macro.DSL where

import Prelude
import Data.Either (Either(..))
import Text.Parsing.Parser (parseErrorMessage)
import Macro.DSL.Parser (parseDSL)

eval :: String -> Either String String
eval src = case parseDSL src of
  Right ret -> pure (show ret)
  Left err -> Left (parseErrorMessage err)
