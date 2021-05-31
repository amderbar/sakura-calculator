module Macro.Sakura.Editor where

import Prelude (Unit)
import Effect (Effect)

foreign import insText :: String -> Effect Unit

foreign import getSelectedString :: Effect String
