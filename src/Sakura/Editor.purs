module Macro.Sakura.Editor
  ( Editor
  , Macro
  , runMacro
  , insText
  , getSelectedString
  , errorMsg
  ) where

import Prelude
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)

foreign import data Editor :: Type

foreign import getEditor :: Effect Editor

foreign import insTextImpl :: EffectFn2 Editor String Unit

foreign import getSelectedStringImpl :: EffectFn1 Editor String

foreign import errorMsgImpl :: EffectFn2 Editor String Unit

insText :: String -> Macro Unit
insText txt = ask >>= \e -> lift (runEffectFn2 insTextImpl e txt)

getSelectedString :: Macro String
getSelectedString = ask >>= lift <<< runEffectFn1 getSelectedStringImpl

errorMsg :: String -> Macro Unit
errorMsg msg = ask >>= \e -> lift (runEffectFn2 errorMsgImpl e msg)

type Macro a = ReaderT Editor Effect a

runMacro :: forall a. Macro a -> Effect a
runMacro m = getEditor >>= runReaderT m
