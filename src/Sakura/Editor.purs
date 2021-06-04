module Macro.Sakura.Editor
  ( Macro
  , runMacro
  , insText
  , getSelectedString
  , errorMsg
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)

foreign import data Editor :: Type

foreign import getEditor :: Effect Editor

foreign import insTextImpl :: EffectFn2 String Editor Unit

foreign import getSelectedStringImpl :: EffectFn1 Editor String

foreign import errorMsgImpl :: EffectFn2 String Editor Unit

insText :: String -> Macro Unit
insText = Macro <<< runEffectFn2 insTextImpl

getSelectedString :: Macro String
getSelectedString = Macro (runEffectFn1 getSelectedStringImpl)

errorMsg :: String -> Macro Unit
errorMsg = Macro <<< runEffectFn2 errorMsgImpl

data Macro a
  = Macro (Editor -> Effect a)

instance functorMacro :: Functor Macro where
  map g (Macro f) = Macro (\e -> g <$> f e)

-- map identity = identity
-- map (f <<< g) = map f <<< map g
instance applyMacro :: Apply Macro where
  apply (Macro g) (Macro f) = Macro (\e -> (g e) <*> (f e))

-- (<<<) <$> f <*> g <*> h = f <*> (g <*> h)
instance applicativeMacro :: Applicative Macro where
  pure a = Macro (\_ -> pure a)

-- (pure identity) <*> v = v
-- (pure f) <*> (pure x) = pure (f x)
-- u <*> (pure y) = (pure (_ $ y)) <*> u
instance bindMacro :: Bind Macro where
  bind (Macro f) g = Macro (\e -> runMacro' e =<< (g <$> (f e)))

-- (x >>= f) >>= g = x >>= (\k -> f k >>= g)
-- apply f x = f >>= \f’ -> map f’ x
instance monadMacro :: Monad Macro

runMacro' :: forall a. Editor -> Macro a -> Effect a
runMacro' editor (Macro f) = f editor

runMacro :: forall a. Macro a -> Effect a
runMacro m = getEditor >>= \e -> runMacro' e m
