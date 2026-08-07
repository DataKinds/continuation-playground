module App.Mutators
  ( setInnerHTML
  )
  where

import Data.Unit (Unit)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Web.DOM (Element)

foreign import _setInnerHTML :: EffectFn2 Element String Unit
setInnerHTML ∷ Element → String → Effect Unit
setInnerHTML = runEffectFn2 _setInnerHTML
