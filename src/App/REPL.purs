module App.REPL
  ( Action(..)
  , State
  , component
  , handleAction
  , render
  , replInputElement
  , replOutputElement
  , setInnerHTML
  )
  where

import Data.Maybe
import Prelude
import Type.Proxy

import Debug (traceM)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lang (evaluate)
import Partial.Unsafe (unsafePartial)
import Safe.Coerce (coerce)
import Web.DOM.Document as D
import Web.DOM.Element (Element, setClassName, toNode)
import Web.DOM.Node (appendChild, textContent)
import Web.Event.Internal.Types (Event)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLInputElement (value, fromElement)
import Web.HTML.Window (document)
import Web.UIEvent.InputEvent as IE
import Web.UIEvent.KeyboardEvent as KE

type State = { count :: Int }

data Action
  = Mount
  | RawInput IE.InputEvent
  | RawKeyUp KE.KeyboardEvent
  | RunCode String

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { count: 0 }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Mount }
    }

-- replOutputElement :: Proxy "replOutputElement"
-- replOutputElement = Proxy
replOutputElement = H.RefLabel "replOutputElement"
replInputElement = H.RefLabel "replInputElement"

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div_
    [ HH.input
        [ HE.onInput (IE.fromEvent >>> unsafePartial fromJust >>> RawInput)
        , HE.onKeyUp RawKeyUp
        , HP.ref replInputElement
        , HP.class_ (ClassName "repl-input")
        ]
    , HH.div [ HP.ref replOutputElement, HP.class_ (ClassName "repl-output") ] []
    ]

foreign import _setInnerHTML :: EffectFn2 Element String Unit

setInnerHTML ∷ Element → String → Effect Unit
setInnerHTML = runEffectFn2 _setInnerHTML

handleAction :: forall cs o m. MonadEffect m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Mount -> do
    handleAction $ RunCode "\\ hello \\ world ..."
    handleAction $ RunCode "?"
  RawInput ie -> pure unit
  RawKeyUp ke -> case KE.key ke of
    "Enter" -> do
      maybeInputElem <- H.getRef replInputElement
      case maybeInputElem of
        Nothing -> pure unit
        Just inputElem -> do
          code <- liftEffect $ value (unsafePartial fromJust <<< fromElement $ inputElem)
          handleAction $ RunCode code
    _ -> traceM ke
  RunCode code -> do
    doc <- H.liftEffect (window >>= document)
    maybeOutputElem <- H.getRef replOutputElement
    let
      appendDivWithContentAndClass cl s = do
        el <- D.createElement "div" (toDocument doc)
        setClassName cl el
        setInnerHTML el s
        case maybeOutputElem of
          Nothing -> pure unit
          Just outputElem -> appendChild (toNode el) (toNode outputElem)
      log s = appendDivWithContentAndClass "output-line" s
      err e = appendDivWithContentAndClass "error-line"  (show e) 
    liftEffect (appendDivWithContentAndClass "input-line" $ "you ran: " <> code)
    liftEffect $ evaluate err log code
    pure unit
