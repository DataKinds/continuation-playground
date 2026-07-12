module App.REPL
  ( Action(..)
  , State
  , component
  , handleAction
  , render
  , replInputElement
  , replOutputElement
  , setInnerHTML
  ) where

import Common
import Data.Maybe
import Prelude
import Type.Proxy

import Control.Monad.Rec.Class (class MonadRec)
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
import Lang (execVMAff, initialDebugVMAction, vmAction)
import Lang as L
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

type State = { vmState :: RealState }

initialState = \_ -> { vmState: L.emptyRealState }

data Action
  = Mount
  | RawInput IE.InputEvent
  | RawKeyUp KE.KeyboardEvent
  | RunCode String
  | StandardOutput String
  | StandardError String

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Mount }
    }

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

appendDivWithContentAndClass doc outputElem cl s = do
  el <- D.createElement "div" (toDocument doc)
  setClassName cl el
  setInnerHTML el s
  appendChild (toNode el) (toNode outputElem)

liftVM :: forall a s o m. MonadAff m => RealEval a -> H.HalogenM State Action s o m Unit
liftVM vmA = do
  { vmState } <- H.get
  newVmState <- H.liftAff $ execVMAff vmA vmState
  H.modify_ _ { vmState = newVmState }

handleAction :: forall s o m. MonadAff m => Action → H.HalogenM State Action s o m Unit
handleAction action = do
  doc <- H.liftEffect (window >>= document)
  maybeOutputElem <- H.getRef replOutputElement
  maybeInputElem <- H.getRef replInputElement

  case [ maybeOutputElem, maybeInputElem ] of
    [ Just outputElem, Just inputElem ] ->
      let
        appendElem = appendDivWithContentAndClass doc outputElem
        log s = appendElem "output-line" s
        err e = appendElem "error-line" (show e)
      in
        case action of
          Mount -> do
            -- Set up the VM
            liftVM $ initialDebugVMAction err log
            handleAction $ RunCode "\\ hello \\ world ..."
            handleAction $ RunCode "?"
          RawInput ie -> pure unit
          RawKeyUp ke -> case KE.key ke of
            "Enter" -> do
              code <- liftEffect $ value (unsafePartial fromJust <<< fromElement $ inputElem)
              handleAction $ RunCode code
            _ -> traceM ke
          StandardOutput rawHtml -> pure unit
          StandardError rawHtml -> pure unit
          RunCode code -> do
            liftEffect (appendElem "input-line" $ "you ran: " <> code)
            liftVM (vmAction code) 
    _ -> pure unit
