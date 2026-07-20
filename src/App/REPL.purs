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
import Data.Tuple
import Prelude
import Type.Proxy

import Control.Monad.Rec.Class (class MonadRec)
import Debug (traceM)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Game.Main as GM
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lang as L
import Partial.Unsafe (unsafePartial)
import Safe.Coerce (coerce)
import StandardLibrary.Core as SLC
import StandardLibrary.Debug as SLD
import Web.DOM.Document as D
import Web.DOM.Element (Element, setAttribute, setClassName, toNode)
import Web.DOM.Node (appendChild, textContent)
import Web.Event.Internal.Types (Event)
import Web.HTML (HTMLDocument, window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLInputElement as HIE 
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

--| Create a folding group element for storing REPL output.
--| Returns the banner element, and the details element that you should pass to appendOutput
appendOutputGroup :: HTMLDocument -> Element -> Effect (Tuple Element Element)
appendOutputGroup doc outputElem = do
  let d = toDocument doc
  detailsEl <- D.createElement "details" d
  setAttribute "open" "true" detailsEl
  summaryEl <- D.createElement "summary" d
  setClassName "input-readback" summaryEl
  setInnerHTML summaryEl "Output"
  appendChild (toNode summaryEl) (toNode detailsEl) 
  appendChild (toNode detailsEl) (toNode outputElem) 
  pure $ Tuple summaryEl detailsEl

--| Appends output to a given element
appendOutput :: HTMLDocument -> Element -> String -> Effect Unit
appendOutput doc detailsEl rawOutput = do
  let d = toDocument doc
  pEl <- D.createElement "p" d
  setInnerHTML pEl rawOutput
  appendChild (toNode pEl) (toNode detailsEl) 

liftVM :: forall a s o m. MonadAff m => RealEval a -> H.HalogenM State Action s o m Unit
liftVM vmA = do
  { vmState } <- H.get
  newVmState <- H.liftAff $ L.execVMAff vmA vmState
  H.modify_ _ { vmState = newVmState }


--| Action which initializes the VM: load the standard library and transfer execution to the game
initVM :: forall s o m. MonadAff m => H.HalogenM State Action s o m Unit
initVM = do
  -- Load the stdlib
  liftVM $ do 
    SLC.gainKnowledge
    SLD.gainDebugKnowledge
  -- Run the game's entrypoint
  handleAction $ RunCode "?"
  handleAction $ RunCode GM.program


handleAction :: forall s o m. MonadAff m => Action → H.HalogenM State Action s o m Unit
handleAction action = do
  doc <- H.liftEffect (window >>= document)
  maybeOutputElem <- H.getRef replOutputElement
  maybeInputElem <- H.getRef replInputElement

  case [ maybeOutputElem, maybeInputElem ] of
    [ Just outputElem, Just inputElem ] ->
        case action of
          Mount -> initVM
          RawInput ie -> pure unit
          RawKeyUp ke -> case KE.key ke of
            "Enter" -> do
              let hInputElem = unsafePartial fromJust <<< HIE.fromElement $ inputElem
              code <- liftEffect $ HIE.value hInputElem
              handleAction $ RunCode code
              liftEffect $ HIE.select hInputElem
            _ -> traceM ke
          StandardOutput rawHtml -> pure unit
          StandardError rawHtml -> pure unit
          RunCode code -> do
            Tuple readbackEl outputGroupEl <- liftEffect (appendOutputGroup doc outputElem)
            liftEffect $ setInnerHTML readbackEl ("💞💖💕  " <> code)
            let
              groupLog s = do
                traceM $ "Got output from language:" <> s 
                appendOutput doc outputGroupEl s
              groupErr e = do
                traceM e
                appendOutput doc outputGroupEl (show e)
            liftVM do
              L.setErrhandler groupErr
              L.setLogger groupLog
              L.vmAction code
    _ -> pure unit
