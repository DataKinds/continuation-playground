module App.REPL where

import Data.Maybe
import Prelude
import Type.Proxy

import Debug (traceM)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Safe.Coerce (coerce)
import Web.DOM.Document as D
import Web.DOM.Element (Element, toNode)
import Web.DOM.Node (appendChild)
import Web.Event.Internal.Types (Event)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)
import Web.UIEvent.InputEvent as IE
import Web.UIEvent.KeyboardEvent as KE

type State = { count :: Int }

data Action
  = Increment
  | RawInput IE.InputEvent
  | RawKeyUp KE.KeyboardEvent
  | RunCode String

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { count: 0 }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }


-- replOutputElement :: Proxy "replOutputElement"
-- replOutputElement = Proxy
replOutputElement = H.RefLabel "replOutputElement"

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div_
    [ HH.p_
        [ HH.text $ "You aaaaa clicked " <> show state.count <> " times" ]
    , HH.button
        [ HE.onClick \_ -> Increment ]
        [ HH.text "Click me" ]
    , HH.input
        [ HE.onInput (IE.fromEvent >>> unsafePartial fromJust >>> RawInput)
        , HE.onKeyUp RawKeyUp
        ]
    , HH.div [ HP.ref replOutputElement ] [ ]
    ]


foreign import _setInnerHTML :: EffectFn2 Element String Unit
setInnerHTML ∷ Element → String → Effect Unit
setInnerHTML = runEffectFn2 _setInnerHTML

handleAction :: forall cs o m. MonadEffect m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Increment -> H.modify_ \st -> st { count = st.count + 1 }
  RawInput ie -> pure unit
  RawKeyUp ke -> case  KE.key ke of
    "Enter" -> handleAction $ RunCode "ok"
    _ -> traceM ke
  RunCode code -> do
    doc <- H.liftEffect (window >>= document)
    printlineElem <- H.liftEffect $ D.createElement "div" (toDocument doc)
    H.liftEffect $ setInnerHTML printlineElem "ok"
    maybeOutputElem <- H.getRef replOutputElement 
    case maybeOutputElem of
      Nothing -> pure unit
      Just outputElem -> do
        H.liftEffect $ appendChild (toNode printlineElem) (toNode outputElem)
        pure unit

