module App.REPL
  ( Action(..)
  , State
  , component
  , handleAction
  , render
  , replInputElement
  , replOutputElement
  ) where

import App.Mutators
import Common
import Data.Maybe
import Data.Tuple
import Prelude
import Type.Proxy

import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array (uncons)
import Data.String (Pattern(..), split)
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
import StandardLibrary.IO as SLIO
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

-- Primitives for appending different types of boxes to the REPL scratch area
getDoc :: forall m. MonadEffect m => m HTMLDocument
getDoc = H.liftEffect (window >>= document)

createChildElem :: String -> Element -> Effect Element
createChildElem name parent = do
  doc <- toDocument <$> getDoc -- TODO: any reason not to do this?
  el <- D.createElement name doc
  appendChild (toNode el) (toNode parent)
  pure el

type ScratchAppender = ReaderT Element Effect -- the global env is the scratchpad element we're appending to
runSA :: forall m a. MonadEffect m => Element -> ScratchAppender a -> m a
runSA e sa = liftEffect $ runReaderT sa e

appendInputGroup :: String -> ScratchAppender Unit
appendInputGroup input = do
  let lines = split (Pattern "\n") input
      firstLine = case uncons lines of
        Just { head, tail } -> head
        _ -> ""
  parent <- ask
  lift $ do
    doc <- toDocument <$> getDoc
    details <- createChildElem "details" parent
    setClassName "row-input" details
    summary <- createChildElem "summary" details
    setInnerHTML summary $ "Input    " <> firstLine
    p <- createChildElem "p" details
    setInnerHTML p input
    pure unit

--| Create a folding group element for storing REPL output.
--| Returns the folded element to append output to.
appendOutputGroup :: ScratchAppender Element
appendOutputGroup = do
  parent <- ask
  lift $ do
    doc <- toDocument <$> getDoc
    details <- createChildElem "details" parent
    setClassName "row-output" details
    setAttribute "open" "true" details
    summary <- createChildElem "summary" details
    setInnerHTML summary "Output"
    -- content <- createChildElem "div" details
    pure details

--| Appends raw output to a given element, returns the generated paragraph
appendOutput :: String -> ScratchAppender Element
appendOutput rawOutput = do
  parent <- ask
  lift $ do
    pEl <- createChildElem "p" parent
    setInnerHTML pEl rawOutput
    pure pEl

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
    SLIO.gainKnowledge
    SLD.gainDebugKnowledge
  -- Run the game's entrypoint
  handleAction $ RunCode "?"
  handleAction $ RunCode GM.program

handleAction :: forall s o m. MonadAff m => Action → H.HalogenM State Action s o m Unit
handleAction action = do
  doc <- getDoc
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
          replRow <- liftEffect $ createChildElem "div" outputElem
          liftEffect $ setClassName "repl-row" replRow
          runSA replRow $ appendInputGroup code
          outputEl <- runSA replRow appendOutputGroup
          let
            groupLog s = do
              traceM $ "Got output from language:" <> s
              runSA outputEl $ appendOutput s
            groupErr e = do
              traceM e
              runSA outputEl $ appendOutput (show e)
          liftVM do
            L.setErrhandler (void <<< groupErr)
            L.setLogger (void <<< groupLog)
            L.vmAction code
    _ -> pure unit
