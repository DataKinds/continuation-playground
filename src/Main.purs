module Main where

import Control.Monad.State
import Data.Maybe
import Prelude

import App.Button as Button
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT)
import Data.Array as A
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.List as L
import Data.String as S
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

type RValue = String -- Runtime value
type RStack = Array RValue
type ModuleName = String
type StackName = String
type WordName = String

--| Evaluation monad for the langauge
class (Monad m, MonadEffect m, MonadThrow String m) <= MonadEval m where
  --| Get a single, raw, space-delimited word from the input, and seek the input forward.
  nextWordRaw :: m (Maybe String)
  --| Add a definition, given a module name, definition name, and definition. 
  record :: ModuleName -> String -> Array String -> m Unit
  --| Run a continuation in a module.
  execute :: ModuleName -> Array String -> m Unit
  --| Add a dependency between modules for name resolution. Puts a module _inside_ another module
  depend :: ModuleName -> ModuleName -> m Unit
  --| Push to a named stack in a module. Creates a stack if it doesn't exist.
  push :: ModuleName -> StackName -> m Unit
  pop :: ModuleName -> StackName -> m RValue
  --| Peek at a value on a stack
  peek :: ModuleName -> StackName -> Int -> m RValue

data Definition evalM = Native (evalM Unit) | Canon (Array String)

type Module evalM =
  { chain :: Array ModuleName
  , defs :: HashMap WordName (Definition evalM)
  , stacks :: HashMap StackName RStack }
emptyModule :: forall m. Module m
emptyModule = { chain: mempty, defs: HM.empty, stacks: mempty }
newtype RealState = RealState 
  { modules :: HashMap ModuleName (Module RealEval)
  , openModule :: ModuleName
  , source :: Array String
  , sourceIx :: Int }
-- emptyRealState :: RealState 
-- emptyRealState = RealState { modules: a, openModule:a, source:a, sourceIx:a }
type RealEval = StateT RealState (ExceptT String Effect)

getOrMakeModule :: ModuleName -> RealEval (Module RealEval)
getOrMakeModule mn = do
  RealState { modules } <- get
  case HM.lookup mn modules of
    (Just m) -> pure m
    Nothing -> do
      modify_ \(RealState st) -> RealState st { modules = HM.insert mn emptyModule st.modules }
      pure emptyModule

alterModule :: ModuleName -> (Maybe (Module RealEval) -> Maybe (Module RealEval)) -> RealEval Unit
alterModule mn f = modify_ \(RealState st) -> RealState st { modules = HM.alter f mn st.modules }

getOrMakeStack :: ModuleName -> StackName -> RealEval RStack
getOrMakeStack mn sn = do
  alterModule mn \m -> let
    stack = 
    Just $ maybe emptyModule (\RealState st -> RealState st { stacks = HM.stacks }) 
  m <- getOrMakeModule mn
  case HM.lookup sn m of
    Just stack -> pure stack
    Nothing -> pure emptyStack -- impossible


instance monadEvalRealEval :: MonadEval RealEval where
  nextWordRaw :: RealEval (Maybe String)
  nextWordRaw = do
    modify_ \(RealState st) -> RealState st { sourceIx = st.sourceIx + 1 }
    RealState { source, sourceIx } <- get
    pure $ A.index source sourceIx
  record :: ModuleName -> String -> Array String -> RealEval Unit
  record mn n def = pure unit
  depend :: ModuleName -> ModuleName -> RealEval Unit
  depend mn mn' = pure unit
  push :: ModuleName -> StackName -> RValue -> RealEval Unit
  push mn sn rv = do
    { stacks } <- getOrMakeStack mn sn
    --TODO
    pure unit
  pop :: ModuleName -> StackName -> RealEval RValue
  pop mn sn = pure "unit"
  peek :: ModuleName -> StackName -> Int -> RealEval RValue
  peek mn sn depth = pure "ok"
  execute :: ModuleName -> Array String -> RealEval Unit
  execute mn cont = pure unit--do

--| Run a program's parse-time directives, like MODULE: 
firstPass :: forall m. MonadEval m => String -> m String
firstPass = words >>> run
  where
    run :: Array String -> m String
    run ws = pure "ok" 


words :: String -> Array String
words = S.split (S.Pattern " ")

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Button.component unit body
