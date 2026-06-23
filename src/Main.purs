module Main where

import Control.Monad.State
import Data.Either
import Data.Maybe
import Prelude
import Common

import App.Button as Button
import Control.Monad.Error.Class (class MonadThrow, catchError, throwError, try)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Array ((:))
import Data.Array as A
import Data.Array.NonEmpty as NA
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.List as L
import Data.List.Lazy as LL
import Data.Newtype (class Newtype, unwrap)
import Data.String as S
import Data.Traversable (sequence, sequence_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Exception (Error, error)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.Document (doctype)


emptyModule :: forall m. Module m
emptyModule = { chain: mempty, defs: HM.empty, stacks: mempty }

emptyStack :: RStack
emptyStack = mempty

emptyRealState :: RealState
emptyRealState = RealState { modules: HM.empty, openModules: NA.singleton "main", source: [], sourceIx: 0 }


-- --| Load up the standard library.
-- emptyRealEval :: String -> RealEval Unit
-- emptyRealEval str = 

getOrMakeModule :: ModuleName -> RealEval (Module RealEval)
getOrMakeModule mn = do
  RealState { modules } <- get
  case HM.lookup mn modules of
    (Just m) -> pure m
    Nothing -> do
      modify_ \(RealState st) -> RealState st { modules = HM.insert mn emptyModule st.modules }
      pure emptyModule

alterModule :: ModuleName -> ((Module RealEval) -> Maybe (Module RealEval)) -> RealEval Unit
alterModule mn f = modify_ \(RealState st) -> RealState st { modules = HM.alter (fromMaybe emptyModule >>> f) mn st.modules }

getOrMakeStack :: ModuleName -> StackName -> RealEval RStack
getOrMakeStack mn sn = do
  m@{ stacks } <- getOrMakeModule mn
  let
    outStack = case HM.lookup sn stacks of
      Just stack -> stack
      Nothing -> emptyStack -- impossible
  alterModule mn (\_ -> pure $ m { stacks = HM.insert sn outStack stacks })
  pure outStack

alterStack :: ModuleName -> StackName -> (RStack -> Maybe RStack) -> RealEval Unit
alterStack mn sn f = alterModule mn \m@{ stacks } -> pure $ m { stacks = HM.alter (fromMaybe emptyStack >>> f) sn stacks }

getOpenModule :: RealEval ModuleName
getOpenModule = get <#> \(RealState { openModules }) -> NA.head openModules

--| Evaluation monad for the langauge
class (Monad m, MonadEffect m, MonadThrow Error m) <= MonadEval m where
  --| Get a single, raw, space-delimited word from the input, and seek the input forward.
  nextWordRaw :: m (Maybe String)
  --| Add a definition, given a module name, definition name, and definition. 
  record :: ModuleName -> String -> Array String -> m Unit
  recordNative :: ModuleName -> String -> m Unit -> m Unit
  --| Switch the active module used for execution.
  enter :: ModuleName -> m Unit
  leave :: m Unit
  --| Run a continuation in the current module, one word at a time.
  execute :: WordName -> m Unit
  --| Add a dependency between modules for name resolution. Puts a module _inside_ another module
  depend :: ModuleName -> ModuleName -> m Unit
  --| Push to a named stack in the current module. Creates a stack if it doesn't exist.
  push :: ModuleName -> StackName -> RValue -> m Unit
  pop :: ModuleName -> StackName -> m (Maybe RValue)
  --| Peek at a value on a stack
  peek :: ModuleName -> StackName -> Int -> m RValue
  --| Look up a name in a module chain
  lookup :: ModuleName -> WordName -> m (Maybe (Definition m))

instance monadEvalRealEval :: MonadEval RealEval where
  nextWordRaw :: RealEval (Maybe String)
  nextWordRaw = do
    RealState { source, sourceIx } <- get
    modify_ \(RealState st) -> RealState st { sourceIx = st.sourceIx + 1 }
    pure $ A.index source sourceIx
  record :: ModuleName -> WordName -> Array WordName -> RealEval Unit
  record mn name def = alterModule mn \m -> pure $ m { defs = HM.insert name (Canon def) m.defs }
  recordNative :: ModuleName -> WordName -> RealEval Unit -> RealEval Unit
  recordNative mn name def = alterModule mn \m -> pure $ m { defs = HM.insert name (Native def) m.defs }
  depend :: ModuleName -> ModuleName -> RealEval Unit
  depend mn mn' = alterModule mn \m -> pure $ m { chain = mn' : m.chain }
  push :: ModuleName -> StackName -> RValue -> RealEval Unit
  push mn sn rv = alterStack mn sn (\stack -> pure $ rv : stack)
  pop :: ModuleName -> StackName -> RealEval (Maybe RValue)
  pop mn sn = do
    stack <- getOrMakeStack mn sn
    case A.uncons stack of
      Nothing -> pure Nothing -- TODO: log exception?
      Just { head: x, tail: xs } -> do
        alterStack mn sn (const (Just xs))
        pure $ Just x
  peek :: ModuleName -> StackName -> Int -> RealEval RValue
  peek mn sn depth = pure "ok"
  lookup :: ModuleName -> WordName -> RealEval (Maybe (Definition RealEval))
  lookup mn name = do
    m@{ defs, chain } <- getOrMakeModule mn
    case HM.lookup name defs of
      Just def -> pure $ Just def
      -- First layer name resolution (in the direct module) failed:
      -- let's do a BFS of the inheritance tree to try to resolve the name in a parent context
      -- Note that this method can diverge, there is no static guarantee at the moment that the dependency graph is acyclic.
      Nothing -> rec chain
    where
    rec :: Array ModuleName -> RealEval (Maybe (Definition RealEval))
    rec mns = case A.uncons mns of
      Nothing -> pure Nothing
      Just { head, tail } -> do
        maybeDef <- lookup head name
        case maybeDef of
          Nothing -> rec tail
          Just def -> pure $ Just def
  enter :: ModuleName -> RealEval Unit
  enter mn = modify_ \(RealState st) -> RealState st { openModules = NA.cons mn st.openModules }
  leave :: RealEval Unit
  leave = do
    RealState { openModules } <- get
    let
      { head: _, tail } = NA.uncons openModules
      maybeNewModules = NA.fromArray tail -- fails if tail is empty
    case maybeNewModules of
      Nothing -> pure unit -- tried to `leave` the final module, just no-op
      Just newModules -> modify_ \(RealState st) -> RealState st { openModules = newModules }
  execute :: WordName -> RealEval Unit
  execute cont = do
    mn <- getOpenModule
    maybeDef <- lookup mn cont
    case maybeDef of
      Nothing -> throwError <<< error $ "unknown word " <> cont
      -- TODO: also handle syntax words
      Just (Native f) -> f
      Just (Canon def) -> do
        _ <- sequence $ map execute def
        pure unit

--| Execute the next word ready to be processed by the VM and seek forward.
--| Follows two passes: if the word is a macro, it is executed immediately and may consume more words.
--| If the word is a not a macro, execute its definition.
executeNextWord :: forall m. MonadEval m => m Unit
executeNextWord = do
  maybeNw <- nextWordRaw
  case maybeNw of
    Nothing -> pure unit
    Just nw -> execute nw

evalRealState :: forall a. RealEval a -> RealState -> (Error -> Effect a) -> Effect a
evalRealState action starting errHandler =
  let
    throwable :: ExceptT Error Effect a
    throwable = evalStateT action starting
  in
    do
      out <- runExceptT throwable
      case out of
        Right x -> pure x
        Left err -> errHandler err

--| Given a string that contains a program, evaluate the program entirely. 
--| Log both error messages and output to stdout
evaluate :: String -> Effect Unit
evaluate str =
  let
    startingState = RealState <<< _ { source = words str } $ unwrap emptyRealState
  in
    evalRealState executeNextWord startingState Console.logShow -- TODO: executeNextWord till we can't

words :: String -> Array String
words = S.split (S.Pattern " ")

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Button.component unit body
