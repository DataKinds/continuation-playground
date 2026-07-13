module Lang where

import Common
import Control.Monad.State
import Data.Either
import Data.Maybe
import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError, try)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Data.Array ((:))
import Data.Array as A
import Data.Array.NonEmpty as NA
import Data.Functor.Contravariant (coerce)
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.List as L
import Data.List.Lazy as LL
import Data.Newtype (class Newtype, unwrap)
import Data.Number (e)
import Data.String (trim)
import Data.String as S
import Data.Traversable (sequence, sequence_)
import Data.Tuple (Tuple(..))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Exception (Error, error)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.Document (doctype)


emptyModule :: forall m. Module m
emptyModule = { chain: mempty, defs: HM.empty, stacks: mempty, openStacks: NA.singleton "main" }

emptyStack :: RStack
emptyStack = mempty

emptyRealState :: RealState
emptyRealState = RealState { modules: HM.empty, openModules: NA.singleton "main", source: [], sourceIx: 0, errorHandler: traceM, outputHandler: Console.log }

throwEOF after = throwError <<< EOF $ "expected word after " <> after <> ", got EOF"

nextWordTrimmedOrThrowEOF errMsg = do
  nw <- nextWordTrimmed
  case nw of
    Nothing -> throwEOF errMsg
    Just nw' -> pure nw'


class (MonadEffect m, MonadError e m) <= MonadSwappableLogger e m | m -> e where
  setErrhandler :: (e -> Effect Unit) -> m Unit
  setLogger :: (String -> Effect Unit) -> m Unit
  getErrhandler :: m (e -> Effect Unit)
  getLogger :: m (String -> Effect Unit)

instance MonadSwappableLogger VMError RealEval where
  setErrhandler eH = modify_ \(RealState st) -> RealState st { errorHandler = eH }
  setLogger oH = modify_ \(RealState st) -> RealState st { outputHandler = oH }
  getErrhandler = get <#> \(RealState st) -> st.errorHandler
  getLogger = get <#> \(RealState st) -> st.outputHandler

--=== Lenses for the module and stack types ===--
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

--| What module should we currently be executing in? TODO: this should be removable
_getActiveModule :: RealEval ModuleName
_getActiveModule = get <#> \(RealState { openModules }) -> NA.head openModules


-- withErrhandler :: forall e m n a b. MonadSwappableLogger e m => MonadThrow e n => (a -> n b) -> m (n b)
-- withErrhandler action = do
--   errHandler <- getErrhandler
--   catchError action errHandler

--| Things that read from the VM state without changing it
class MonadThrow VMError m <= MonadReadVM m where
  --| What module is active for execution? 
  getActiveModule :: m ModuleName
  getOpenModuleChain :: m (NA.NonEmptyArray ModuleName) 
  --| What stack are we actively executing on?
  getOpenStack :: m StackName
  --| Grab a runtime instance of the open stack
  dumpOpenStack :: m RStack
  --| Peek at a value on a stack
  peek :: ModuleName -> StackName -> Int -> m (Maybe RValue)
  --| Look up a name in a module chain
  lookup :: ModuleName -> WordName -> m (Maybe (Definition m))

instance MonadReadVM RealEval where
  getActiveModule = get <#> \(RealState { openModules }) -> NA.head openModules
  getOpenModuleChain = get <#> \(RealState { openModules }) -> openModules
  getOpenStack = _getActiveModule >>= getOrMakeModule >>= \{ openStacks } -> pure $ NA.head openStacks -- TODO: WTF?
  dumpOpenStack = do
    mn <- _getActiveModule
    sn <- getOpenStack
    getOrMakeStack mn sn
  peek mn sn depth = do
    stack <- getOrMakeStack mn sn
    pure $ A.index stack depth
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

--| Handling the input tape of words to be executed
class Monad m <= MonadVMTape m where
  loadRaw :: String -> m Unit
  --| Pop a single, raw, space-delimited word from the input
  popRawWord :: m (Maybe String)
  --| Push a single, raw, space-delimited word back to the input
  pushRawWord :: String -> m Unit

instance MonadVMTape RealEval where
  loadRaw raw = 
    modify_ \(RealState st) -> RealState st { source = words raw, sourceIx = 0 }
    where words = S.split (S.Pattern " ")
  popRawWord = do
    RealState { source, sourceIx } <- get
    modify_ \(RealState st) -> RealState st { sourceIx = st.sourceIx + 1 }
    pure $ A.index source sourceIx
  pushRawWord w =
    modify_ \(RealState st@{ source, sourceIx }) -> RealState st { source = fromMaybe source $ A.insertAt sourceIx w source }

--| Evaluation monad for the langauge
class (MonadReadVM m, MonadVMTape m, MonadEffect m, MonadAff m, MonadSwappableLogger VMError m, MonadThrow VMError m) <= MonadVM m where
  --| Add a definition, given a module name, definition name, and definition. 
  define :: ModuleName -> WordName -> Definition m -> m Unit
  --| Push and pop the active module used for execution.
  enter :: ModuleName -> m Unit
  leave :: m Unit
  --| Within the active module, push and pop the active stack used for execution.
  into :: StackName -> m Unit
  outof :: m Unit
  --| Run a continuation in the current module, one word at a time.
  execute :: WordName -> m Unit
  --| Add a dependency between modules for name resolution. Puts a module _inside_ another module
  depend :: ModuleName -> ModuleName -> m Unit
  --| Push to a named stack in the current module. Creates a stack if it doesn't exist.
  push :: ModuleName -> StackName -> RValue -> m Unit
  pop :: ModuleName -> StackName -> m (Maybe RValue)

instance monadEvalRealEval :: MonadVM RealEval where
  define mn name def = alterModule mn \m -> pure $ m { defs = HM.insert name def m.defs }

  depend mn mn' = alterModule mn \m -> pure $ m { chain = mn' : m.chain }

  push mn sn rv = alterStack mn sn (\stack -> pure $ rv : stack)

  pop mn sn = do
    stack <- getOrMakeStack mn sn
    case A.uncons stack of
      Nothing -> pure Nothing -- TODO: log exception?
      Just { head: x, tail: xs } -> do
        alterStack mn sn (const (Just xs))
        pure $ Just x

  enter mn = modify_ \(RealState st) -> RealState st { openModules = NA.cons mn st.openModules }

  leave = do
    RealState { openModules } <- get
    let
      { head: _, tail } = NA.uncons openModules
      maybeNewModules = NA.fromArray tail -- fails if tail is empty
    case maybeNewModules of
      Nothing -> pure unit -- tried to `leave` the final module, just no-op
      Just newModules -> modify_ \(RealState st) -> RealState st { openModules = newModules }

  into sn = do
    mn <- getActiveModule
    alterModule mn \m -> Just m { openStacks = NA.cons sn m.openStacks }

  outof = do
    mn <- getActiveModule 
    alterModule mn \m@{ openStacks } ->
      let
        { head: _, tail } = NA.uncons openStacks
        maybeNewStacks = NA.fromArray tail -- fails if tail is empty
      in
        case maybeNewStacks of
          Nothing -> Just m -- tried to `leave` the final module, just no-op
          Just newStacks -> Just m { openStacks = newStacks }

  execute cont = do
    mns <- getOpenModuleChain
    case NA.findMap (\mn -> pure $ lookup mn cont) mns of
      Nothing -> throwError $ UnknownWord (NA.toArray mns) cont
      Just (Native f) -> f
      Just (Canon def) -> do
        _ <- sequence $ map execute def
        pure unit
      Just (NativeSyntax fmacro) -> do
        expansion <- fmacro
        -- traceM $ "got an expansion"
        -- traceM expansion
        _ <- sequence $ map pushRawWord (A.reverse expansion)
        pure unit
      Just (CanonSyntax defmacro) ->
        pure unit -- TODO


--| Like popRawWord, but skips whitespace if you don't care.
nextWordTrimmed :: forall m. MonadVM m => m (Maybe String)
nextWordTrimmed = do
  maybeNw <- popRawWord
  case maybeNw of
    Nothing -> pure Nothing
    Just nw
      | trim nw == "" -> nextWordTrimmed -- just whitespace, keep scanning
      | otherwise -> pure $ Just nw

--| Execute the next word ready to be processed by the VM and seek forward. Returns false if out of input.
executeNextWord :: forall m. MonadVM m => m Boolean
executeNextWord = do
  maybeNw <- nextWordTrimmed
  errorHandler <- map liftEffect <$> getErrhandler
  case maybeNw of
    Nothing -> pure false
    Just nw -> catchError (execute nw) errorHandler *> pure true

--| Initialize a language evaluation monad
-- initialVMAction :: forall m. MonadVM m => (VMError → Effect Unit) → (String → Effect Unit) → m Unit
-- initialVMAction errHandler outputHandler =
--   setLogger outputHandler
--     *> setErrhandler errHandler
--     *> gainKnowledge

-- initialDebugVMAction :: (VMError → Effect Unit) → (String → Effect Unit) → RealEval Unit
-- initialDebugVMAction errHandler outputHandler = initialVMAction errHandler outputHandler *> gainDebugKnowledge

--| Execute some code within the evaluation monad
vmAction :: forall m. MonadVM m => MonadRec m => String -> m Unit
vmAction input = let 
    go _ = do
      notDone <- executeNextWord
      pure $ if notDone then Loop unit else Done unit
    execAction = tailRecM go unit
  in loadRaw input *> execAction

--| Execute our implementation of MonadVM in the Aff monad, returning the intermediate state of the interpreter after execution.
execVMAff :: forall a. RealEval a -> RealState -> Aff RealState
execVMAff action starting@(RealState { errorHandler }) =
  let
    throwable :: ExceptT VMError Aff RealState
    throwable = execStateT action starting
  in
    do
      out <- runExceptT throwable
      case out of
        Right newState -> pure newState
        Left err -> do
          liftEffect $ errorHandler err
          pure starting
