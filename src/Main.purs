module Main where

import Common
import Control.Monad.State
import Data.Either
import Data.Maybe
import Prelude

import App.REPL as REPL
import Control.Monad.Error.Class (class MonadThrow, catchError, throwError, try)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array ((:))
import Data.Array as A
import Debug (traceM)
import Data.Array.NonEmpty as NA
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.List as L
import Data.List.Lazy as LL
import Data.Newtype (class Newtype, unwrap)
import Data.String (trim)
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
emptyModule = { chain: mempty, defs: HM.empty, stacks: mempty, openStacks: NA.singleton "main" }

emptyStack :: RStack
emptyStack = mempty

emptyRealState :: RealState
emptyRealState = RealState { modules: HM.empty, openModules: NA.singleton "main", source: [], sourceIx: 0 }

throw = error >>> throwError
throwUnderflow = throw "stack underflow" -- TODO: better errors
throwEOF after = throw $ "expected word after " <> after <> ", got EOF"


nextWordTrimmedOrEOF errMsg = do
  nw <- nextWordTrimmed
  case nw of
    Nothing -> throwEOF errMsg
    Just nw' -> pure nw'

--| Load up the standard library.
gainKnowledge :: forall m. MonadEval m => m Unit
gainKnowledge = do
  recordNative "main" "help" $ do
    liftEffect $ Console.log "need help?!"
  recordNativeSyntax "main" "\\" $ do
    nw <- nextWordTrimmedOrEOF "backslash"
    mn <- getOpenModule
    sn <- getOpenStack
    push mn sn nw
    pure []

  recordNative "main" "enter" $ do
    mn <- getOpenModule
    sn <- getOpenStack
    rv <- pop mn sn 
    case rv of
      Nothing -> throwUnderflow
      Just rv' -> enter rv'

  recordNativeSyntax "main" "ENTER:" $ do
    nw <- nextWordTrimmedOrEOF "ENTER:"
    pure ["\\", nw, "enter"]

--| Load up functions that hook into the interpreter internals
gainDebugKnowledge :: RealEval Unit
gainDebugKnowledge = do
  depend "main" "debug"
  recordNative "debug" "?" $ do
    RealState { modules, openModules } <- get
    mn <- getOpenModule
    sn <- getOpenStack
    m@{ chain, defs, stacks, openStacks } <- getOrMakeModule mn

    let
      lines = join
        [ [ "module-toy 0. enjoy your stay"
          , ""
          , show (HM.size modules) <> " modules loaded (" <> S.joinWith "," (HM.keys modules) <> ")"
          , "you've entered the scope of " <> show (NA.length openModules) <> " modules. currently " <> mn <> " is active"
          , ""
          , "within this module (" <> mn <> "), there are " <> show (HM.size defs) <> " definitions:"
          ]
        , HM.toArrayBy (\k v -> "  * " <> k <> ": " <> show v) defs
        , [ ""
          , "this module (" <> mn <> ") has the following resolution chain: [" <> S.joinWith "," chain <> "]"
          ]
        , [ ""
          , "within this module (" <> mn <> ") there are " <> show (HM.size stacks) <> " stacks, and " <> sn <> " is active"
          ]
        ]
    liftEffect <<< Console.log <<< S.joinWith "\n" $ lines

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
_getOpenModule' :: RealEval ModuleName
_getOpenModule' = get <#> \(RealState { openModules }) -> NA.head openModules

--| Evaluation monad for the langauge
class (Monad m, MonadEffect m, MonadThrow Error m) <= MonadEval m where
  --| Get a single, raw, space-delimited word from the input, and seek the input forward.
  nextWordRaw :: m (Maybe String)
  --| Add a definition, given a module name, definition name, and definition. 
  record :: ModuleName -> String -> Array String -> m Unit
  recordNative :: ModuleName -> String -> m Unit -> m Unit
  recordNativeSyntax :: ModuleName -> String -> m (Array WordName) -> m Unit
  --| Query the VM for the currently active module or stack.
  getOpenModule :: m ModuleName --| What module should we currently be executing in?
  getOpenStack :: m StackName --| What stack should we be modifying? Note that this is scoped to modules.
  --| Switch the active module used for execution.
  enter :: ModuleName -> m Unit
  leave :: m Unit
  --| Within the active module, switch the active stack used for execution.
  into :: StackName -> m Unit
  outof :: m Unit
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
  recordNativeSyntax mn name def = alterModule mn \m -> pure $ m { defs = HM.insert name (NativeSyntax def) m.defs }
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

  getOpenModule = get <#> \(RealState { openModules }) -> NA.head openModules
  getOpenStack = _getOpenModule' >>= getOrMakeModule >>= \{ openStacks } -> pure $ NA.head openStacks -- TODO: WTF?

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
  into :: StackName -> RealEval Unit
  into sn = do
    mn <- getOpenModule
    alterModule mn \m -> Just m { openStacks = NA.cons sn m.openStacks }
  outof :: RealEval Unit
  outof = do
    mn <- _getOpenModule' -- TODO: why can't I just call getOpenModule here???
    alterModule mn \m@{ openStacks } ->
      let
        { head: _, tail } = NA.uncons openStacks
        maybeNewStacks = NA.fromArray tail -- fails if tail is empty
      in
        case maybeNewStacks of
          Nothing -> Just m -- tried to `leave` the final module, just no-op
          Just newStacks -> Just m { openStacks = newStacks }
  execute :: WordName -> RealEval Unit
  execute cont = do
    mn <- getOpenModule
    maybeDef <- lookup mn cont
    case maybeDef of
      Nothing -> throwError <<< error $ "unknown word " <> cont <> " in module " <> mn
      Just (Native f) -> f
      Just (Canon def) -> do
        _ <- sequence $ map execute def
        pure unit
      Just (NativeSyntax fmacro) -> do
        expansion <- fmacro
        -- traceM $ "got an expansion"
        -- traceM expansion
        _ <- sequence $ map execute expansion -- TODO: this breaks in cases with nested expansions
        pure unit
      Just (CanonSyntax defmacro) ->
        pure unit -- TODO

--| Like nextWordRaw, but skips whitespace if you don't care.
nextWordTrimmed :: forall m. MonadEval m => m (Maybe String)
nextWordTrimmed = do
  maybeNw <- nextWordRaw
  case maybeNw of
    Nothing -> pure Nothing
    Just nw
      | trim nw == "" -> nextWordTrimmed -- just whitespace, keep scanning
      | otherwise -> pure $ Just nw

--| Execute the next word ready to be processed by the VM and seek forward. Returns false if out of input.
--| Follows two passes: if the word is a macro, it is executed immediately and may consume more words.
--| If the word is a not a macro, execute its definition.
executeNextWord :: forall m. MonadEval m => m Boolean
executeNextWord = do
  maybeNw <- nextWordTrimmed
  case maybeNw of
    Nothing -> pure false
    Just nw
      | trim nw == "" -> executeNextWord -- just whitespace, keep scanning
      | otherwise -> execute nw *> pure true

--| Low level function to carry out an action specified by RealEval.
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
    go _ = do
      notDone <- executeNextWord
      pure $ if notDone then Loop unit else Done unit
    execAction = gainKnowledge *> gainDebugKnowledge *> (tailRecM go unit)
  in
    evalRealState execAction startingState Console.logShow -- TODO: executeNextWord till we can't

words :: String -> Array String
words = S.split (S.Pattern " ")

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI REPL.component unit body
