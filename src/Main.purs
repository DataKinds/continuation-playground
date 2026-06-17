module Main where

import Data.Maybe
import Prelude

import App.Button as Button
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.State (State)
import Data.Array as A
import Data.HashMap (HashMap)
import Data.List as L
import Data.String as S
import Data.Sequence (Seq)
import Data.Sequence as Seq
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

type RValue = String -- Runtime value
type ModuleName = String
type StackName = String
type WordName = String

--| Evaluation monad for the langauge
class (Monad m, MonadEffect m, MonadThrow String m) <= MonadEval m where
  --| Add a definition, given a module name, definition name, and definition. 
  record :: ModuleName -> String -> L.List String -> m Unit
  --| Run a continuation in a module.
  execute :: ModuleName -> L.List String -> m Unit
  --| Add a dependency between modules for name resolution. Puts a module _inside_ another module
  depend :: ModuleName -> ModuleName -> m Unit
  --| Push to a named stack in a module. Creates a stack if it doesn't exist.
  push :: ModuleName -> StackName -> m Unit
  pop :: ModuleName -> StackName -> m Unit
  --| Peek at a value on a stack
  peek :: ModuleName -> StackName -> Int -> m RValue

type Module =
  { chain :: Array ModuleName
  , defs :: HashMap WordName (L.List String)
  , stacks :: HashMap  }
type RealState = 
  { modules :: HashMap ModuleName Module }
type RealEval = State RealState 

--| Run a program's parse-time directives, like MODULE: 
firstPass :: forall m. MonadEval m => String -> m String
firstPass = words >>> run
  where
    run :: L.List String -> m String
    run ws = record 

nextWordRaw :: L.List String -> Maybe (Tuple String (L.List String))
nextWordRaw ws = L.uncons ws >>= (\{ head, tail } -> pure $ Tuple head tail)

words :: String -> L.List String
words = S.split (S.Pattern " ") >>> L.fromFoldable

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Button.component unit body
