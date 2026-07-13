module StandardLibrary.Debug where

import Prelude

import Common as C
import Control.Monad.State (get)
import Data.Array as A
import Data.Array.NonEmpty as NA
import Data.Foldable (class Foldable)
import Data.HashMap as HM
import Data.String as S
import Effect.Class (liftEffect)
import Lang as L

--| Load up functions that hook into the interpreter internals
gainDebugKnowledge :: C.RealEval Unit
gainDebugKnowledge = do
  L.depend "main" "debug"
  L.define "debug" "?" $ C.Native do
    C.RealState { modules, openModules } <- get
    l <- map liftEffect <$> L.getLogger
    mn <- L.getActiveModule
    mns <- L.getOpenModuleChain
    sn <- L.getOpenStack
    m@{ chain, defs, stacks, openStacks } <- L.getOrMakeModule mn
    let
      unlist :: forall a. Foldable a => a String -> String
      unlist as = S.joinWith ", " $ A.fromFoldable as
      unwords as = S.joinWith " " $ A.fromFoldable as
      unlines as = S.joinWith "\n" $ A.fromFoldable as

      lines =
        [ "janna v0 knows " <> show (HM.size modules) <> " modules: " <> unlist (HM.keys modules)
        , "open scopes (active scope first): " <> unlist mns
        , ""
        , "within " <> mn <> ", there are " <> show (HM.size defs) <> " definitions:"
        , unwords $ HM.keys defs
        , ""
        , "within " <> mn <> ", the resolution chain is: " <> unlist chain
        , ""
        , "within " <> mn <> ", the active stack is " <> sn <> ". overall, we have:"
        , unlines $ HM.toArrayBy (\sn s -> sn <> show s) stacks

        ]

    liftEffect <<< l <<< S.joinWith "\n" $ lines
    pure unit
  L.define "debug" "..." $ C.Native do
    l <- map liftEffect <$> L.getLogger
    mn <- L.getActiveModule
    sn <- L.getOpenStack
    m@{ chain, defs, stacks, openStacks } <- L.getOrMakeModule mn
    let
      moduleLine = "module " <> mn <> ", open stack " <> sn
      stackLines = HM.toArrayBy (\sn s -> sn <> show s) stacks
    l $ moduleLine <> "\n    " <> S.joinWith "\n    " stackLines

