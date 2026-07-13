module StandardLibrary.Debug where

import Prelude

import Common as C
import Control.Monad.State (get)
import Data.Array.NonEmpty as NA
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
    sn <- L.getOpenStack
    m@{ chain, defs, stacks, openStacks } <- L.getOrMakeModule mn
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


