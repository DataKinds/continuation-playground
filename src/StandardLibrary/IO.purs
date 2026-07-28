module StandardLibrary.IO where

import Common as C
import Control.Monad.Error.Class (throwError)
import Debug (traceM)
import Effect.Class (liftEffect)
import Lang as L

--| Load up the standard library.
gainKnowledge :: forall m. L.MonadVM m => L.MonadSwappableLogger C.VMError m => m Unit
gainKnowledge = do
  L.depend "main" "io"
  L.define "io" "." $ C.Native do
    l <- map liftEffect <$> L.getLogger
    mn <- L.getActiveModule
    sn <- L.getActiveStack
    rv <- L.popWithUnderflow mn sn
    l $ show rv
