module StandardLibrary.IO where

import Prelude

import Common as C
import Control.Monad.Error.Class (throwError)
import Data.String (joinWith, trim)
import Debug (traceM)
import Effect.Class (liftEffect)
import Lang as L

appendSpaceIfFull :: String -> String
appendSpaceIfFull s
  | trim s == "" = s
  | otherwise = s <> " "

--| Load up the IO functions.
gainKnowledge :: forall m. L.MonadVM m => L.MonadSwappableLogger C.VMError m => m Unit
gainKnowledge = do
  L.depend "main" "io"
  -- Basic printing words --
  
  --| Print and pop a raw value
  L.define "io" "." $ C.Native do
    l <- map liftEffect <$> L.getLogger
    mn <- L.getActiveModule
    sn <- L.getActiveStack
    rv <- L.popWithUnderflow mn sn
    l $ show rv
  --| Print and pop a quote as a string
  L.define "io" ".s" $ C.Native do
    l <- map liftEffect <$> L.getLogger
    mn <- L.getActiveModule
    sn <- L.getActiveStack
    rvs <- L.popQuoteWithUnderflow mn sn
    l rvs
  --| Print the whole current stack
  L.define "io" ".." $ C.Native do
    l <- map liftEffect <$> L.getLogger
    sn <- L.dumpOpenStack
    l $ show sn
