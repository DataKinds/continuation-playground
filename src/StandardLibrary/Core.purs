module StandardLibrary.Core where

import Prelude

import Common as C
import Effect.Class (liftEffect)
import Lang as L
import Data.Maybe

--| Load up the standard library.
gainKnowledge :: forall m. L.MonadVM m => L.MonadSwappableLogger L.VMError m => m Unit
gainKnowledge = do
  L.define "main" "help" $ C.Native do
    l <- map liftEffect <$> L.getLogger
    l "need help?!" 
  L.define "main" "..." $ C.Native do
    l <- map liftEffect <$> L.getLogger
    sn <- L.dumpOpenStack
    l $ show sn -- TODO: dump whole stack
  L.define "main" "\\" $ C.NativeSyntax do
    nw <- L.nextWordTrimmedOrThrowEOF "backslash"
    mn <- L.getOpenModule
    sn <- L.getOpenStack
    L.push mn sn nw
    pure []

  L.define "main" "enter" $ C.Native do
    mn <- L.getOpenModule
    sn <- L.getOpenStack
    rv <- L.pop mn sn
    case rv of
      Nothing -> L.throwUnderflow
      Just rv' -> L.enter rv'

  L.define "main" "ENTER:" $ C.NativeSyntax do
    nw <- L.nextWordTrimmedOrThrowEOF "ENTER:"
    pure [ "\\", nw, "enter" ]
