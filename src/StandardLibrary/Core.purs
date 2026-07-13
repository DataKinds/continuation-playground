module StandardLibrary.Core where

import Data.Maybe
import Prelude

import Common as C
import Control.Monad.Error.Class (throwError)
import Effect.Class (liftEffect)
import Lang as L

--| Load up the standard library.
gainKnowledge :: forall m. L.MonadVM m => L.MonadSwappableLogger C.VMError m => m Unit
gainKnowledge = do
  L.depend "main" "core"
  L.define "core" "help" $ C.Native do
    l <- map liftEffect <$> L.getLogger
    l "need help?!" 
  L.define "core" ".." $ C.Native do
    l <- map liftEffect <$> L.getLogger
    sn <- L.dumpOpenStack
    l $ show sn
  L.define "core" "\\" $ C.NativeSyntax do
    nw <- L.nextWordTrimmedOrThrowEOF "backslash"
    mn <- L.getActiveModule
    sn <- L.getActiveStack
    L.push mn sn nw
    pure []

  --| Open or close a stack
  L.define "core" "into" $ C.Native do
    mn <- L.getActiveModule
    sn <- L.getActiveStack
    rv <- L.popWithUnderflow mn sn
    L.into rv
  L.define "core" "into:" $ C.NativeSyntax do
    nw <- L.nextWordTrimmedOrThrowEOF "into:"
    pure [ "\\", nw, "into" ]
  L.define "core" "outof" $ C.Native L.outof

  --| Open or close a module
  L.define "core" "enter" $ C.Native do
    mn <- L.getActiveModule
    sn <- L.getActiveStack
    rv <- L.popWithUnderflow mn sn
    L.enter rv
  L.define "core" "enter:" $ C.NativeSyntax do
    nw <- L.nextWordTrimmedOrThrowEOF "enter:"
    pure [ "\\", nw, "enter" ]
  L.define "core" "leave" $ C.Native L.leave

  --| Push and pull data across stack and module boundaries
  L.define "core" "kiss" $ C.Native do
    mn <- L.getActiveModule
    sn <- L.getActiveStack
    sn' <- L.popWithUnderflow mn sn
    mn' <- L.popWithUnderflow mn sn
    rv <- L.popWithUnderflow mn sn
    L.push mn' sn' rv
  L.define "core" "kiss:" $ C.NativeSyntax do
    mn' <- L.nextWordTrimmedOrThrowEOF "kiss:"
    sn' <- L.nextWordTrimmedOrThrowEOF "kiss:"
    pure [ "\\", mn', "\\", sn', "kiss" ]
  L.define "core" "suck" $ C.Native do
    mn <- L.getActiveModule
    sn <- L.getActiveStack
    sn' <- L.popWithUnderflow mn sn
    mn' <- L.popWithUnderflow mn sn
    rv <- L.popWithUnderflow mn' sn'
    L.push mn sn rv
  L.define "core" "suck:" $ C.NativeSyntax do
    mn' <- L.nextWordTrimmedOrThrowEOF "suck:"
    sn' <- L.nextWordTrimmedOrThrowEOF "suck:"
    pure [ "\\", mn', "\\", sn', "suck" ]

  --| Push and pull data across same-module stack
  L.define "core" "peck" $ C.Native do
    mn <- L.getActiveModule
    sn <- L.getActiveStack
    sn' <- L.popWithUnderflow mn sn
    rv <- L.popWithUnderflow mn sn
    L.push mn sn' rv
  L.define "core" "peck:" $ C.NativeSyntax do
    sn' <- L.nextWordTrimmedOrThrowEOF "peck:"
    pure [ "\\", sn', "peck" ]
  L.define "core" "want" $ C.Native do
    mn <- L.getActiveModule
    sn <- L.getActiveStack
    sn' <- L.popWithUnderflow mn sn
    rv <- L.popWithUnderflow mn sn'
    L.push mn sn rv
  L.define "core" "want:" $ C.NativeSyntax do
    sn' <- L.nextWordTrimmedOrThrowEOF "want:"
    pure [ "\\", sn', "want" ]

