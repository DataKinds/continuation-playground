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
  L.define "main" "help" $ C.Native do
    l <- map liftEffect <$> L.getLogger
    l "need help?!" 
  L.define "main" ".." $ C.Native do
    l <- map liftEffect <$> L.getLogger
    sn <- L.dumpOpenStack
    l $ show sn
  L.define "main" "\\" $ C.NativeSyntax do
    nw <- L.nextWordTrimmedOrThrowEOF "backslash"
    mn <- L.getActiveModule
    sn <- L.getOpenStack
    L.push mn sn nw
    pure []

  --| Open a stack
  L.define "main" "into" $ C.Native do
    mn <- L.getActiveModule
    sn <- L.getOpenStack
    maybeRv <- L.pop mn sn
    case maybeRv of
      Nothing -> throwError $ C.Underflow mn sn
      Just rv -> L.into rv
  L.define "main" ">" $ C.NativeSyntax do
    nw <- L.nextWordTrimmedOrThrowEOF ">"
    pure [ "\\", nw, "into" ]

  --| Open a module
  L.define "main" "enter" $ C.Native do
    mn <- L.getActiveModule
    sn <- L.getOpenStack
    rv <- L.pop mn sn
    case rv of
      Nothing -> throwError $ C.Underflow mn sn
      Just rv' -> L.enter rv'
  L.define "main" "!>" $ C.NativeSyntax do
    nw <- L.nextWordTrimmedOrThrowEOF "!>"
    pure [ "\\", nw, "enter" ]
