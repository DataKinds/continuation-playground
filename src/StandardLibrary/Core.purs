module StandardLibrary.Core where

import Data.Array
import Data.Maybe
import Prelude

import Common as C
import Control.Monad.Error.Class (throwError)
import Data.String (joinWith, trim)
import Debug (trace, traceM)
import Effect.Class (liftEffect)
import Lang as L

-- Generic function to consume everything between a balanced pair of words
poploop :: forall m. L.MonadVM m => String -> String -> m (Array String)
poploop openWord closeWord = go 0 []
  where
  go :: Int -> Array String -> m (Array String)
  go depth words = do
    word <- L.nextWordOrThrowEOF openWord
    let escapedCloseWord = "\\"<>closeWord
    trace { word, depth } \_ -> case unit of
      _
        | word == closeWord, depth <= 0 -> pure $ reverse words
        | word == openWord -> go (depth + 1) $ word:words
        | word == closeWord -> go (depth - 1) $ word:words
        | word == escapedCloseWord -> go depth $ closeWord:words
        | otherwise -> go depth $ word:words

--| Load up the standard library.
gainKnowledge :: forall m. L.MonadVM m => L.MonadSwappableLogger C.VMError m => m Unit
gainKnowledge = do
  -- Core knowledge!~ --
  L.depend "main" "core"
  L.define "core" "help" $ C.Native do
    l <- map liftEffect <$> L.getLogger
    l "need help?!" 

  -- Base syntax --
  
  --| QQ next word
  L.define "core" "\\" $ C.NativeSyntax do
    nw <- L.nextWordTrimmedOrThrowEOF "backslash"
    mn <- L.getActiveModule
    sn <- L.getActiveStack
    L.push mn sn (C.Term nw)
    pure []
  --| Consume a quotation
  L.define "core" "[" $ C.NativeSyntax do
    nws <- poploop "[" "]"
    mn <- L.getActiveModule
    sn <- L.getActiveStack
    L.push mn sn (C.Quote $ trim $ joinWith "" nws) -- trim cuz the spaces between the [ ] and the string get ingested by nextWordOrThrowEOF
    pure []

  -- Multistack navigation --

  --| Open a stack
  L.define "core" "into" $ C.Native do
    mn <- L.getActiveModule
    sn <- L.getActiveStack
    rv <- L.popTermWithUnderflow mn sn
    L.into rv
  L.define "core" "into:" $ C.NativeSyntax do
    nw <- L.nextWordTrimmedOrThrowEOF "into:"
    pure [ "\\", nw, "into" ]
  --| Close a stack
  L.define "core" "outof" $ C.Native L.outof
  --| Push data into a stack in the same module
  L.define "core" "peck" $ C.Native do
    mn <- L.getActiveModule
    sn <- L.getActiveStack
    sn' <- L.popTermWithUnderflow mn sn
    rv <- L.popWithUnderflow mn sn
    L.push mn sn' rv
  L.define "core" "peck:" $ C.NativeSyntax do
    sn' <- L.nextWordTrimmedOrThrowEOF "peck:"
    pure [ "\\", sn', "peck" ]
  --| Pull data from a stack in the same module
  L.define "core" "want" $ C.Native do
    mn <- L.getActiveModule
    sn <- L.getActiveStack
    sn' <- L.popTermWithUnderflow mn sn
    rv <- L.popWithUnderflow mn sn'
    L.push mn sn rv
  L.define "core" "want:" $ C.NativeSyntax do
    sn' <- L.nextWordTrimmedOrThrowEOF "want:"
    pure [ "\\", sn', "want" ]

  -- Module navigation --

  --| Open or close a module
  L.define "core" "enter" $ C.Native do
    mn <- L.getActiveModule
    sn <- L.getActiveStack
    rv <- L.popTermWithUnderflow mn sn
    L.enter rv
  L.define "core" "enter:" $ C.NativeSyntax do
    nw <- L.nextWordTrimmedOrThrowEOF "enter:"
    pure [ "\\", nw, "enter" ]
  L.define "core" "leave" $ C.Native L.leave
  --| Push and pull data across stack and module boundaries
  L.define "core" "kiss" $ C.Native do
    mn <- L.getActiveModule
    sn <- L.getActiveStack
    sn' <- L.popTermWithUnderflow mn sn
    mn' <- L.popTermWithUnderflow mn sn
    rv <- L.popWithUnderflow mn sn
    L.push mn' sn' rv
  L.define "core" "kiss:" $ C.NativeSyntax do
    mn' <- L.nextWordTrimmedOrThrowEOF "kiss:"
    sn' <- L.nextWordTrimmedOrThrowEOF "kiss:"
    pure [ "\\", mn', "\\", sn', "kiss" ]
  L.define "core" "suck" $ C.Native do
    mn <- L.getActiveModule
    sn <- L.getActiveStack
    sn' <- L.popTermWithUnderflow mn sn
    mn' <- L.popTermWithUnderflow mn sn
    rv <- L.popWithUnderflow mn' sn'
    L.push mn sn rv
  L.define "core" "suck:" $ C.NativeSyntax do
    mn' <- L.nextWordTrimmedOrThrowEOF "suck:"
    sn' <- L.nextWordTrimmedOrThrowEOF "suck:"
    pure [ "\\", mn', "\\", sn', "suck" ]

  -- Definitions --

  --| Define an immediate word by ingesting a term and a quote
  L.define "core" "define" $ C.Native do
    mn <- L.getActiveModule
    sn <- L.getActiveStack
    body <- L.popQuoteWithUnderflow mn sn
    name <- L.popTermWithUnderflow mn sn
    L.define mn name $ C.Canon $ L.lex body
  L.define "core" ":" $ C.NativeSyntax do
    name <-  L.nextWordTrimmedOrThrowEOF ":"
    body <- poploop ":" ";"
    pure $ [ "\\", name, "[" ] <> body <> [ "]", "define" ]

