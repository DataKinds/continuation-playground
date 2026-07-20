module Common where

import Control.Monad.Trans.Class
import Prelude

import Control.Monad.Except (ExceptT)
import Control.Monad.State (StateT)
import Data.Array (intercalate)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Foldable (surround)
import Data.Generic.Rep (class Generic)
import Data.HashMap (HashMap)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (Error)


data RValue = Term String | Quote (Array RValue) -- Runtime value
instance Show RValue where
  show (Term s) = s
  show (Quote rvs) = "[" <> surround " " (show <$> rvs) <> "]"
type RStack = Array RValue
type ModuleName = String
type StackName = String
type WordName = String

data Definition evalM = Native (evalM Unit) | Canon (Array WordName) | NativeSyntax (evalM (Array WordName)) | CanonSyntax (Array WordName)
instance showDefinition :: Show (Definition evalM) where
    show (Native _) = "<native code>" 
    show (Canon words) = joinWith " " words
    show (NativeSyntax _) = "<native macro>" 
    show (CanonSyntax words) = "MACRO: " <> (joinWith " " words)


type Module evalM =
  { chain :: Array ModuleName
  , defs :: HashMap WordName (Definition evalM)
  , stacks :: HashMap StackName RStack
  , openStacks :: NonEmptyArray StackName
  }

data VMError
  = UnknownWord (Array ModuleName) WordName -- Lookup failure
  | EOF String -- Syntax word tried to read too many tokens
  | Underflow ModuleName StackName -- Stack underflow
  | WhatsThat ModuleName StackName String String -- Value or type error in a given stack. First arg expected, second arg recieved
  | UnknownError String
instance Show VMError where
  show (UnknownWord mn wn) = "Unknown word " <> wn <> " in modules " <> (show mn)
  show (EOF s) = "EOF " <> s
  show (Underflow mn sn) = "Underflow " <> mn <> "." <> sn
  show (WhatsThat mn sn wanted got) = "Popped an " <> got <> " off of " <> mn <> "." <> sn <> ", but wanted a " <> wanted
  show (UnknownError s) = "Unknown " <> s

newtype RealState = RealState
  { modules :: HashMap ModuleName (Module RealEval)
  , openModules :: NonEmptyArray ModuleName
  , source :: Array String
  , sourceIx :: Int
  , errorHandler :: VMError -> Effect Unit
  , outputHandler :: String -> Effect Unit
  }

derive instance newtypeRealState :: Newtype RealState _


type RealEval = ExceptT VMError (StateT RealState Aff)

