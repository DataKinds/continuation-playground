module Common where

import Control.Monad.Trans.Class
import Prelude

import Control.Monad.Except (ExceptT)
import Control.Monad.State (StateT)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.HashMap (HashMap)
import Data.Newtype (class Newtype)
import Data.String (joinWith)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (Error)


type RValue = String -- Runtime value
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

newtype RealState = RealState
  { modules :: HashMap ModuleName (Module RealEval)
  , openModules :: NonEmptyArray ModuleName
  , source :: Array String
  , sourceIx :: Int
  , errorHandler :: Error -> Effect Unit
  , outputHandler :: String -> Effect Unit
  }

derive instance newtypeRealState :: Newtype RealState _

type RealEval = StateT RealState (ExceptT Error Aff) -- TODO: custom Error datatype, with MonadThrow/MonadCatch OurError Effect instances

