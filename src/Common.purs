module Common where

import Control.Monad.Trans.Class
import Prelude

import Control.Monad.Except (ExceptT)
import Control.Monad.State (StateT)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.HashMap (HashMap)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Exception (Error)


type RValue = String -- Runtime value
type RStack = Array RValue
type ModuleName = String
type StackName = String
type WordName = String

data Definition evalM = Native (evalM Unit) | Canon (Array String)

type Module evalM =
  { chain :: Array ModuleName
  , defs :: HashMap WordName (Definition evalM)
  , stacks :: HashMap StackName RStack
  }

newtype RealState = RealState
  { modules :: HashMap ModuleName (Module RealEval)
  , openModules :: NonEmptyArray ModuleName
  , source :: Array String
  , sourceIx :: Int
  }

derive instance newtypeRealState :: Newtype RealState _

type RealEval = StateT RealState (ExceptT Error Effect) -- TODO: custom Error datatype, with MonadThrow/MonadCatch OurError Effect instances
