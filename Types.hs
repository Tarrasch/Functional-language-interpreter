module Types where

import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Identity

import Absgrammar



-- I've put this module seperate to avoid circle references at module level

------------------------ Types ------------------------

type MyMonad a =  (ReaderT Env
                    (ErrorT ErrorMessage
                      IO))
                        a

 
type ErrorMessage = String

type Env = [(Ident, LookupType)]

type LookupType = Value

data Value = VClojure Exp Env
           | VInt Integer
  deriving Show

