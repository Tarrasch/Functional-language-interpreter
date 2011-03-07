module Env where

import Absgrammar
import Control.Monad.Instances -- For Functor instance of (,)


------------------------ Notes & Overview ------------------------

-- This module define the "variable-environment".

-- The functions defined in this module are all functions working with Env.


------------------------ Types ------------------------

-- Let first pair be global and second local
type Env = (SingleEnv, SingleEnv)

type SingleEnv = [(Ident, LookupValue)]

type LookupValue = Value

data Value = VClojure Exp SingleEnv
           | VInt Integer
  deriving Show

----------------------- Constants ------------------------

emptyEnv :: Env
emptyEnv = ([], [])


----------------------- Modifiers ------------------------

setLocalBindings :: SingleEnv -> Env -> Env
setLocalBindings = fmap . const


----------------------- Queries ------------------------

-- | Search after a name in all the scopes, if there are more than one variable 
--   with the same name, the one latest declared is returned.
envLookup :: Ident -> Env -> Maybe LookupValue
envLookup id (glob, loc) = lookup id (loc ++ glob)


----------------------- Other ------------------------

defsToEnvironment :: [Def] -> Env
defsToEnvironment defs = (map (fmap exp2Value . aux) defs, [])
 where aux :: Def -> (Ident, Exp)
       aux (DefFun fname idents exp0) = (fname, foldr ELambda exp0 idents)
       exp2Value :: Exp -> Value
       exp2Value exp = VClojure exp []
