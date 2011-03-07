module Env where

import Absgrammar
import Control.Monad.Instances -- For Functor instance of (,)


------------------------ Notes & Overview ------------------------

-- This module define the "variable-environment".

-- The functions defined in this module are all functions working with Env.


------------------------ Types ------------------------

type Env = [(Ident, IO Value)]

data Value = VClojure Exp Env
           | VInt Integer

----------------------- Constants ------------------------

emptyEnv :: Env
emptyEnv = []


----------------------- Modifiers ------------------------

addBinding :: Ident -> IO Value -> Env -> Env
addBinding x val = ((x, val):)


----------------------- Queries ------------------------

inScope :: Ident -> Env -> Bool
inScope x = elem x . map fst


-- | Search after a name in all the scopes, if there are more than one variable 
--   with the same name, the one latest declared is returned.
envLookup :: Ident -> Env -> Maybe (IO Value)
envLookup = lookup


----------------------- Other ------------------------

defsToEnvironment :: [Def] -> Env
defsToEnvironment = map (fmap (return . exp2Value) . aux)
 where aux :: Def -> (Ident, Exp)
       aux (DefFun fname idents exp0) = (fname, foldr ELambda exp0 idents)
       exp2Value :: Exp -> Value
       exp2Value exp = VClojure exp []
