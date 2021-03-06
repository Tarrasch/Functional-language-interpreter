module Env where

import Absgrammar


------------------------ Notes & Overview ------------------------

-- This module define the "variable-environment".

-- The functions defined in this module are all functions working with Env.


------------------------ Types ------------------------

type Env = [(Ident, Exp)]

----------------------- Constants ------------------------

emptyEnv :: Env
emptyEnv = []


----------------------- Modifiers ------------------------

addBinding :: Ident -> Exp -> Env -> Env
addBinding x val = ((x, val):)


----------------------- Queries ------------------------

inScope :: Ident -> Env -> Bool
inScope x = elem x . map fst


-- | Search after a name in all the scopes, if there are more than one variable 
--   with the same name, the one latest declared is returned.
envLookup :: Ident -> Env -> Maybe Exp
envLookup = lookup


----------------------- Other ------------------------

defsToEnvironment :: [Def] -> Env
defsToEnvironment = map aux
 where aux :: Def -> (Ident, Exp)
       aux (DefFun fname idents exp0) = (fname, foldr ELambda exp0 idents)

