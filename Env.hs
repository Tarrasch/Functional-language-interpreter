module Env where

import Absgrammar
import Data.IORef
import Control.Monad
import Control.Monad.Instances -- For Functor instance of (,)


------------------------ Notes & Overview ------------------------

-- This module define the "variable-environment".

-- The functions defined in this module are all functions working with Env.


------------------------ Types ------------------------

-- Let first pair be global and second local
type Env = (SingleEnv, SingleEnv)

type SingleEnv = [(Ident, LookupValue)]

type LookupValue = IORef Value

data Value = VClojure Exp SingleEnv
           | VInt Integer

----------------------- Constants ------------------------

emptyEnv :: Env
emptyEnv = ([], [])


----------------------- Modifiers ------------------------

setLocalBindings :: SingleEnv -> Env -> Env
setLocalBindings = fmap . const

getLocalBindings :: Env -> SingleEnv
getLocalBindings = snd


----------------------- Queries ------------------------

-- | Search after a name in all the scopes, if there are more than one variable 
--   with the same name, the one latest declared is returned.
envLookup :: Ident -> Env -> Maybe LookupValue
envLookup id (glob, loc) = lookup id (loc ++ glob)


----------------------- Other ------------------------

defsToEnvironment :: [Def] -> IO Env
defsToEnvironment defs = liftM2 (,) (liftM2 zip leftList rightList) (return [])
 where auxId :: Def -> Ident
       auxId (DefFun fname idents exp0) = fname
       auxExp :: Def -> Exp
       auxExp (DefFun fname idents exp0) = foldr ELambda exp0 idents
       exp2Value :: Exp -> IO LookupValue
       exp2Value exp = newIORef $ VClojure exp []
       leftList :: IO [Ident]
       leftList  = return $ map auxId defs
       rightList :: IO [LookupValue]
       rightList = mapM (exp2Value . auxExp) defs
