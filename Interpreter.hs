module Interpreter (interpret) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Absgrammar
import Lexgrammar
import Pargrammar
import Printgrammar

import MyMonad
import Env

------------------------ Notes & Overview ------------------------

-- This module contains most of the code, and is the module that are using
-- the other smaller modules.

-- Interpret is the same as Execution.


----------------------------- Run-Function -------------------------------  

-- | Interprets a given program. 
--   The IO contains no side effects, it's safe to use unsafePerformIO on it.
interpret :: Program -> IO (Either ErrorMessage Integer)
interpret (Prog defs) = case mainExp of
                          Just exp -> return $ fixIt $ runMonad env (calcExp exp)
                          Nothing  -> return $ Left "no main is defined"
  where env     = defsToEnvironment defs
        mainExp = envLookup (Ident "main") env
        fixIt   = fmap (\(VInt i) -> i)

----------------------------- Value -------------------------------  

data Value = VLambda Ident Exp
           | VInt Integer

liftIntOp :: (Integer -> Integer -> Integer) -> 
             (MyMonad Value -> MyMonad Value -> MyMonad Value)
liftIntOp op mv1 mv2 = do 
  (VInt i1) <- mv1   
  (VInt i2) <- mv2   
  return $ VInt $ i1 `op` i2
 
----------------------------- Substituting -------------------------------  


----------------------------- Interpreting -------------------------------  

calcExp :: Exp -> MyMonad Value
calcExp e = case e of
  ELambda id exp        -> return (VLambda id exp)
  EApply eFubn eArg     -> do
    VLambda id eBody <- calcExp eFun
    local (addBinding id eArg) (calcExp eBody)    
  EIfElse eCond e1 e2   -> do
    VInt b <- calcExp eCond
    calcExp (if b /= 0 then e1 else e2)    
  EPlus e1 e2           -> liftIntOp (+) (calcExp e1) (calcExp e2)
  EMinus e1 e2          -> liftIntOp (-) (calcExp e1) (calcExp e2)
  ELessThan e1 e2       -> fail "< not defined"
  EInteger n            -> return $ VInt n
  EIdent id             -> do
    mExp <- asks $ envLookup id
    case mExp of
      Just exp -> calcExp exp
      Nothing  -> fail "variable was unbound when looking up"


{-
whnf :: Exp -> MyMonad Exp
whnf (EApply (ELambda id eBody) eArg) = e
whnf e = e
-}

