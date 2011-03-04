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
                          Just exp -> return $ fixIt $ runMonad env (whnf exp)
                          Nothing  -> return $ Left "no main is defined"
  where env     = defsToEnvironment defs
        mainExp = envLookup (Ident "main") env
        fixIt   = fmap (\(EInteger i) -> i)

----------------------------- Value -------------------------------  

data Value = VLambda Ident Exp
           | VInt Integer

liftIntOp :: (Integer -> Integer -> Integer) -> 
             (MyMonad Exp -> MyMonad Exp -> MyMonad Exp)
liftIntOp op mv1 mv2 = do 
  (EInteger i1) <- mv1   
  (EInteger i2) <- mv2   
  return $ EInteger $ i1 `op` i2

----------------------------- Substituting -------------------------------  


----------------------------- Interpreting -------------------------------  

whnf :: Exp -> MyMonad Exp
whnf e = case e of
  ELambda id exp        -> return $ ELambda id exp
  EApply eFun eArg      -> do
    ELambda id eBody <- whnf eFun
    local (addBinding id eArg) (whnf eBody)    
  EIfElse eCond e1 e2   -> do
    EInteger b <- whnf eCond
    whnf (if b /= 0 then e1 else e2)    
  EPlus e1 e2           -> liftIntOp (+) (whnf e1) (whnf e2)
  EMinus e1 e2          -> liftIntOp (-) (whnf e1) (whnf e2)
  ELessThan e1 e2       -> liftIntOp (intLessThan) (whnf e1) (whnf e2)
  EInteger n            -> return $ EInteger n
  EIdent id             -> do
    mExp <- asks $ envLookup id
    case mExp of
      Just exp -> whnf exp
      Nothing  -> fail "variable was unbound when looking up"

  where intLessThan i1 i2 = toInteger . fromEnum $ i1 < i2

