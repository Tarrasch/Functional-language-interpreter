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
                          Just exp -> runMonad env $ (calcExp exp) >>= calculate
                          Nothing  -> return $ Left "no main is defined"
  where env     = defsToEnvironment defs
        mainExp = envLookup (Ident "main") env

----------------------------- Value -------------------------------  

data Value = VClojure Exp Env
           | VInt Integer
  deriving Show

liftIntOp :: (Integer -> Integer -> Integer) -> 
             (MyMonad Value -> MyMonad Value -> MyMonad Value)
liftIntOp op mv1 mv2 = do 
  asks (length) >>= debug
  asks (lookup (Ident "x")) >>= debug
  i1 <- mv1 >>= calculate   
  i2 <- mv2 >>= calculate   
  return $ VInt $ i1 `op` i2
 
----------------------------- Substituting -------------------------------  
 

----------------------------- Interpreting -------------------------------  

calculate :: Value -> MyMonad Integer
calculate val = debug val >> case val of 
  (VInt i)                   -> return i
  (VClojure (ELambda _ _) _) -> fail "Can't calculate a lambda abstraction!"
  (VClojure exp env')        -> local (env'++) $ (calcExp exp) >>= calculate


calcExp :: Exp -> MyMonad Value
calcExp e = debugTree e >> whnf e >>= \e' -> case e' of
  ELambda id exp        -> return $ VClojure (ELambda id exp) []
  EInteger n            -> return $ VInt n
  otherwise             -> fail "whnf didn't get to whnf" 


whnf :: Exp -> MyMonad Exp
whnf e = case e of
  EApply eFun eArg      -> do
    VClojure (ELambda id eBody) env' <- calcExp eFun
    eArg' <- whnf eArg
    local (((id, eArg') : env') ++) (whnf eBody)
  EIfElse eCond e1 e2   -> do
    b <- calcExp eCond >>= calculate
    whnf (if b /= 0 then e1 else e2)    
  EPlus e1 e2           -> liftIntOp' (+) (calcExp e1) (calcExp e2)
  EMinus e1 e2          -> liftIntOp' (-) (calcExp e1) (calcExp e2)
  ELessThan e1 e2       -> liftIntOp' intLessThan (calcExp e1) (calcExp e2)
  EIdent id             -> do
    mExp <- asks $ envLookup id
    case mExp of
      Just exp -> whnf exp
      Nothing  -> fail $ "variable " ++ show id ++ " was unbound when looking up"
  integerOrAbstraction  -> return integerOrAbstraction
  
  where liftIntOp' f a b = liftIntOp f a b >>= (\(VInt i) -> return $ EInteger i)

intLessThan :: Integer -> Integer -> Integer
intLessThan i1 i2 = toInteger . fromEnum $ i1 < i2  
  
debug :: Show a => a -> MyMonad ()
debug = liftIO . print

debugTree = liftIO . print . printTree
