module Interpreter (interpret) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Data.IORef
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
interpret (Prog defs) = case mainVal of
                          Just ioVal -> ioVal >>= (runMonad env . calculate)
                          Nothing    -> return $ Left "no main is defined"
  where env     = defsToEnvironment defs
        mainVal = envLookup (Ident "main") env

----------------------------- Value -------------------------------  

liftIntOp :: (Integer -> Integer -> Integer) -> 
             (MyMonad Value -> MyMonad Value -> MyMonad Value)
liftIntOp op mv1 mv2 = do 
  i1 <- mv1 >>= calculate   
  i2 <- mv2 >>= calculate   
  return $ VInt $ i1 `op` i2
 

----------------------------- Interpreting -------------------------------  

calculate :: Value -> MyMonad Integer
calculate val = whnf val >>= \val' -> case val' of 
    (VInt i) -> return i
    _        -> fail "Can't calculate a lambda abstraction!"

whnf :: Value -> MyMonad Value
whnf val = case val of 
  (VInt i)                   -> return val
  (VClojure (ELambda _ _) _) -> return val
  (VClojure exp env')        -> local (env'++) $ (calcExp exp) >>= whnf

memorize :: Value -> MyMonad (IO Value)
memorize val0 = do 
  currEnv <- ask
  ioRef <- liftIO $ newIORef val0
  return $ do val <- readIORef ioRef
              val' <- whnf 
    

calcExp :: Exp -> MyMonad Value
calcExp e = case e of
  ELambda id exp        -> return $ VClojure (ELambda id exp) []
  EApply eFun eArg      -> do
    VClojure (ELambda id eBody) env' <- calcExp eFun
    vIOArg <- memorize (VClojure eArg [])
    return $ VClojure eBody ((id, vIOArg) : env')
  EIfElse eCond e1 e2   -> do
    b <- calcExp eCond >>= calculate
    calcExp (if b /= 0 then e1 else e2)    
  EPlus e1 e2           -> liftIntOp' (+) e1 e2
  EMinus e1 e2          -> liftIntOp' (-) e1 e2
  ELessThan e1 e2       -> liftIntOp' intLessThan e1 e2
  EInteger n            -> return $ VInt n
  EIdent id             -> do
    mIOVal <- asks $ envLookup id
    case mIOVal of
      Just ioVal -> liftIO ioVal 
      Nothing    -> fail $ "variable " ++ show id ++ " was unbound when looking up"
  where liftIntOp' f e1 e2 = liftIntOp f (calcExp e1) (calcExp e2)
        intLessThan i1 i2 = toInteger . fromEnum $ i1 < i2
 


 
----------------------------- Debugging -------------------------------  

debug :: Show a => a -> MyMonad ()
debug = liftIO . print

debugTree :: Exp -> MyMonad ()
debugTree = liftIO . print . printTree
