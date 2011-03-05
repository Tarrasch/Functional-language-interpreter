module MyMonad where

import Env
import Absgrammar

import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Identity


------------------------ Notes & Overview ------------------------

-- The monad is inspired a bit from http://www.cse.chalmers.se/edu/course/afp/lectures/lecture5/Interpreter3.hs.html

-- In this module I've defined the monad and added some help-functions.

-- Everything is exported deliberetly.


------------------------ Types ------------------------

type MyMonad a =  (ReaderT FullEnv
                    (ErrorT ErrorMessage
                      IO))
                        a


type FullEnv = Env -- remove or fix laater
 
type ErrorMessage = String


--------------------------- Running and Unwrapping --------------------------

-- | The most sensible unwrapper, the Symbolic Table and the expected return 
--   type are supplied for parameters, and the execution starts with an empty
--   variable-environment.
runMonad :: Env -> MyMonad a -> IO (Either ErrorMessage a)
runMonad env =  
            runErrorT
          . startReaderFrom env
  where
    startReaderFrom :: env -> ReaderT env m a -> m a
    startReaderFrom = flip runReaderT



--------------------------- Useful functions --------------------------

-- | Fails with the given ErrorMessage if the first arguement is false
msgGuard :: Bool -> ErrorMessage -> MyMonad ()
msgGuard b msg = when (not b) (fail msg)


