module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import Absgrammar
import Lexgrammar
import Pargrammar
import ErrM

import Interpreter


------------------------ Notes & Overview ------------------------



------------------------------------------------------------------

-- driver

check :: String -> IO () 
check s = case pProgram (myLexer s) of
            Bad err  -> do putStrLn "SYNTAX ERROR"
                           putStrLn err
                           exitFailure 
            Ok  tree -> do
              res <- interpret tree
              case res of
                Left errMsg -> do
                  putStrLn "ERROR, gave message:"
                  putStrLn errMsg
                  exitFailure 
                Right value -> do
                  putStrLn "Sucess, main returned value:"
                  print value

main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> readFile file >>= check
            _      -> do putStrLn "Usage: lab4 <SourceFile>"
                         exitFailure
