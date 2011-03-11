module Absgrammar where

-- Haskell module generated by the BNF converter

newtype Ident = Ident String deriving (Eq,Ord,Show)
data Program =
   Prog [Def]
  deriving (Eq,Ord,Show)

data Def =
   DefFun Ident [Ident] Exp
  deriving (Eq,Ord,Show)

data Exp =
   ELambda Ident Exp
 | EIfElse Exp Exp Exp
 | EPlus Exp Exp
 | EMinus Exp Exp
 | ELessThan Exp Exp
 | EApply Exp Exp
 | EInteger Integer
 | EIdent Ident
  deriving (Eq,Ord,Show)
