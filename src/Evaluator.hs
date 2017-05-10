module Evaluator where

import Control.Monad.State

evalExp :: Exp -> (ReaderT Valuation (ErrorT String Identity)) Value

evalExp (EVar ident) = undefined

evalExp (ELitInt intval) = undefined

evalExp ELitTrue = undefined
evalExp ELitFalse = undefined
evalExp (EApp Ident exprs)

evalExp (EInt n) =
  return (VInt n)

evalExp (EStr str) =
  return (VStr str)

evalExp (EAdd e1 e2) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  return (vAdd v1 v2)

evalExp (EMul e1 e2) = do
  n1 <- evalExp e1
  n2 <- evalExp e2
  return (vMul n1 n2)

evalExp (ECon e1 e2) = do
  str1 <- evalExp e1
  str2 <- evalExp e2
  return (vCon str1 str2)
