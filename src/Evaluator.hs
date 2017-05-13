module Evaluator where

import TypeChecker
import Types
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State

-- EVALUATION OF EXPRESSIONS --
type Valuation = Ident -> ValueType

evalExp :: Expr -> (ReaderT Valuation (ErrorT String Identity)) ValueType

evalExp (EVar ident) = undefined

evalExp (ELitInt n) = return (VInt n)

evalExp ELitTrue = return (VBool True)

evalExp ELitFalse = return (VBool False)

evalExp (EApp ident exprs) = undefined

evalExp (EString str) = return (VStr str)

evalExp (Neg expr) = do
  v <- evalExp expr
  return (valNeg v)

evalExp (Not expr) = undefined

evalExp (EMul e1 Times e2) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  return (valMul v1 v2)

evalExp (EMul e1 Div e2) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  return (valDiv v1 v2)

evalExp (EAdd e1 Plus e2) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  return (valAdd v1 v2)

evalExp (EAdd e1 Minus e2) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  return (valSub v1 v2)

evalExp (ERel e1 op e2) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  return (valRel v1 op v2)

evalExp (EAnd e1 e2) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  return (valAnd v1 v2)

evalExp (EOr e1 e2) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  return (valOr v1 v2)

-- ARITHMETIC VALUATION
valAdd :: ValueType -> ValueType -> ValueType
valAdd (VInt n1) (VInt n2) = VInt (n1 + n2)
valAdd (VStr s1) (VStr s2) = VStr (s1 ++ s2)

valSub :: ValueType -> ValueType -> ValueType
valSub (VInt n1) (VInt n2) = VInt (n1 - n2)

valMul :: ValueType -> ValueType -> ValueType
valMul (VInt n1) (VInt n2) = VInt (n1 * n2)

valDiv :: ValueType -> ValueType -> ValueType
valDiv (VInt n1) (VInt n2) =
  if n2 /= 0
  then VInt (n1 `div` n2)
  else error "Error: Division by 0!"

valOr :: ValueType -> ValueType -> ValueType
valOr (VBool v1) (VBool v2) = VBool (v1 || v2)

valAnd :: ValueType -> ValueType -> ValueType
valAnd (VBool v1) (VBool v2) = VBool (v1 && v2)

valRel :: ValueType -> RelOp -> ValueType -> ValueType
valRel v1 op v2 = case (v1, v2) of
  (VInt i1, VInt i2) -> valRel' v1 op v1
  (VStr s1, VStr s2) -> valRel' v1 op v1
  (VBool b1, VBool b2) -> valRel' v1 op v1

valRel' :: ValueType -> RelOp -> ValueType -> ValueType
valRel' v1 op v2 = case op of
  LTH ->  VBool (v1 < v2)
  LE ->   VBool (v1 <= v2)
  GTH ->  VBool (v1 > v2)
  GE ->   VBool (v1 >= v2)
  EQU ->  VBool (v1 == v2)
  NE ->   VBool (v1 /= v2)

valNeg :: ValueType -> ValueType
valNeg (VInt n) = VInt (-n)

-- STATEMENTS EXECUTION --
execStmt Empty = undefined

execStmt (BStmt stmts) = undefined

execStmt (Decl type_ items) = undefined

execStmt (Ass ident expr) = undefined

execStmt (Ret expr)= undefined

execStmt VRet = undefined

execStmt (Cond expr stmt)= undefined

execStmt (CondElse expr1 stmt expr2)= undefined

execStmt (While expr stmt) = undefined

execStmt (ForTo item expr stmt) = undefined

execStmt (ForDownTo item expr stmt) = undefined

execStmt (SExp expr) = undefined

runEval e = runIdentity (runErrorT (runReaderT (evalExp e) initValuation))
initValuation x = error "Wrong!"

extExps = [EAdd (EString "lol") Plus (EString "hehe"),
          EMul (ELitInt 5) Times (Neg (ELitInt 10)),
          EMul (ELitInt 10) Div (ELitInt 2),
          EMul (ELitInt 1) Div (ELitInt 0)]
extStr = EString "lol"


testEval e = let res = runEval e
    in case res of
        Left rterr -> "Runtime error: " ++ rterr
        Right n -> "Result: " ++ show n
