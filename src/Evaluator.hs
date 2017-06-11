module Evaluator where

import TypeChecker
import Types
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import Data.List as L
import qualified Data.Map as M

-- EVALUATION OF EXPRESSIONS --
type Valuation = Ident -> ValueType


initEnv = Env {
  varEnv = M.empty,
  funEnv = M.empty,
  retVal = VVoid
}

initStore = M.empty :: Store

evalExp :: Expr -> MM ValueType
--evalExp :: Expr -> (ReaderT Valuation (ErrorT String Identity)) ValueType

evalExp (EVar ident) = do
  loc <- asks (\e -> (varEnv e) M.! ident)
  val <- gets (M.! loc)
  return val

evalExp (ELitInt n) = return (VInt n)

evalExp ELitTrue = return (VBool True)

evalExp ELitFalse = return (VBool False)

evalExp (EApp ident []) = do
  st <- get
  env <- ask
  let fun = ((funEnv env) M.! ident)
  env2 <- local (\e -> (Env (varEnv env) (funEnv e) (retVal e))) (execStmt (BStmt (body fun)))
  return (retVal env2)

evalExp (EApp ident exprs) = do
  st <- get
  args_val <- mapM evalExp exprs
  env <- ask
  let fun = ((funEnv env) M.! ident)
  let newlocs = [ (Loc i) | i <- [-(length exprs)..(-1)]]
  let idents = [ids | (Arg _ ids) <- (fargs fun)]
  let args = zip newlocs args_val
  let st1 = M.fromList args
  let env1 = M.fromList $ zip idents newlocs
  modify (\_ -> M.union st st1)
  env2 <- local (\e -> (Env (M.union (varEnv env) env1) (funEnv e) (retVal e))) (execStmt (BStmt (body fun)))
  return (retVal env2)

--  args_val <- mapM evalExp exprs
--  let fun = ((funEnv env) M.! ident)
--  let args_ident = [ids | (Args _ ids) <- (fargs fun)]
--  return VVoid
  -- Stworzyć jej lokalne srodowisko z zewaluowanymi argumentami
  -- wykonac blok funkcji w danym srodowisku


evalExp (EString str) = return (VStr str)

evalExp (Neg expr) = do
  v <- evalExp expr
  return (valNeg v)

evalExp (Not expr) = do
  (VBool v) <- evalExp expr
  return (VBool (not v))

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
  (_, _) -> error "Incorrect relation types!"

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

execStmt :: Stmt -> MM Env
execStmt Empty = do
  env <- ask
  return env

execStmt (BStmt (Block (h:t))) = do
  env <- execStmt h
  local (\_ -> env) (execStmt (BStmt (Block t)))

execStmt (BStmt (Block [])) = do
  env <- ask
  return env

execStmt (Decl type_ (t:h)) = do
  newEnv <- declItem type_ t
  local (\_ -> newEnv) (execStmt (Decl type_ h))

execStmt (Decl type_ []) = do
  env <- ask
  return env

execStmt (Ass ident_ expr) = do
  ev <- evalExp expr
  env <- ask
  let location = (varEnv env) M.! ident_
  modify (M.adjust (\_ -> ev) location)
  return env

execStmt (Ret expr)= do
  env <- ask
  ev <- evalExp expr
  return (Env (varEnv env) (funEnv env) ev)

execStmt VRet = do
  env <- ask
  return (Env (varEnv env) (funEnv env) VVoid)

execStmt (Cond expr stmt)= do
  env <- ask
  (VBool b) <- evalExp expr
  if b then
    execStmt stmt
  else
    return env

execStmt (CondElse expr stmt1 stmt2)= do
  (VBool b) <- evalExp expr
  if b then
    execStmt stmt1
  else
    execStmt stmt2

execStmt (While expr (BStmt (Block (h:t)))) = do
  env <- execStmt (Cond expr h)
  local (\_ -> env) (execStmt (While expr (BStmt (Block t))))

execStmt (While expr (BStmt (Block []))) = do
  env <- ask
  return env

execStmt (While expr stmt) = do
  env <- execStmt (Cond expr stmt)
  return env

execStmt (ForTo (NoInit ident_) expr stmt) = do
  env <- ask
  (VInt ev) <- evalExp expr
  st <- get
  let loc = (varEnv env) M.! ident_
  let (VInt it_val) = (st M.! loc)
  if (it_val <= ev)
  then let newEnv = execStmt stmt in
    modify (M.adjust (\_ -> (VInt (it_val + 1))) loc) >> return env
  else return env

execStmt (ForTo (Init ident_ expr1) expr2 stmt) = do
  env <- execStmt (Ass ident_ expr1)
  (VInt ev) <- evalExp expr2
  st <- get
  let loc = (varEnv env) M.! ident_
  let (VInt it_val) = (st M.! loc)
  if (it_val <= ev)
  then let newEnv = execStmt stmt in
    modify (M.adjust (\_ -> (VInt (it_val + 1))) loc) >> return env
  else return env

execStmt (ForDownTo (NoInit ident_) expr stmt) = do
  env <- ask
  (VInt ev) <- evalExp expr
  st <- get
  let loc = (varEnv env) M.! ident_
  let (VInt it_val) = (st M.! loc)
  if (it_val >= ev)
  then let newEnv = execStmt stmt in
    modify (M.adjust (\_ -> (VInt (it_val - 1))) loc) >> return env
  else return env

execStmt (ForDownTo (Init ident_ expr1) expr2 stmt) = do
  env <- execStmt (Ass ident_ expr1)
  (VInt ev) <- evalExp expr2
  st <- get
  let loc = (varEnv env) M.! ident_
  let (VInt it_val) = (st M.! loc)
  if (it_val >= ev)
  then let newEnv = execStmt stmt in
    modify (M.adjust (\_ -> (VInt (it_val - 1))) loc) >> return env
  else return env

execStmt (SExp expr) = do
  env <- ask
  ev <- evalExp expr
  return env

declItem :: Type -> Item -> MM Env
declItem t (NoInit ident_) = do
  st <- get
  env <- ask
  let newloc = (Loc (M.size st))
  modify (M.insert newloc (defaultVal t))
  return (Env (M.insert ident_ newloc (varEnv env)) (funEnv env) (retVal env))

declItem t (Init ident_ expr) = do
  st <- get
  env <- ask
  v <- evalExp expr
  let newloc = (Loc (M.size st))
  modify (M.insert newloc v)
  return (Env (M.insert ident_ newloc (varEnv env)) (funEnv env) (retVal env))

defaultVal :: Type -> ValueType
defaultVal x = case x of
  (Bool) -> (VBool False)
  (Int) -> (VInt 0)
  (Str) -> (VStr "")
  Void -> VVoid

runEval e = runStateT (runReaderT (evalExp e) initEnv) initStore

execProgram :: Program -> MM Env
execProgram (Program (t:h)) = do
  liftIO $ putStrLn $ "execProgram t"
  env <- execTopDef t
  local (\_ -> env) (execProgram (Program h))

execProgram (Program []) = do
  liftIO $ putStrLn $ "execProgram []"
  env <- ask
  let z = evalExp (EApp (Ident "main") [])
  return env

execTopDef :: TopDef -> MM Env
execTopDef (FnDef typ ident args body) = do
  liftIO $ putStrLn $ "TopDef"
  env <- ask
  return (Env (varEnv env) (M.insert ident (FunType typ ident args body) (funEnv env)) (retVal env))

extP = Program [FnDef Int (Ident "main") [] (Block [Decl Int [Init (Ident "i")
       (ELitInt 666)], While (ERel (EVar (Ident "i")) GTH (ELitInt 0))
       (Ass (Ident "i") (EAdd (EVar (Ident "i")) Plus (ELitInt 1))),Ret (ELitInt 0)])]
