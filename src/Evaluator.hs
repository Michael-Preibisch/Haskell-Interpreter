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
  retVal = VNone
}

initStore = M.empty :: Store

evalExp :: Expr -> MM ValueType
evalExp (EVar ident) = do
  loc <- asks (\e -> (varEnv e) M.! ident)
  val <- gets (M.! loc)
  return val

evalExp (ELitInt n) = return (VInt n)

evalExp ELitTrue = return (VBool True)

evalExp ELitFalse = return (VBool False)

evalExp (EApp ident []) = do
  liftIO $ putStrLn $ "EApp ident []" ++ show ident
  st <- get
  env <- ask
  let fun = ((funEnv env) M.! ident)
  env2 <- local (\e -> (Env (varEnv env) (funEnv e) (retVal e))) (execStmt (BStmt (body fun)))
  return (retVal env2)

evalExp (EApp ident exprs) = do
  liftIO $ putStrLn $ "EApp ident exprs " ++ show ident
  st <- get
  env <- ask
  args_val <- mapM evalExp exprs
  let fun = ((funEnv env) M.! ident)
  let newlocs = [ (Loc i) | i <- [-((length exprs) + M.size st)..(-M.size st)]]
  let idents = [ids | (Arg _ ids) <- (fargs fun)]
  let args = zip newlocs args_val
  let st1 = M.fromList args
  let env1 = M.fromList $ zip idents newlocs
  modify (\_ -> M.union st st1)
  env2 <- local (\e -> (Env (M.union env1 (varEnv env)) (funEnv e) (retVal e))) (execStmt (BStmt (body fun)))
  return (retVal env2)

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

evalExp (EtoString expr) = do
  e <- evalExp expr
  case e of
    (VInt n) -> return (VStr (show n))
    (VBool b) -> return (VStr (show b))
    (VStr s) -> return (VStr s)
    _ -> error "EtoString error"

evalExp (EtoInt expr) = do
  e <- evalExp expr
  case e of
    (VInt n) -> return (VInt n)
    (VBool b) -> return (VInt (if b then 1 else 0))
    (VStr s) -> return (VInt i) where
      i = fst (L.head (reads s ::[(Integer, String)]))
    _ -> error "EtoString error"

-- ARITHMETIC VALUATION
valAdd :: ValueType -> ValueType -> ValueType
valAdd (VInt n1) (VInt n2) = VInt (n1 + n2)
valAdd (VStr s1) (VStr s2) = VStr (s1 ++ s2)
valAdd x y = error $ "Trying to add non string/int values " ++ (show x) ++ "  " ++ (show y)

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
  (VInt i1, VInt i2) -> valRel' v1 op v2
  (VStr s1, VStr s2) -> valRel' v1 op v2
  (VBool b1, VBool b2) -> valRel' v1 op v2
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

execStmt (SPrint expr) = do
  e <- evalExp expr
  case e of
    (VBool e) -> liftIO $ putStrLn $ show e
    (VInt n) -> liftIO $ putStrLn $ show n
    (VStr s) -> liftIO $ putStrLn $ s
  env <- ask
  return env

execStmt (BStmt (Block (h:t))) = do
  env <- execStmt h
  if ((retVal env) == VNone)  then
    local (\_ -> env) (execStmt (BStmt (Block t)))
  else
    return env


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

execStmt (Ret expr) = do
  env <- ask
  ev <- evalExp expr
  return (Env (varEnv env) (funEnv env) ev)

execStmt VRet = do
  env <- ask
  return (Env (varEnv env) (funEnv env) VVoid)

execStmt (Cond expr stmt) = do
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

execStmt (While expr stmt) = do
  (VBool b) <- evalExp expr
  if b then do
    env <- execStmt stmt
    local (\_ -> env) (execStmt (While expr stmt))
  else do
    env <- ask
    return env

execStmt (ForTo (NoInit ident_) expr stmt) = do
  env <- ask
  st <- get
  let loc = (varEnv env) M.! ident_
  let (VInt it_val) = (st M.! loc)
  (VInt ev) <- evalExp expr
  if (it_val < ev) then
    execStmt stmt >>
    modify (M.adjust (\_ -> (VInt (it_val + 1))) loc) >>
    execStmt (ForTo (NoInit ident_) expr stmt)
  else
    if (it_val == ev) then
      execStmt stmt
    else
      return env

execStmt (ForTo (Init ident_ expr1) expr2 stmt) = do
  env <- ask
  st <- get
  let loc = (varEnv env) M.! ident_
  let (VInt it_val) = (st M.! loc)
  (VInt ev) <- evalExp expr1
  modify (M.adjust (\_ -> (VInt ev)) loc)
  execStmt (ForTo (NoInit ident_) expr2 stmt)


execStmt (ForDownTo (NoInit ident_) expr stmt) = do
  env <- ask
  st <- get
  let loc = (varEnv env) M.! ident_
  let (VInt it_val) = (st M.! loc)
  (VInt ev) <- evalExp expr
  if (it_val > ev) then
    execStmt stmt >>
    modify (M.adjust (\_ -> (VInt (it_val - 1))) loc) >>
    execStmt (ForDownTo (NoInit ident_) expr stmt)
  else
    if (it_val == ev) then
      execStmt stmt
    else
      return env

execStmt (ForDownTo (Init ident_ expr1) expr2 stmt) = do
  env <- ask
  st <- get
  let loc = (varEnv env) M.! ident_
  let (VInt it_val) = (st M.! loc)
  (VInt ev) <- evalExp expr1
  modify (M.adjust (\_ -> (VInt ev)) loc)
  execStmt (ForDownTo (NoInit ident_) expr2 stmt)

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

runProgram p = runErrorT (runStateT (runReaderT (execProgram p) initEnv) initStore)

execProgram :: Program -> MM Env
execProgram (Program (t:h)) = do
  env <- execTopDef t
  local (\_ -> env) (execProgram (Program h))

execProgram (Program []) = do
  env <- ask
  local (\_ -> env) (evalExp (EApp (Ident "main") []))
  return env

execTopDef :: TopDef -> MM Env
execTopDef (FnDef typ ident args body) = do
  liftIO $ putStrLn $ "TopDef"
  env <- ask
  return (Env (varEnv env) (M.insert ident (FunType typ ident args body) (funEnv env)) (retVal env))
