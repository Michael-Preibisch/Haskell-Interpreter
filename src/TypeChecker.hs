module TypeChecker where

import Types
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Identity
import qualified Data.Map as M

type TT = ReaderT Typing (ErrorT String Identity)

type VEnvT = M.Map Ident Type -- Środowisko zmiennych (typy)

type FEnvT = M.Map Ident FunTyping -- Środowisko funkcji (typy)

data FunTyping = FunTyping {
  retType :: Type, -- Typ zwracany przez funkcje
  argsType :: [Type] -- Typy kolejnych argumentów
}

data Typing = Typing {
  vEnv :: VEnvT, -- Środowisko dla zmiennych:  "Ident -> Type"
  fEnv :: FEnvT -- Środowisko dla funkcji "Ident -> (Type, [Type])"
}

initTyping :: Typing
initTyping = Typing {
  vEnv = M.empty,
  fEnv = M.empty
}


-- EXPRESSIONS CHECK

checkExprType :: Expr -> TT Type
checkExprType (ELitInt _) = return Int

checkExprType (EString _) = return Str

checkExprType ELitTrue = return Bool

checkExprType ELitFalse = return Bool

checkExprType (EMul e1 _ e2) = do
    t1 <- checkExprType e1
    t2 <- checkExprType e2
    if (t1 == Int && t2 == Int) then return Int
    else fail ("Type error: trying to multiply " ++ show t1 ++ " and " ++ show t2 ++ ".")

checkExprType (EAdd e1 _ e2) = do
    t1 <- checkExprType e1
    t2 <- checkExprType e2
    if (t1 == Int && t2 == Int) then return Int
    else if (t1 == Str && t2 == Str) then return Str
    else fail ("Type error: Trying to add " ++ show t1 ++ " and " ++ show t2 ++ ".")

checkExprType (EVar iden) = do
  env <- asks (\e -> (vEnv e))
  return (env M.! iden)

checkExprType (EApp iden args) = do
  fenv <- asks (\e -> (fEnv e))
  argTypes <- mapM checkExprType args
  let funInfo = fenv M.! iden
  if (argTypes == argsType funInfo) then
    return (Fun (retType funInfo) argTypes)
  else
    error "Type error: Wrong function parameters!"

checkExprType (Neg expr) = do
  t1 <- checkExprType expr
  if (t1 /= Int) then
    error "Type error: Cannot negate non-integer!"
  else
    return Int

checkExprType (Not expr) = do
  t1 <- checkExprType expr
  if (t1 /= Bool) then
    error "Type error: Cannot logic-negate non-bool!"
  else
    return Bool

checkExprType (ERel e1 _ e2) = do
  t1 <- checkExprType e1
  t2 <- checkExprType e2
  if (t1 /= t2) then
    error "Type error: Cannot compare to diffrent types!"
  else
    return Bool

checkExprType (EAnd e1 e2) = do
  t1 <- checkExprType e1
  t2 <- checkExprType e2
  if (t1 /= Bool || t2 /= Bool) then
    error "Type error: Cannot 'and' non-bool!"
  else
    return Bool

checkExprType (EOr e1 e2) = do
  t1 <- checkExprType e1
  t2 <- checkExprType e2
  if (t1 /= Bool || t2 /= Bool) then
    error "Type error: Cannot 'or' non-bool!"
  else
    return Bool

-- STATEMENTS CHECK

checkStmtType :: Stmt -> TT (Maybe Typing)
checkStmtType Empty = do
  env <- ask
  return (Just env)

checkStmtType (BStmt (Block stmts)) = do
  checkStmtListType stmts

checkStmtType (Decl t items) = do
  checkDeclListType t items

checkStmtType (Ass ident_ expr) = do
  env <- ask
  et <- checkExprType expr
  let varType = M.lookup ident_ (vEnv env)
  case varType of
    Nothing -> error "Declaration error: Assign to non-declared variable!"
    Just t -> if (et == t) then
      return (Just env)
    else
      error "Declaration error: Types do not match on assign!"

checkStmtType (Ret expr) = undefined
checkStmtType VRet = undefined
checkStmtType (Cond expr stmt) = do
  e <- checkExprType expr
  if (e /= Bool) then
    error "Type error: Non-boolean condition!"
  else
    checkStmtType stmt
checkStmtType (CondElse expr stmt1 stmt2) = do
  e <- checkExprType expr
  if (e /= Bool) then
    error "Type error: Non-boolean condition!"
  else
    checkStmtType stmt1 >> checkStmtType stmt2
checkStmtType (While expr stmt) = do
  e <- checkExprType expr
  if (e /= Bool) then
    error "Type error: Non-boolean condition!"
  else
    checkStmtType stmt

checkStmtType (ForTo (NoInit ident_) expr stmt) = undefined

checkStmtType (ForTo (Init ident_ ex) expr stmt) = undefined

checkStmtType _ = undefined
checkStmtType _ = undefined
checkStmtType _ = undefined


checkStmtListType :: [Stmt] -> TT (Maybe Typing)
checkStmtListType (h:t) = do
  newEnv <- checkStmtType h
  case newEnv of
    Nothing -> checkStmtListType t
    Just e -> local (\_ -> e) (checkStmtListType t)

checkDeclListType :: Type -> [Item] -> TT (Maybe Typing)
checkDeclListType t (h:r) = do
  newEnv <- checkDeclType t h
  case newEnv of
    Nothing -> error "Error with multiple declarations!"
    Just e -> local (\_ -> e) (checkDeclListType t r)

checkDeclType :: Type -> Item -> TT (Maybe Typing)
checkDeclType t (NoInit ident_) = do
  env <- ask
  case (M.lookup ident_ (vEnv env)) of
    Just e -> error "Declaration error: variable already declared!"
    Nothing -> return (Just $ Typing (M.insert ident_ t (vEnv env)) (fEnv env))

checkDeclType t (Init ident_ expr) = do
  env <- ask
  et <- checkExprType expr
  case (M.lookup ident_ (vEnv env)) of
    Just e -> error "Declaration error: variable already declared!"
    Nothing -> if (et == t) then
        return (Just $ Typing (M.insert ident_ t (vEnv env)) (fEnv env))
      else
        error "Declaration error: Type of expression do not match with type declared!"



showType :: Type -> String
showType Int = show "Int"
showType Bool = show "Bool"
showType Str = show "Bool"
showType _ = show "Unknown"

runCheck :: Expr -> Either String Type
runCheck e = runIdentity (runErrorT (runReaderT (checkExprType e) initTyping))
--initTyping x = Void

extType :: Expr
extType = EVar (Ident "x")
