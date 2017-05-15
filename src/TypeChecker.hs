module TypeChecker where

import Types
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Identity
import qualified Data.Map as M
import qualified Data.List as L

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
} deriving Show

initTyping :: Typing
initTyping = Typing {
  vEnv = M.empty,
  fEnv = M.empty
}

instance Show FunTyping where
  show (FunTyping a b) = show a ++ " " ++ show b
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

checkStmtType (ForTo (NoInit ident_) expr stmt) = do
  it <- checkExprType (EVar ident_)
  e <- checkExprType expr
  if (it /= Int) || (e /= Bool) then
    error "Type error: ForTo loop with non-integer iterator or non-bool condition!"
  else
    checkStmtType stmt

checkStmtType (ForTo (Init ident_ ex) expr stmt) =  do
  it <- checkExprType (EVar ident_)
  ext <- checkExprType ex
  e <- checkExprType expr
  if (it /= Int) || (e /= Bool) || (ext /= Int) then
    error "Type error: ForTo loop with non-integer iterator or non-bool condition!"
  else
    checkStmtType stmt

checkStmtType (ForDownTo (NoInit ident_) expr stmt) = do
  it <- checkExprType (EVar ident_)
  e <- checkExprType expr
  if (it /= Int) || (e /= Bool) then
    error "Type error: ForDownTo loop with non-integer iterator or non-bool condition!"
  else
    checkStmtType stmt


checkStmtType (ForDownTo (Init ident_ ex) expr stmt) =  do
  it <- checkExprType (EVar ident_)
  ext <- checkExprType ex
  e <- checkExprType expr
  if (it /= Int) || (e /= Bool) || (ext /= Int) then
    error "Type error: ForDownTo loop with non-integer iterator or non-bool condition!"
  else
    checkStmtType stmt

checkStmtType (SExp expr) = do
  e <- checkExprType expr
  x <- ask
  if (e /= Void) then
    error "Type error: Non-void expression used as statement!"
  else
    return (Just x)

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

-- CHECK FUNCTION DECLARATIONS

checkTopDefType :: TopDef -> TT (Maybe Typing)
checkTopDefType (FnDef typ ident_ args (Block body)) = do
  newEnv <- putFunction typ ident_ args
  case newEnv of
    Just e -> local (\_ -> e) (checkStmtType (BStmt (Block body)))
    Nothing -> error "Declaration error!"

-- Zwróci środowisko zaktualizowane o nową funkcje
putFunction :: Type -> Ident -> [Arg] -> TT (Maybe Typing)
putFunction typ ident_ args = do
  let argsT = checkArgs args
  let argsM = M.fromList [(k, a) | (Arg a k) <- args]
  env <- ask
  case (M.lookup ident_ (fEnv env)) of
    Just e -> error "Declaration error: Function redeclaration!"
    Nothing -> return (Just $ Typing (M.union (vEnv env) argsM) (M.insert ident_ (FunTyping typ argsT) (fEnv env)))

checkArgs :: [Arg] -> [Type]
checkArgs args = if (length $ L.nub [ids | (Arg _ ids ) <- args]) /= length args then
    error "Declaration error: Function's parameters indentifiers are not unique!"
  else
    [tps | (Arg tps _) <- args]


-- CHECK PROGRAM
checkProgram :: Program -> TT (Maybe Typing)
checkProgram (Program (t:h)) = do
  env <- checkTopDefType t
  case env of
    Nothing -> error "Error with program typecheck!"
    Just e -> local (\_ -> e) (checkProgram (Program h))

showType :: Type -> String
showType Int = show "Int"
showType Bool = show "Bool"
showType Str = show "Bool"
showType _ = show "Unknown"

runCheck :: Expr -> Either String Type
runCheck e = runIdentity (runErrorT (runReaderT (checkExprType e) initTyping))
--initTyping x = Void


runProgramCheck p = runIdentity (runErrorT (runReaderT (checkProgram p) initTyping))

extType :: Expr
extType = EVar (Ident "x")

extArgs = [Arg Int (Ident "x")]

p1 = (Program [])
extProg = (Program [FnDef Int (Ident "main") [Arg Int (Ident "x")] (Block [Ret (EAdd (EVar (Ident "x")) Plus (ELitInt 111))])])
