module TypeChecker where

import Types
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Identity
import qualified Data.Map.Lazy as M
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
  fEnv :: FEnvT, -- Środowisko dla funkcji "Ident -> (Type, [Type])"
  currRet :: Type -- Typ zwracany przez aktualnie badaną funkcję
} deriving Show

initTyping :: Typing
initTyping = Typing {
  vEnv = M.empty,
  fEnv = M.empty,
  currRet = Void -- Tak naprawde to undefined
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
  case (M.lookup iden env) of
    Nothing -> error "Error: Undeclared variable!"
    Just e -> return e

checkExprType (EApp iden args) = do
  fenv <- asks (\e -> (fEnv e))
  argTypes <- mapM checkExprType args
  case (M.lookup iden fenv) of
    Just e -> if argTypes == argsType e then
      return (retType e)
    else
      error "Type error: Wrong function parameters!"
    Nothing -> error ("Function " ++ (getName iden) ++ " undeclared!")
{- let funInfo = (fenv M.! iden)  in case funInfo of
    Just _ ->  if (argTypes == argsType funInfo) then
        return (Fun (retType funInfo) argTypes)
      else
        error "Type error: Wrong function parameters!"
    Nothing -> error "Error: Calling non-declared function!"
-}

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

checkStmtType (Ret expr) = do
  et <- checkExprType expr
  env <- ask
  if (et /= (currRet env)) then
    error "Return type does not match with function declaration!"
  else
    return (Just env)

checkStmtType VRet = do
  env <- ask
  if (Void /= (currRet env)) then
    error "Return type does not match with function declaration!"
  else
    return (Just env)

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
  if (it /= Int) || (e /= Int) then
    error ("Type error: ForTo loop with non-integer iterator or condition!" ++ (show e) ++ " " ++ (show it))
  else
    checkStmtType stmt

checkStmtType (ForTo (Init ident_ ex) expr stmt) =  do
  it <- checkExprType (EVar ident_)
  ext <- checkExprType ex
  e <- checkExprType expr
  if (it /= Int) || (e /= Int) || (ext /= Int) then
    error ("Type error: ForTo loop with non-integer iterator or condition!" ++ (show e) ++ " " ++ (show it))
  else
    checkStmtType stmt

checkStmtType (ForDownTo (NoInit ident_) expr stmt) = do
  it <- checkExprType (EVar ident_)
  e <- checkExprType expr
  if (it /= Int) || (e /= Int) then
    error "Type error: ForDownTo loop with non-integer iterator or condition!"
  else
    checkStmtType stmt


checkStmtType (ForDownTo (Init ident_ ex) expr stmt) =  do
  it <- checkExprType (EVar ident_)
  ext <- checkExprType ex
  e <- checkExprType expr
  if (it /= Int) || (e /= Int) || (ext /= Int) then
    error "Type error: ForDownTo loop with non-integer iterator or condition!"
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
checkStmtListType [] = do
  env <- ask
  return (Just env)

checkDeclListType :: Type -> [Item] -> TT (Maybe Typing)
checkDeclListType t (h:r) = do
  newEnv <- checkDeclType t h
  case newEnv of
    Nothing -> error "Error with multiple declarations!"
    Just e -> local (\_ -> e) (checkDeclListType t r)
checkDeclListType _ [] = do
  env <- ask
  return (Just env)

checkDeclType :: Type -> Item -> TT (Maybe Typing)
checkDeclType t (NoInit ident_) = do
  env <- ask
  case (M.lookup ident_ (vEnv env)) of
    Just _ -> error ("Declaration error: variable " ++ (show t) ++ " " ++ (getName ident_) ++ " already declared!")
    Nothing -> return (Just $ Typing (M.insert ident_ t (vEnv env)) (fEnv env) (currRet env))

checkDeclType t (Init ident_ expr) = do
  env <- ask
  et <- checkExprType expr
  case (M.lookup ident_ (vEnv env)) of
    Just _ -> error "Declaration error: variable already declared!"
    Nothing -> if (et == t) then
        return (Just $ Typing (M.insert ident_ t (vEnv env)) (fEnv env) (currRet env))
      else
        error "Declaration error: Type of expression do not match with type declared!"

-- CHECK FUNCTION DECLARATIONS

checkTopDefType :: TopDef -> TT (Maybe Typing)
checkTopDefType (FnDef typ ident_ args (Block fbody)) = do
  let argsM = M.fromList [(k, a) | (Arg a k) <- args]
  newEnv <- putFunction typ ident_ args
  case newEnv of
    Just e -> local (\_ -> (Typing (M.union (vEnv e) argsM) (fEnv e) (currRet e))) (checkStmtType (BStmt (Block fbody))) >> return newEnv
    Nothing -> error "Declaration error!"

-- Zwróci środowisko zaktualizowane o nową funkcje
putFunction :: Type -> Ident -> [Arg] -> TT (Maybe Typing)
putFunction typ ident_ args = do
  let argsT = checkArgs args
  let argsM = M.fromList [(k, a) | (Arg a k) <- args]
  env <- ask
  case (M.lookup ident_ (fEnv env)) of
    Just _ -> error "Declaration error: Function redeclaration!"
    Nothing -> return (Just $ Typing (vEnv env) (M.insert ident_ (FunTyping typ argsT) (fEnv env)) typ)

-- return (Just $ Typing (M.union (vEnv env) argsM) (M.insert ident_ (FunTyping typ argsT) (fEnv env)) typ)

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
checkProgram (Program []) = do
  env <- ask
  return (Just env)

showType :: Type -> String
showType Int = show "Int"
showType Bool = show "Bool"
showType Str = show "Bool"
showType _ = show "Unknown"

getName :: Ident -> String
getName (Ident s) = s

runCheck :: Expr -> Either String Type
runCheck e = runIdentity (runErrorT (runReaderT (checkExprType e) initTyping))
--initTyping x = Void

--type TT = ReaderT Typing (ErrorT String Identity)
runProgramCheck p = runIdentity (runErrorT (runReaderT (checkProgram p) initTyping))

extType :: Expr
extType = EVar (Ident "x")

extArgs = [Arg Int (Ident "x")]

p1 = (Program [])
p2 = (Program [FnDef Int (Ident "main") [Arg Int (Ident "x")] (Block [Ret (EAdd (EVar (Ident "x")) Plus (ELitInt 111))])])
p3 = (Program [FnDef Int (Ident "main") [] (Block [ForTo (Init (Ident "i") (ELitInt 1)) (EAdd (ELitInt 9) Plus (ELitInt 1)) (BStmt (Block [CondElse (ERel (EVar (Ident "i")) LE (ELitInt 5)) (SExp (EApp (Ident "printInt") [EApp (Ident "fact") [EVar (Ident "i")]])) (SExp (EApp (Ident "printInt") [EApp (Ident "factr") [EVar (Ident "i")]]))])),Ret (ELitInt 0)])])
p4 = (Program [FnDef Void (Ident "printInt") [Arg Int (Ident "x")] (Block [VRet]),FnDef Int (Ident "fact") [Arg Int (Ident "i")] (Block [CondElse (ERel (EVar (Ident "i")) LE (EAdd (Neg (ELitInt 7)) Plus (EMul (ELitInt 2) Times (ELitInt 4)))) (Ret (ELitInt 1)) (Ret (EMul (EVar (Ident "i")) Times (EApp (Ident "fact") [EAdd (EVar (Ident "i")) Minus (ELitInt 1)])))]),FnDef Int (Ident "main") [] (Block [Decl Int [NoInit (Ident "i")],ForTo (Init (Ident "i") (ELitInt 1)) (EAdd (ELitInt 9) Plus (ELitInt 1)) (BStmt (Block [SExp (EApp (Ident "printInt") [EApp (Ident "fact") [EVar (Ident "i")]])])),Ret (ELitInt 0)])])
