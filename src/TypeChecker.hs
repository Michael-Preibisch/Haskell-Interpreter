module TypeChecker where

import Types
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Identity
type Typing = Ident -> Type

showType Int = show "Int"
showType Bool = show "Bool"
showType Str = show "Bool"
showType _ = show "Unknown"

checkType :: Expr -> ((ReaderT Typing (ErrorT String Identity)) Type)
checkType (ELitInt _) = return Int

checkType (EString _) = return Str

checkType ELitTrue = return Bool

checkType ELitFalse = return Bool

checkType (EMul e1 op e2) = do
    t1 <- checkType e1
    t2 <- checkType e2
    if (t1 == Int && t2 == Int) then return Int
    else fail ("Type error: trying to multiply " ++ show t1 ++ " and " ++ show t2 ++ ".")

checkType (EAdd e1 op e2) = do
    t1 <- checkType e1
    t2 <- checkType e2
    if (t1 == Int && t2 == Int) then return Int
    else if (t1 == Str && t2 == Str) then return Str
    else fail ("Type error: Trying to add " ++ show t1 ++ " and " ++ show t2 ++ ".")

runCheck e = runIdentity (runErrorT (runReaderT (checkType e) initTyping))
initTyping x = Void
