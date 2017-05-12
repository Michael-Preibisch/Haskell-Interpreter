module Test where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Identity
import Data.Char

type Parser a = (ErrorT String (StateT String Identity)) a
--                       błąd           stan

-- parse :: (Parser a) -> String -> wynik
parse parser input = runIdentity (runStateT (runErrorT parser) input)

-- state ma put get i modify

pEof :: Parser () -- puste wejście
pEof = do
    input <- get
    if input == "" then return ()
    else fail "expected end"

pChar :: Char -> Parser Char
pChar c = do
    input <- get
    case input of
        "" -> fail ("expected '" ++ [c] ++ "' but found eof")
        c':input' ->
            if c == c' then do
                put input'
                return c
            else fail ("expected '" ++ [c] ++ "' but found '" ++ [c'] ++ "'")

pSat :: (Char -> Bool) -> Parser Char
pSat sat = do
    input <- get
    case input of
        "" -> fail ("found the end")
        c:input' ->
            if sat c then do
                put input'
                return c
            else fail ("unexpected '" ++ [c] ++ "'")

pOr :: Parser a -> Parser a -> Parser a
pOr p1 p2 = do
    input <- get
    p1 `catchError` (\_ -> do
        put input
        p2)

pDigit :: Parser Int
pDigit = do
    x <- pSat isDigit
    return (digitToInt x)

--                     71783
--                     1783 10000
pDecimalRec :: Parser (Int, Int)
pDecimalRec = do
    x <- pDigit
    (r, m) <- pOr pDecimalRec (return (0, 1))
    return (x * m + r, 10 * m)

pDecimal :: Parser Int
pDecimal = do
    (r, m) <- pDecimalRec
    return r

pIdent :: Parser String
pIdent = do
    x <- pSat isAlpha
    r <- pOr pIdent (return "")
    return (x:r)

pDecimalOnly = do
    x <- pDecimal
    pEof
    return x

type Var = String
data Exp = EInt Int
         | EStr String
         | EVar Var
         | EMul Exp Exp
         | EAdd Exp Exp
         | ELet Var Exp Exp
         | ECon Exp Exp
    deriving Show

-- 12*7+3+4
pInt = do
    x <- pDecimal
    return (EInt x)

pVar = do
    x <- pIdent
    return (EVar x)

pMul = do
    e1 <- pExp2
    pChar '*'
    e2 <- pExp3
    return (EMul e1 e2)

pAdd = do
    e1 <- pExp3
    pChar '+'
    e2 <- pExp4
    return (EAdd e1 e2)

pBracket = do
    pChar '('
    e <- pExp
    pChar ')'
    return e

pWhitespace = do
    pOr (do {
        pChar ' ';
        pWhitespace
    }) (return ())


pString "" = return ""
pString (c:s) = do
    pChar c
    pString s
    return (c:s)

pLetIn = do
    pString "let"
    pWhitespace
    x <- pIdent
    pWhitespace
    pString "="
    pWhitespace
    e1 <- pExp
    pWhitespace
    pString "in"
    pWhitespace
    e2 <- pExp
    return (ELet x e1 e2)

pStringLiteral = do
    pString "string"
    pWhitespace
    str <- pIdent
    return (EStr str)

pCon = do
    str1 <- pExp5
    pWhitespace
    pChar '.'
    pWhitespace
    str2 <- pExp6
    return (ECon str1 str2)

-- najpierw najbardziej ogólne
-- potem szczegółowe
pExp = pExp7
--pExp = do
--    x <- pExp4
--    pEof
--    return x

pProgram = do
    x <- pExp
    pEof
    return x

pExp1 = pOr pInt pVar
pExp2 = pOr pBracket pExp1
pExp3 = pOr pMul pExp2
pExp4 = pOr pAdd pExp3
pExp5 = pOr pStringLiteral pExp4
pExp6 = pOr pCon pExp5
pExp7 = pOr pLetIn pExp6

--------------------------------------------------------------------------------

-- analiza statyczna
-- sprawdzanie, czy wszystkie zmienne są zadeklarowane
--check :: Exp -> [String]
check :: Exp -> ((ReaderT Typing (ErrorT String Identity)) Type)
check (EInt _) = return TInt

check (EStr _) = return TString

check (EVar x) = do
    typing <- ask
    let t = typing x in do
    if (t /= TUndefined) then return t
    else fail ("Undeclared variable '" ++ x ++ "'.")

check (EMul e1 e2) = do
    t1 <- check e1
    t2 <- check e2
    if (t1 == TInt && t2 == TInt) then return TInt
    else fail ("Trying to multiply " ++ show t1 ++ " and " ++ show t2 ++ ".")

check (EAdd e1 e2) = do
    t1 <- check e1
    t2 <- check e2
    if (t1 == TInt && t2 == TInt) then return TInt
    else fail ("Trying to add " ++ show t1 ++ " and " ++ show t2 ++ ".")

check (ECon e1 e2) = do
    t1 <- check e1
    t2 <- check e2
    if (t1 == TString && t2 == TString) then return TString
    else fail ("Trying to concat " ++ show t1 ++ " and " ++ show t2 ++ ".")

check (ELet x e1 e2) = do
    t <- check e1
    local (\typing -> setType typing x t) (check e2)

runCheck e = runIdentity (runErrorT (runReaderT (check e) initTyping))

runProgram str =
    let exp = parse pProgram str
    in case exp of
         (Left err, rem) -> "Error " ++ err ++ " remainining " ++ rem
         (Right e, _) ->
             let checked = runCheck e
                 in case checked of
                      (Left err) -> err
                      (Right _) ->
                          let res = runEval e
                              in case res of
                                  Left rterr -> "Runtime error: " ++ rterr
                                  Right n -> "Result: " ++ show n

-- ReaderT [Var] -- lista zadeklarowanych zmiennych
-- WriterT [String] -- akumulowanie błędów

-- lepiej EBin BinOp Exp Exp - wtedy jedna reguła dla wszystkich operacji binarnych

-- ReaderT ErrorT, wynikiem będzie int

--------------------------------------------------------------------------------

-- let x = 12 in x + 7

-- eval

-- error
-- bez writera
-- reader do wartości zmiennych

type Valuation = Var -> Value

getValue valuation x = valuation x

setValue valuation x n =
    \y -> if x == y then n else valuation y

initValuation x = error "Wrong!"

data Value = VInt Int | VStr String deriving Show

eval :: Exp -> (ReaderT Valuation (ErrorT String Identity)) Value

eval (EInt n) =
    return (VInt n)

eval (EStr str) =
    return (VStr str)

eval (EAdd e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    return (vAdd v1 v2)

eval (EMul e1 e2) = do
    n1 <- eval e1
    n2 <- eval e2
    return (vMul n1 n2)

eval (ECon e1 e2) = do
    str1 <- eval e1
    str2 <- eval e2
    return (vCon str1 str2)

eval (EVar x) = do
    valuation <- ask -- z monady reader
    return (getValue valuation x)

eval (ELet x e1 e2) = do
    n1 <- eval e1
    n2 <- local (\v -> setValue v x n1) (eval e2)
    return n2

runEval e = runIdentity (runErrorT (runReaderT (eval e) initValuation))

--------------------------------------------------------------------------------

-- String id -> StrExp
-- Exp . Exp -> konkatenacja
-- zdefiniować data Value = VInt Int | VStr String
-- type Valuation = Var -> Value
-- eval zaktualizowany (np. 7 . (string x))
-- check zaktualizowany

vAdd :: Value -> Value -> Value
vAdd (VInt n1) (VInt n2) = VInt (n1 + n2)
vAdd _ _ = error "not ints"

vMul :: Value -> Value -> Value
vMul (VInt n1) (VInt n2) = VInt (n1 * n2)
vMul _ _ = error "not ints"

vCon :: Value -> Value -> Value
vCon (VStr str1) (VStr str2) = VStr (str1 ++ str2)
vCon _ _ = error "not strings"

data Type = TInt | TString | TUndefined
    deriving (Eq, Show)

type Typing = Var -> Type

initTyping x = TUndefined

getType typing x = typing x

setType typing x n =
    \y -> if x == y then n else typing y

extStr = EStr "lol"
