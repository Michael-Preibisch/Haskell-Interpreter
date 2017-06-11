import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Maybe
import TypeChecker
import Evaluator
import LexMokka
import ErrM
import ParMokka
import PrintMokka
import System.Environment
import System.IO

parser = pProgram . myLexer

run p = do
  case runProgramCheck p of
    (Right _) -> do
      let res = execProgram p
      putStrLn "ok"
    (Left e) -> error e

main :: IO ()
main = do
   file <- getArgs
   case file of
       [] -> error "No args provided!"
       file:_ -> do
           program <- readFile file
           case parser program of
               Ok p   -> run p
               Bad e -> error e
