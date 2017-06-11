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
    (Right _) ->
      runProgram p
    (Left e) -> error e

main :: IO ()
main = do
   file <- getArgs
   case file of
       [] -> error "No args provided!"
       file:_ -> do
           program <- readFile file
           case parser program of
               Ok p  -> do
                 run p
                 return ()
               Bad e -> error e
