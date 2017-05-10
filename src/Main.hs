import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Maybe
import Types
import LexMokka
import ParMokka
import SkelMokka
import PrintMokka
import AbsMokka

type Bindings = Map.Map String Int;

-- Returns True if the "count" variable contains correct bindings size.
isCountCorrect :: Bindings -> Bool
isCountCorrect bindings = runReader calc_isCountCorrect bindings

-- The Reader monad, which implements this complicated check.
calc_isCountCorrect :: Reader Bindings Bool
calc_isCountCorrect = do
   count <- asks (lookupVar "count")
   bindings <- ask
   return (count == (Map.size bindings))

-- The selector function to  use with 'asks'.
-- Returns value of the variable with specified name.
lookupVar :: String -> Bindings -> Int
lookupVar name bindings = fromJust (Map.lookup name bindings)

sampleBindings = Map.fromList [("count",3), ("1",1), ("b",2)]

main = do
   putStrLn $ show $ pProgram $ myLexer "int main(int x) { return x + 111; }"
