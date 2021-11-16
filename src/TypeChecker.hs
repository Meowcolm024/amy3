module TypeChecker where

import           SymbolTable
import qualified Types                         as T
import Types (AType(..))

data Constraint a = Counted Int
                  | Basic (AType a)
                  deriving (Show)
