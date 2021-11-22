module TypeChecker where

import           Control.Monad.Except           ( throwError )
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import qualified Data.Map                      as Map
import           SymbolTable                    ( SymbolTable )
import           Types

-- | env with counter in state monad
data Env = Env
    { locals :: Map.Map Idx (AType Idx)
    , index  :: Int
    }
    deriving Show

-- | empty env
initEnv :: Env
initEnv = Env Map.empty 0

type CheckState a = ExceptT String (State Env) a

-- | generate type constraints
--   expr and symbol table get from name analysis
genConstraint :: Expr Idx -> SymbolTable -> CheckState a
genConstraint = undefined

subst = undefined

solveConstraint = undefined
