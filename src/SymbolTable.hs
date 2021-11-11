module SymbolTable where

import qualified Data.Map                      as Map
import           Types

-- | function/constructor/type signature
data Signature a = FunSig [AType a] [AType a] (AType a)
                 | ConstrSig [AType a] [AType a] (AType a)
                 | TypeIns (AType a)
                 deriving (Show)

-- | symbol table
data SymbolTable a = SymbolTable
    { types        :: Map.Map a (Signature a)
    , functions    :: Map.Map a (Signature a)
    , constructors :: Map.Map a (Signature a)
    }
    deriving (Show)

-- | prototype
type TemplateTable = SymbolTable String
-- |concrete
type ConcreteTable = SymbolTable Idx
