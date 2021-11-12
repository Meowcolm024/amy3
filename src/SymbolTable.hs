module SymbolTable where

import qualified Data.Map                      as Map
import           Types

type SymbolMap a = Map.Map a (Signature a)

-- | function/constructor/type signature
data Signature a = FunSig [AType a] [AType a] (AType a)
                 | ConstrSig [AType a] [AType a] (AType a)
                 | TypeIns (AType a)
                 deriving (Show)

-- | symbol table
-- the symbol table is only used for global definition
data SymbolTable a = SymbolTable
    { types        :: SymbolMap a
    , functions    :: SymbolMap a
    , constructors :: SymbolMap a
    , entry        :: Definition a
    }
    deriving Show

-- | prototype
-- all the prototypes
type TemplateTable = SymbolTable String

emptySymbolMap :: SymbolMap a
emptySymbolMap = Map.empty
