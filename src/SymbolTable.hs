module SymbolTable where

import qualified Data.Map                      as Map
import           Types

-- | function/constructor/type signature
data Signature a = FunSig [AType a] [AType a] (AType a)
                 | ConstrSig [AType a] [AType a] (AType a)
                 | TypeIns (AType a)
                 deriving (Show)

-- name (string) -> signature
type SymbolMap a = Map.Map a (Signature a)

-- | symbol table
-- the symbol table is only used for global definition
data SymbolTable a = SymbolTable
    { types        :: SymbolMap a                   -- ^ type def
    , functions    :: SymbolMap a                   -- ^ func def
    , constructors :: Map.Map a (SymbolMap a)       -- ^ constr def
    , entry        :: Definition a                  -- ^ main function
    }
    deriving Show

-- | prototype
-- all the prototypes
type TemplateTable = SymbolTable String
