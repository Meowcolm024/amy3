module SymbolTable where

import qualified Data.Map                      as Map
import           Types

-- | function/constructor/type signature
data Signature a = FunSig [AType a] [AType a] (AType a)
                 | ConstrSig [AType a] [AType a] (AType a)
                 | TypeSig (AType a)
                 deriving (Show)

-- name (string) -> signature
type SymbolMap a = Map.Map a (Signature a)

-- | symbol table
-- the symbol table is only used for global definition
data SymbolTable = SymbolTable
    { defsByName   :: Map.Map String Idx
    , types        :: SymbolMap Idx
    , functions    :: SymbolMap Idx
    , constructors :: Map.Map Idx (SymbolMap Idx)
    }

instance Show SymbolTable where
    show (SymbolTable _ t f c) =
        "<Types>\n" ++ unlines cs ++ "\n<Functions>\n" ++ unlines
            (map g $ Map.toList f)
      where
        g (p, v) = "  " ++ show p ++ " := " ++ show v
        cs = map (\(k, v) -> show k ++ ":\n" ++ unlines (map g $ Map.toList v))
            $ Map.toList c

lookupName :: String -> SymbolTable -> Maybe Idx
lookupName name table = Map.lookup name (defsByName table)

-- table used in state monad
data TableST = TableST
    { table :: SymbolTable
    , index :: Int
    }
    deriving Show

emptyTable :: TableST
emptyTable = TableST (SymbolTable Map.empty Map.empty Map.empty Map.empty) 0

incIdx :: TableST -> TableST
incIdx t@(TableST _ i) = t { index = i + 1 }
