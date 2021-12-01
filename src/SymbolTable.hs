module SymbolTable where

import qualified Data.Map                      as Map
import           Types

-- | function/constructor/type signature
data Signature a = FunSig [AType a] [AType a] (AType a)         -- type variable, parameter type, return type
                 | ConstrSig [AType a] [AType a] (AType a)      -- type variable, parameter type, case type
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

-- | get index from name
lookupName :: String -> SymbolTable -> Maybe Idx
lookupName name table = Map.lookup name (defsByName table)

-- | lookup constructor in table
lookupConstr :: String -> String -> SymbolTable -> Maybe (Idx, Signature Idx)
lookupConstr tn cn table = do
    tid    <- lookupName tn table
    cid    <- lookupName (tn ++ "." ++ cn) table
    csts   <- Map.lookup tid (constructors table)
    result <- Map.lookup cid csts
    pure (cid, result)

-- | table used in state monad
data TableST = TableST
    { table      :: SymbolTable             -- ^ symbol table
    , localIndex :: Int                     -- ^ local index counter
    , index      :: Int                     -- ^ global index counter
    }
    deriving Show

type Local = Map.Map String Idx      -- ^ local env

-- | empty table for state
emptyTable :: TableST
emptyTable = TableST
    (SymbolTable (Map.fromList pds) Map.empty (Map.fromList pfs) Map.empty)
    0
    0
  where
    (pds, pfs) = unzip $ zipWith
        (\(n, s) y -> ((n, Idx n (negate y)), (Idx n (negate y), s)))
        (Map.toList primitives)
        [1 ..]

-- | primitive functions
data Primitive a = Primitive
    { pName :: String           -- ^ function name
    , pArgs :: [AType a]        -- ^ arg types
    , pRet  :: AType a          -- ^ return type
    }
    deriving Show

-- | primitive functions
primitives :: Map.Map String (Signature a)
primitives = Map.fromList $ map
    (\x -> (pName x, primToSig x))
    [ Primitive "print"    [AnyType]    UnitType
    , Primitive "println"  [AnyType]    UnitType
    , Primitive "readLine" []           StringType
    , Primitive "toInt"    [StringType] IntType
    ]

-- | convert primitives to signatures
primToSig :: Primitive a -> Signature a
primToSig (Primitive n a r) = FunSig [] a r

isPrimitive :: String -> Bool
isPrimitive p = Map.member p primitives

type FuncTable = Map.Map Idx (Definition Idx)

buildFuncTable :: Program Idx -> FuncTable
buildFuncTable []         = Map.empty
buildFuncTable (def : st) = case def of
    FunDef idx ats pds at ex -> Map.insert idx def $ buildFuncTable st
    EntryPoint de            -> buildFuncTable (de : st)
    _                        -> buildFuncTable st
