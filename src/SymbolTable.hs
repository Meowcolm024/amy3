module SymbolTable where

import           Control.Applicative
import qualified Data.Map                      as Map
import           Types

-- name (string) -> signature
type SymbolMap a = Map.Map a (Signature a)

-- | function/constructor/type signature
data Signature a = FunSig [AType a] [AType a] (AType a)
                 | ConstrSig [AType a] [AType a] (AType a)
                 | TypeIns (AType a)
                 deriving (Show)

-- | symbol table
-- the symbol table is only used for global definition
data SymbolTable a = SymbolTable
    { types        :: SymbolMap a       -- ^ type def
    , functions    :: SymbolMap a       -- ^ func def
    , constructors :: SymbolMap a       -- ^ constr def
    , entry        :: Definition a      -- ^ main function
    }
    deriving Show

-- | prototype
-- all the prototypes
type TemplateTable = SymbolTable String

emptySymbolMap :: SymbolMap a
emptySymbolMap = Map.empty

-- | env
-- first lookup local then global finally prim
data Env a = Env
    { primFun   :: SymbolMap a     -- ^ primitive functions
    , global    :: SymbolTable a   -- ^ global definitions
    , localType :: SymbolMap a     -- ^ local type env
    , localVal  :: SymbolMap a     -- ^ local var env
    }
    deriving Show

-- | lookup types, starting from local type variables
-- ! this function does not check type application
lookupType :: Ord a => a -> Env a -> Maybe (Signature a)
lookupType k (Env _ g lt _) = Map.lookup k lt <|> Map.lookup k (types g)

getTypeEnv :: Ord a => Env a -> SymbolMap a
getTypeEnv (Env _ g lt _) = Map.union lt (types g)

-- | lookup constructors
lookupConstr :: Ord k => k -> Env k -> Maybe (Signature k)
lookupConstr k (Env _ g _ _) = Map.lookup k (constructors g)

-- | lookup variable or function
-- local binding may shadow function definitions
lookupValFun :: Ord k => k -> Env k -> Maybe (Signature k)
lookupValFun k (Env p g _ lv) =
    Map.lookup k lv <|> Map.lookup k (functions g) <|> Map.lookup k p

-- | concrete type and function collected
-- it will be generated during type checking
-- used for code generation
data SymbolIns a = SymbolIns
    { insTypes :: [Signature a]     -- ^ concrete types
    , insFuncs :: [Signature a]     -- ^ concrete functions
    }
    deriving Show

emptyIns :: SymbolIns a
emptyIns = SymbolIns [] []
