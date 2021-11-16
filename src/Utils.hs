module Utils where

import qualified Data.Map                      as Map
import           SymbolTable
import           Types

-- | primitive functions
data Primitive = Primitive
    { pName :: String               -- ^ function name
    , pArgs :: [AType String]       -- ^ arg types
    , pRet  :: AType String         -- ^ return type
    }
    deriving Show

-- | primitive functions
primitives :: Map.Map String (Signature String)
primitives = Map.fromList $ map
    (\x -> (pName x, primToSig x))
    [ Primitive "print"    [StringType] UnitType
    , Primitive "println"  [StringType] UnitType
    , Primitive "readLine" []           StringType
    ]

-- | convert primitives to signatures
primToSig :: Primitive -> Signature String
primToSig (Primitive n a r) = FunSig [] a r

isTypeDef :: Definition a -> Bool
isTypeDef EnumDef{} = True
isTypeDef _         = False

isMainDef :: Definition a -> Bool
isMainDef (EntryPoint _) = True
isMainDef _              = False

isFuncDef :: Definition a -> Bool
isFuncDef FunDef{} = True
isFuncDef _        = False

-- | check if is a valid type (in env)
checkTypeEX :: AType String -> SymbolMap String -> Bool
checkTypeEX (TypeParam a  ) env = Map.member a env
checkTypeEX (EnumType n as) env = all (`Map.member` env) (n : typeVars as)
  where
    typeVars []                   = []
    typeVars (TypeParam t   : ts) = t : typeVars ts
    typeVars (EnumType t as : ts) = t : typeVars as ++ typeVars ts  -- expand nested types
    typeVars (_             : ts) = typeVars ts
checkTypeEX _ _ = True                                  -- primitive types

-- | convert type variable list to symbol table
typeArgsToEnv :: Ord k => [AType k] -> Map.Map k (Signature k)
typeArgsToEnv tas =
    Map.fromList $ zipWith (\(TypeParam x) y -> (x, TypeIns y)) tas tas

-- | convert param defs to local bindings
--   name -> type
argsToEnv :: Ord k => [ParamDef k] -> Map.Map k (Signature k)
argsToEnv = Map.fromList . map (\x -> (paramName x, TypeIns $ paramType x))
