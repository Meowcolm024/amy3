module Utils where

import qualified Data.Map                      as Map
import           SymbolTable
import           Types

-- | primitive functions
data Primitive = Primitive
    { pName :: String
    , pArgs :: [AType String]
    , pRet  :: AType String
    }
    deriving Show

-- | primitive functions
primitives :: Map.Map String Primitive
primitives = Map.fromList $ zipWith
    (\_ y -> (pName y, y))
    [0 ..]
    [ Primitive "print"    [StringType] UnitType
    , Primitive "readLine" []           StringType
    ]

-- | convert primitives to defintions
primToDef :: Primitive -> Definition String
primToDef (Primitive n a r) = FunDef n
                                     []
                                     (zipWith ParamDef (show <$> [0 ..]) a)
                                     r
                                     (Bottom (LitString "<primitives>"))

-- | env
type Env = Map.Map String

isTypeDef :: Definition a -> Bool
isTypeDef EnumDef{} = True
isTypeDef _         = False

isMainDef :: Definition a -> Bool
isMainDef (EntryPoint _) = True
isMainDef _              = False

-- | check if is a valid type (in env)
checkTypeEX :: AType String -> SymbolMap String -> Bool
checkTypeEX (TypeParam a  ) env = Map.member a env
checkTypeEX (EnumType n as) env = all (`Map.member` env) (n : typeVars as)
  where
    typeVars []                 = []
    typeVars (TypeParam t : ts) = t : typeVars ts
    typeVars (_           : ts) = typeVars ts
checkTypeEX _ _ = True                                  -- primitive types

-- | convert type variable list to symbol table
typeArgsToEnv :: Ord k => [AType k] -> Map.Map k (Signature k)
typeArgsToEnv tas =
        Map.fromList $ zipWith (\(TypeParam x) y -> (x, TypeIns y)) tas tas
        