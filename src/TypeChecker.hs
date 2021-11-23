module TypeChecker where

import           Control.Monad.Except           ( throwError )
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import qualified Data.Map                      as Map
import           SymbolTable                    ( Signature(TypeSig)
                                                , SymbolTable(..)
                                                )
import           Types
import Data.Either (isRight)
import Control.Monad (guard)

-- | type constraint
data Constraint = Constraint
    { found    :: AType Idx
    , expected :: AType Idx
    }
    deriving Show

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

-- | check type application in enum definitions
--   others will be checked during generating constraints
checkEnum :: Program Idx -> SymbolTable -> Either String [()]
checkEnum []                       _                          = Right []
checkEnum (EnumDef _ _ cases : st) tb@(SymbolTable _ tys f c) = do
    this <- concat <$> traverse checkCase cases
    rest <- checkEnum st tb
    pure $ this <> rest
  where
    checkCase :: CaseDef Idx -> Either String [()]
    checkCase ~(CaseDef cn params _) =
        (`traverse` params) $ \(ParamDef _ t) -> if checkTypeApp t
            then Right ()
            else
                Left
                $  "Invalid type application in definition of "
                ++ nameIdx cn
    checkTypeApp :: AType Idx -> Bool
    checkTypeApp (EnumType t ats) =
        let Just (TypeSig (EnumType _ ats')) = Map.lookup t tys
        in  length ats == length ats' && all checkTypeApp ats
    checkTypeApp _ = True
checkEnum (_ : st) tb = checkEnum st tb

-- | generate type constraints
--   expr and symbol table get from name analysis
genConstraint :: Expr Idx -> SymbolTable -> CheckState [Constraint]
genConstraint expr st = undefined

-- | substitue tmp type var i to type t
subst :: [Constraint] -> Int -> AType Idx -> [Constraint]
subst cs i t = undefined

-- | solve type constraint
solveConstraint :: [Constraint] -> Either String ()
solveConstraint = undefined
