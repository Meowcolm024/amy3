{-# LANGUAGE TupleSections #-}
module TypeChecker where

import           Control.Monad                  ( zipWithM )
import           Control.Monad.Except           ( MonadTrans(lift)
                                                , throwError
                                                )
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Data.Either                    ( isRight )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromJust )
import           SymbolTable                    ( Signature(..)
                                                , SymbolTable(..)
                                                )
import           Types

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

-- | the return type of the main function will be ignored
checkMain :: Program Idx -> Program Idx
checkMain []                   = []
checkMain (EntryPoint mn : rs) = EntryPoint mn { retType = AnyType } : rs
checkMain (r             : rs) = r : checkMain rs

-- | generate type constraints
--   expr and symbol table get from name analysis
genConstraint :: Definition Idx -> SymbolTable -> CheckState [Constraint]
genConstraint ~(FunDef _ targs params ret expr) st = do
    let initEnv =
            map (\(TypeParam x) -> (x, TypeParam x)) targs
                ++ map (\(ParamDef n t) -> (n, t)) params
    -- init env
    lift . put $ Env (Map.fromList initEnv) 0
    -- generate constraint for the body
    genCons expr ret
  where
    genCons :: Expr Idx -> AType Idx -> CheckState [Constraint]
    genCons expr expected = case expr of
        Variable idx -> do
            Env e _ <- lift get
            let at = fromJust $ Map.lookup idx e
            pure . pure $ Constraint at expected
        LitInt    _ -> pure . pure $ Constraint IntType expected
        LitBool   b -> pure . pure $ Constraint BooleanType expected
        LitString s -> pure . pure $ Constraint StringType expected
        LitUnit     -> pure . pure $ Constraint UnitType expected
        Plus ex ex' -> do
            lhs <- genCons ex IntType
            rhs <- genCons ex' IntType
            pure $ lhs ++ rhs ++ [Constraint IntType expected]
        Minus ex ex' -> do
            lhs <- genCons ex IntType
            rhs <- genCons ex' IntType
            pure $ lhs ++ rhs ++ [Constraint IntType expected]
        Mult ex ex' -> do
            lhs <- genCons ex IntType
            rhs <- genCons ex' IntType
            pure $ lhs ++ rhs ++ [Constraint IntType expected]
        Div ex ex' -> do
            lhs <- genCons ex IntType
            rhs <- genCons ex' IntType
            pure $ lhs ++ rhs ++ [Constraint IntType expected]
        LessThan ex ex' -> do
            lhs <- genCons ex IntType
            rhs <- genCons ex' IntType
            pure $ lhs ++ rhs ++ [Constraint BooleanType expected]
        LessEqual ex ex' -> do
            lhs <- genCons ex IntType
            rhs <- genCons ex' IntType
            pure $ lhs ++ rhs ++ [Constraint BooleanType expected]
        And ex ex' -> do
            lhs <- genCons ex BooleanType
            rhs <- genCons ex' BooleanType
            pure $ lhs ++ rhs ++ [Constraint BooleanType expected]
        Or ex ex' -> do
            lhs <- genCons ex BooleanType
            rhs <- genCons ex' BooleanType
            pure $ lhs ++ rhs ++ [Constraint BooleanType expected]
        Equals ex ex' -> do
            e@(Env _ i) <- lift get
            lift . put $ e { index = i + 1 }
            lhs <- genCons ex (Counted i)
            rhs <- genCons ex' (Counted i)
            pure $ lhs ++ rhs ++ [Constraint BooleanType expected]
        Concat ex ex' -> do
            lhs <- genCons ex StringType
            rhs <- genCons ex' StringType
            pure $ lhs ++ rhs ++ [Constraint StringType expected]
        Seq ex ex' -> do
            lhs <- genCons ex AnyType
            rhs <- genCons ex' expected
            pure $ lhs ++ rhs
        Not ex -> do
            e <- genCons ex BooleanType
            pure $ e ++ [Constraint BooleanType expected]
        Neg ex -> do
            e <- genCons ex IntType
            pure $ e ++ [Constraint IntType expected]
        Call idx exs -> do
            let Just (FunSig targs params ret) = Map.lookup idx (functions st)
            Env e i <- lift get
            lift . put $ Env e (i + length targs)
            let (rt : ps) = typeApp
                    (zip (map (\(TypeParam x) -> x) targs) [i ..])
                    (ret : params)
            pcs <- zipWithM genCons exs ps
            pure $ concat pcs ++ [Constraint rt expected]
        ConstrCall idx at exs -> do
            let EnumType p _ = at
            let Just (ConstrSig targs params ret) =
                    Map.lookup p (constructors st) >>= Map.lookup idx
            Env e i <- lift get
            lift . put $ Env e (i + length targs)
            let (rt : ps) = typeApp
                    (zip (map (\(TypeParam x) -> x) targs) [i ..])
                    (ret : params)
            pcs <- zipWithM genCons exs ps
            pure $ concat pcs ++ [Constraint rt expected]
        Let (ParamDef n t) bd ex -> do
            b            <- genCons bd t
            e@(Env lc _) <- lift get
            lift . put $ e { locals = Map.insert n t lc }
            e <- genCons ex expected
            pure $ b ++ e
        IfElse ex ex' ex3 -> do
            pr          <- genCons ex BooleanType
            e@(Env _ i) <- lift get
            lift . put $ e { index = i + 1 }
            el <- genCons ex' (Counted i)
            at <- genCons ex3 (Counted i)
            pure $ pr ++ el ++ at ++ [Constraint (Counted i) expected]
        Match ex mcs -> do
            e@(Env _ i) <- lift get
            lift . put $ e { index = i + 1 }
            e  <- genCons ex (Counted i)
            cs <- mapM (handleCases expected (Counted i)) mcs
            pure $ e ++ concat cs ++ [Constraint (Counted i) expected]
        Bottom ex -> genCons ex StringType
    -- | type application
    typeApp :: [(Idx, Int)] -> [AType Idx] -> [AType Idx]
    typeApp env ts = map tapp ts
      where
        tapp (TypeParam t      ) = Counted $ fromJust $ lookup t env
        tapp (EnumType n params) = EnumType n (map tapp params)
        tapp t                   = t

    handleCases
        :: AType Idx -> AType Idx -> MatchCase Idx -> CheckState [Constraint]
    handleCases expected scrt (MatchCase pat expr) = do
        let (patc, env) = handlePat pat scrt
        retc <- genCons expr expected
        pure $ patc ++ retc
    -- also update env on the fly
    handlePat :: Pattern Idx -> AType Idx -> ([Constraint], [(Idx, AType Idx)])
    handlePat pat expected = case pat of
        WildcardPattern   -> ([], [])
        LiteralPattern ex -> (, []) $ case ex of
            LitInt    _ -> [Constraint IntType expected]
            LitBool   _ -> [Constraint BooleanType expected]
            LitString _ -> [Constraint StringType expected]
            LitUnit     -> [Constraint UnitType expected]
            _           -> error "Not a literal"
        IdPattern idx           -> ([], [(idx, expected)])
        EnumPattern idx at pats -> undefined

-- | eval state and get the constarint
runConstraint :: CheckState [Constraint] -> Either String [Constraint]
runConstraint cs = evalState (runExceptT cs) initEnv

-- | substitue tmp type var i to type t
subst :: [Constraint] -> Int -> AType Idx -> [Constraint]
subst cs i t = undefined

-- | solve type constraint
solveConstraint :: [Constraint] -> Either String ()
solveConstraint = undefined
