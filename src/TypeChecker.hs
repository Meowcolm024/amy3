{-# LANGUAGE TupleSections #-}
module TypeChecker
    ( checkType
    , runConstraint
    ) where

import           Control.Monad                  ( zipWithM )
import           Control.Monad.Trans.State
import           Data.Either                    ( isRight )
import           Data.Foldable                  ( traverse_ )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromJust )
import           SymbolTable                    ( Signature(..)
                                                , SymbolTable(..)
                                                )
import           Types
import           Utils                          ( removeQuot )

-- | type constraint
data Constraint = Constraint
    { found    :: AType Idx
    , expected :: AType Idx
    }
    deriving Show

-- | env with counter in state monad
type Env = Map.Map Idx (AType Idx)

type CheckState a = State Int a

-- | check type application in enum definitions
--   others will be checked during generating constraints
checkEnum :: Program Idx -> SymbolTable -> Either String [()]
checkEnum []         _                          = Right []
checkEnum (def : st) tb@(SymbolTable _ tys f c) = case def of
    FunDef fn _ params ret _ -> do
        this <- (`traverse` params) $ checkPD fn
        re <- if checkTypeApp ret then Right () else Left $ errRet ++ nameIdx fn
        rest <- checkEnum st tb
        pure $ re : this <> rest
    EnumDef _ _ cases -> do
        this <- concat <$> traverse checkCase cases
        rest <- checkEnum st tb
        pure $ this <> rest
    _ -> checkEnum st tb
  where
    checkCase :: CaseDef Idx -> Either String [()]
    checkCase ~(CaseDef cn params _) = (`traverse` params) $ checkPD cn
    checkTypeApp :: AType Idx -> Bool
    checkTypeApp (EnumType t ats) =
        let Just (TypeSig (EnumType _ ats')) = Map.lookup t tys
        in  length ats == length ats' && all checkTypeApp ats
    checkTypeApp _ = True

    checkPD cn (ParamDef _ t) =
        if checkTypeApp t then Right () else Left $ errMsg ++ nameIdx cn
    errMsg = "Invalid type application in definition of "
    errRet = "Invalid type application in return type of "


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
    -- generate constraint for the body
    genCons expr ret (Map.fromList initEnv)
  where
    genCons :: Expr Idx -> AType Idx -> Env -> CheckState [Constraint]
    genCons expr expected env = case expr of
        Variable idx -> do
            let at = fromJust $ Map.lookup idx env
            pure . pure $ Constraint at expected
        LitInt    _ -> pure . pure $ Constraint IntType expected
        LitBool   b -> pure . pure $ Constraint BooleanType expected
        LitString s -> pure . pure $ Constraint StringType expected
        LitUnit     -> pure . pure $ Constraint UnitType expected
        Plus ex ex' -> do
            lhs <- genCons ex IntType env
            rhs <- genCons ex' IntType env
            pure $ lhs ++ rhs ++ [Constraint IntType expected]
        Minus ex ex' -> do
            lhs <- genCons ex IntType env
            rhs <- genCons ex' IntType env
            pure $ lhs ++ rhs ++ [Constraint IntType expected]
        Mult ex ex' -> do
            lhs <- genCons ex IntType env
            rhs <- genCons ex' IntType env
            pure $ lhs ++ rhs ++ [Constraint IntType expected]
        Div ex ex' -> do
            lhs <- genCons ex IntType env
            rhs <- genCons ex' IntType env
            pure $ lhs ++ rhs ++ [Constraint IntType expected]
        Mod ex ex' -> do
            lhs <- genCons ex IntType env
            rhs <- genCons ex' IntType env
            pure $ lhs ++ rhs ++ [Constraint IntType expected]
        LessThan ex ex' -> do
            lhs <- genCons ex IntType env
            rhs <- genCons ex' IntType env
            pure $ lhs ++ rhs ++ [Constraint BooleanType expected]
        LessEqual ex ex' -> do
            lhs <- genCons ex IntType env
            rhs <- genCons ex' IntType env
            pure $ lhs ++ rhs ++ [Constraint BooleanType expected]
        And ex ex' -> do
            lhs <- genCons ex BooleanType env
            rhs <- genCons ex' BooleanType env
            pure $ lhs ++ rhs ++ [Constraint BooleanType expected]
        Or ex ex' -> do
            lhs <- genCons ex BooleanType env
            rhs <- genCons ex' BooleanType env
            pure $ lhs ++ rhs ++ [Constraint BooleanType expected]
        Equals ex ex' -> do
            i <- get
            put $ i + 1
            lhs <- genCons ex (Counted i) env
            rhs <- genCons ex' (Counted i) env
            pure $ lhs ++ rhs ++ [Constraint BooleanType expected]
        Concat ex ex' -> do
            lhs <- genCons ex StringType env
            rhs <- genCons ex' StringType env
            pure $ lhs ++ rhs ++ [Constraint StringType expected]
        Seq ex ex' -> do
            lhs <- genCons ex AnyType env
            rhs <- genCons ex' expected env
            pure $ lhs ++ rhs
        Not ex -> do
            e <- genCons ex BooleanType env
            pure $ e ++ [Constraint BooleanType expected]
        Neg ex -> do
            e <- genCons ex IntType env
            pure $ e ++ [Constraint IntType expected]
        Call idx exs -> do
            let Just (FunSig targs params ret) = Map.lookup idx (functions st)
            i <- get
            put $ i + length targs
            let (rt : ps) = typeApp
                    (zip (map (\(TypeParam x) -> x) targs) [i ..])
                    (ret : params)
            pcs <- zipWithM (\p q -> genCons p q env) exs ps
            pure $ concat pcs ++ [Constraint rt expected]
        ConstrCall idx at exs -> do
            let EnumType p _ = at
            let Just (ConstrSig targs params ret) =
                    Map.lookup p (constructors st) >>= Map.lookup idx
            i <- get
            put $ i + length targs
            let (rt : ps) = typeApp
                    (zip (map (\(TypeParam x) -> x) targs) [i ..])
                    (ret : params)
            pcs <- zipWithM (\p q -> genCons p q env) exs ps
            pure $ concat pcs ++ [Constraint rt expected]
        Let (ParamDef n t) bd ex -> do
            b <- genCons bd t env
            e <- genCons ex expected (Map.insert n t env)
            pure $ b ++ e
        IfElse ex ex' ex3 -> do
            pr <- genCons ex BooleanType env
            i  <- get
            put $ i + 1
            el <- genCons ex' (Counted i) env
            at <- genCons ex3 (Counted i) env
            pure $ pr ++ el ++ at ++ [Constraint (Counted i) expected]
        Match ex mcs -> do
            i <- get
            put $ i + 1
            -- constraint for expr to be matched
            e  <- genCons ex (Counted i) env
            -- same type for patterns and return type is "expected"
            cs <- mapM (handleCase expected (Counted i) env) mcs
            pure $ e ++ concat cs
        Bottom ex -> genCons ex StringType env
    -- | type application
    typeApp :: [(Idx, Int)] -> [AType Idx] -> [AType Idx]
    typeApp env ts = map tapp ts
      where
        tapp (TypeParam t      ) = Counted $ fromJust $ lookup t env
        tapp (EnumType n params) = EnumType n (map tapp params)
        tapp t                   = t
    -- | handle single case
    handleCase expected scrt env (MatchCase pat expr) = do
        (patc, moreEnv) <- handlePat pat scrt
        retc            <- genCons expr expected (Map.union moreEnv env)
        pure $ patc ++ retc
    -- | handle patterns
    handlePat :: Pattern Idx -> AType Idx -> CheckState ([Constraint], Env)
    handlePat pat expected = case pat of
        WildcardPattern   -> pure ([], Map.empty)
        LiteralPattern ex -> pure $ (, Map.empty) $ case ex of
            LitInt    _ -> [Constraint IntType expected]
            LitBool   _ -> [Constraint BooleanType expected]
            LitString _ -> [Constraint StringType expected]
            LitUnit     -> [Constraint UnitType expected]
            _           -> error "Not a literal"
        IdPattern idx           -> pure ([], Map.fromList [(idx, expected)])
        EnumPattern idx at pats -> do
            let EnumType p _ = at
            let Just (ConstrSig targs params ret) =
                    Map.lookup p (constructors st) >>= Map.lookup idx
            i <- get
            put $ i + length targs
            let tp  = zip (map (\(TypeParam x) -> x) targs) [i ..]
            -- map type variables to counted
            let tap = map (\(_, i) -> Counted i) tp
            let ps  = typeApp tp params
            (cs, es) <- unzip <$> zipWithM handlePat pats ps
            pure
                ( concat cs ++ [Constraint (EnumType p tap) expected]
                , foldr Map.union Map.empty es
                )

-- | eval state and get the constarint
runConstraint :: Program Idx -> SymbolTable -> [[Constraint]]
runConstraint dfs st = helper (\d -> evalState (genConstraint d st) 0) dfs
  where
    helper _ []                  = []
    helper f (x@FunDef{}   : xs) = f x : helper f xs
    helper f (EntryPoint x : xs) = f x : helper f xs
    helper f (_            : xs) = helper f xs

-- | substitue tmp type var i to type t
subst :: [Constraint] -> Int -> AType Idx -> [Constraint]
subst cs i t =
    (\(Constraint fo ex) -> Constraint (ss fo i t) (ss ex i t)) <$> cs
  where
    ss (Counted x) f t | f == x = t
    ss (EnumType a ts) f t      = EnumType a $ map (\tpe -> ss tpe f t) ts
    ss tpe             _ _      = tpe

-- | solve type constraint
solveConstraint :: [Constraint] -> Either String ()
solveConstraint []                        = Right ()
solveConstraint (c@(Constraint f e) : cs) = case (f, e) of
    (Counted i  , AnyType    )          -> solveConstraint $ subst cs i AnyType
    (AnyType    , Counted i  )          -> solveConstraint $ subst cs i AnyType
    (_          , AnyType    )          -> solveConstraint cs
    (AnyType    , _          )          -> solveConstraint cs

    (IntType    , IntType    )          -> solveConstraint cs
    (BooleanType, BooleanType)          -> solveConstraint cs
    (StringType , StringType )          -> solveConstraint cs
    (UnitType   , UnitType   )          -> solveConstraint cs
    (TypeParam x, TypeParam y) | x == y -> solveConstraint cs
    (EnumType x tx, EnumType y ty) | x == y ->
        solveConstraint (zipWith Constraint tx ty ++ cs)

    (IntType      , Counted i    )  -> solveConstraint $ subst cs i IntType
    (BooleanType  , Counted i    )  -> solveConstraint $ subst cs i BooleanType
    (StringType   , Counted i    )  -> solveConstraint $ subst cs i StringType
    (UnitType     , Counted i    )  -> solveConstraint $ subst cs i UnitType
    (v@TypeParam{}, Counted i    )  -> solveConstraint $ subst cs i v
    (e@EnumType{} , Counted i    )  -> solveConstraint $ subst cs i e

    (Counted i    , IntType      )  -> solveConstraint $ subst cs i IntType
    (Counted i    , BooleanType  )  -> solveConstraint $ subst cs i BooleanType
    (Counted i    , StringType   )  -> solveConstraint $ subst cs i StringType
    (Counted i    , UnitType     )  -> solveConstraint $ subst cs i UnitType
    (Counted i    , v@TypeParam{})  -> solveConstraint $ subst cs i v
    (Counted i    , e@EnumType{} )  -> solveConstraint $ subst cs i e

    (Counted i, Counted j) | i == j -> solveConstraint cs
    (Counted i, Counted j)          -> solveConstraint $ subst cs j (Counted i)

    _ -> Left $ "Cannot unify type " ++ removeQuot f ++ " and " ++ removeQuot e

-- | check type and returned the solved constraint
checkType :: Program Idx -> SymbolTable -> Either String ()
checkType pg st = checkEnum pg st
    *> traverse_ solveConstraint (runConstraint (checkMain pg) st)
