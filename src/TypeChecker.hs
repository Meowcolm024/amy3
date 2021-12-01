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
type Env = Map.Map Idx (AType Idx)

type CheckState a = ExceptT String (State Int) a

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
            i <- lift get
            lift . put $ i + 1
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
            i <- lift get
            lift . put $ i + length targs
            let (rt : ps) = typeApp
                    (zip (map (\(TypeParam x) -> x) targs) [i ..])
                    (ret : params)
            pcs <- zipWithM (\p q -> genCons p q env) exs ps
            pure $ concat pcs ++ [Constraint rt expected]
        ConstrCall idx at exs -> do
            let EnumType p _ = at
            let Just (ConstrSig targs params ret) =
                    Map.lookup p (constructors st) >>= Map.lookup idx
            i <- lift get
            lift . put $ i + length targs
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
            i  <- lift get
            lift . put $ i + 1
            el <- genCons ex' (Counted i) env
            at <- genCons ex3 (Counted i) env
            pure $ pr ++ el ++ at ++ [Constraint (Counted i) expected]
        Match ex mcs -> do
            i <- lift get
            lift . put $ i + 1
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
            i <- lift get
            lift . put $ i + length targs
            let
                ps = typeApp (zip (map (\(TypeParam x) -> x) targs) [i ..])
                             params
            (cs, es) <- unzip <$> zipWithM handlePat pats ps
            pure
                ( concat cs {- ++ [Constraint at expected] -}
                , foldr Map.union Map.empty es
                )

-- | eval state and get the constarint
runConstraint :: CheckState [Constraint] -> Either String [Constraint]
runConstraint cs = evalState (runExceptT cs) 0

testCons :: [Definition Idx] -> SymbolTable -> [[Constraint]]
testCons dfs st = concat . sequence $  helper (\d -> runConstraint (genConstraint d st)) dfs
  where
    helper _ []                = []
    helper f (x@FunDef{} : xs) = f x : helper f xs
    helper f (_          : xs) = helper f xs

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
solveConstraint []                    = Right ()
solveConstraint (Constraint f e : cs) = undefined
