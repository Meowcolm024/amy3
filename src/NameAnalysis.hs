module NameAnalysis
    ( analyze
    ) where

import           Control.Monad                  ( when )
import           Control.Monad.Except           ( liftEither
                                                , throwError
                                                )
import           Control.Monad.Trans.Class      ( MonadTrans(lift) )
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import qualified Data.List                     as L
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )
import           SymbolTable
import           Types
import           Utils                          ( isMainDef )

-- | stateful symbol table with exceptions
type Analysis a = ExceptT String (State TableST) a

-- | check whether there are multiple main functions or invalid signature
checkEntry :: Program String -> Analysis (Program String)
checkEntry defs = case et of
    []  -> pure defs
    [e] -> if validEntry e
        then pure defs
        else throwError "Invalid entry point signature"
    _ -> throwError "Multipule entry points found"
  where
    -- all the mains
    et = filter isMainDef defs
    -- | check if the main function has valid signature
    validEntry (EntryPoint (FunDef _ [] [] UnitType _)) = True
    validEntry _ = False

-- | index type args locally
addTargs :: [AType String] -> Int -> [AType Idx]
addTargs []                 _ = []
addTargs (TypeParam a : ts) i = TypeParam (Idx a i) : addTargs ts (i + 1)
addTargs (_           : ts) i = addTargs ts i

-- | discover types
addTypes :: Program String -> Analysis SymbolTable
addTypes [] = do
    (TableST t _ _) <- lift get
    pure t
addTypes (EnumDef name targs _ : ts) = do
    let targs' = addTargs targs 0
    (TableST (SymbolTable dn ty f c) lidx idx) <- lift get
    -- abort when already defined
    when (isJust $ Map.lookup name dn) $ throwError $ "Redefinition of " ++ name
    -- add to symbol table
    lift . put $ TableST
        (SymbolTable
            (Map.insert name (Idx name idx) dn)
            (Map.insert (Idx name idx)
                        (TypeSig (EnumType (Idx name idx) targs'))
                        ty
            )
            f
            c
        )
        lidx
        (idx + 1)
    addTypes ts
addTypes (_ : ts) = addTypes ts

-- | check name in param
-- also distinguish type variable and enum type
checkParamName :: ParamDef String -> SymbolTable -> Either String (AType Idx)
checkParamName p@(ParamDef _ t) tb@(SymbolTable dn ty f c) = case t of
    EnumType n args -> do
        case Map.lookup n dn of
            Just i -> case Map.lookup i ty of
                Just (TypeSig (EnumType n' targs)) | nameIdx n' == n ->
                    EnumType i
                        <$> traverse
                                (\x -> checkParamName (ParamDef "" x) tb)
                                args
                _ -> if not (null args)
                    then Left $ "Type application in type variable: " ++ n
                    else Right $ TypeParam i
            Nothing -> Left $ "Unexpected type: " ++ n
    TypeParam a -> case Map.lookup a dn of
        Just i  -> Right $ TypeParam i
        Nothing -> Left $ "Unexpected type: " ++ a
    x -> Right $ fmap (const (Idx "" 0)) x


-- helper function to check param defs
checkParams targs s@(SymbolTable dn _ _ _) = traverse
    (`checkParamName` s { defsByName = Map.fromList targs `Map.union` dn })

typeParamBinding :: [AType String] -> [(String, Idx)]
typeParamBinding targs =
    zipWith (\(TypeParam x) (TypeParam y) -> (x, y)) targs (addTargs targs 0)

checkDup :: String -> [String] -> Either String ()
checkDup name ns = if length ns /= length (L.nub ns)
    then Left $ "Arguments of " ++ name ++ " constians duplicated names."
    else Right ()

-- | discover type constructors
addConstrs :: Program String -> Analysis SymbolTable
addConstrs [] = do
    (TableST t _ _) <- lift get
    pure t
addConstrs (EnumDef name targs csts : ts) = do
    let targs' = typeParamBinding targs
    mapM_ (addC targs') csts
    addConstrs ts
  where
    addC :: [(String, Idx)] -> CaseDef String -> Analysis ()
    addC targs' ~(CaseDef caseName params (EnumType name _)) = do
        (TableST s@(SymbolTable dn ty f c) lidx idx) <- lift get
        -- check duplicate names
        liftEither $ checkDup caseName $ map (\(ParamDef n _) -> n) params
        -- check if all the params 
        case checkParams targs' s params of
            Left  ss  -> throwError ss
            Right pts -> case Map.lookup name dn of
                Nothing    -> throwError $ "Unexpected type: " ++ name
                Just parId -> do
                    let Just (TypeSig p@(EnumType _ tags)) =
                            Map.lookup parId ty
                    let
                        nw = Map.singleton (Idx caseName idx)
                                           (ConstrSig tags pts p)
                    let parCsts = Map.lookup parId c
                    case parCsts of
                        Nothing -> lift . put $ TableST
                            (s
                                { defsByName   = Map.insert
                                                     (name ++ "." ++ caseName)
                                                     (Idx caseName idx)
                                                     dn
                                , constructors = Map.insert parId nw c
                                }
                            )
                            lidx
                            (idx + 1)
                        Just m -> do
                            -- abort when already defined
                            when (isJust $ lookupConstr name caseName s)
                                $  throwError
                                $  "Redefinition of constructor "
                                ++ caseName
                                ++ " in definition of "
                                ++ name
                            lift . put $ TableST
                                (s
                                    { defsByName   = Map.insert
                                                         (name ++ "." ++ caseName)
                                                         (Idx caseName idx)
                                                         dn
                                    , constructors = Map.insert
                                                         parId
                                                         (Map.union nw m)
                                                         c
                                    }
                                )
                                lidx
                                (idx + 1)
addConstrs (_ : ts) = addConstrs ts

-- | add fuction signatures
addFuncSig :: Program String -> Analysis SymbolTable
addFuncSig [] = do
    (TableST t _ _) <- lift get
    pure t
addFuncSig (EntryPoint f                   : st) = addFuncSig (f : st)
addFuncSig (FunDef name targs params ret _ : st) = do
    (TableST s@(SymbolTable dn ty f c) lidx idx) <- lift get
    -- local type vars shadows global types
    let targs' = typeParamBinding targs
    -- check duplicate names
    liftEither $ checkDup name $ map (\(ParamDef n _) -> n) params
    -- abort when already defined
    when (isJust $ Map.lookup name dn) $ throwError $ "Redefinition of " ++ name
    case checkParams targs' s (ParamDef "ret" ret : params) of
        Left  str       -> throwError str
        Right ~(r : ps) -> do
            let nw = FunSig (map (TypeParam . snd) targs') ps r
            lift . put $ TableST
                (s { defsByName = Map.insert name (Idx name idx) dn
                   , functions  = Map.insert (Idx name idx) nw f
                   }
                )
                lidx
                (idx + 1)
    addFuncSig st
addFuncSig (_ : st) = addFuncSig st

-- | transform dunction body
tranfromFunc :: Program String -> Analysis (Program Idx)
tranfromFunc [] = pure []
tranfromFunc (EntryPoint e : st) =
    (++) <$> ((EntryPoint <$>) <$> tranfromFunc [e]) <*> tranfromFunc st
tranfromFunc (FunDef name targs params ret body : st) = do
    tb@(TableST s _ _) <- lift get
    let targs'           = typeParamBinding targs
    let Right (rt : tps) = checkParams targs' s (ParamDef "ret" ret : params)
    let ps               = trParams (zip params tps) (length targs')
    let Just fi          = lookupName name s
    let (nls, lidx) = addBindings params (Map.fromList targs', length targs') -- init local bindings
    lift . put $ tb { localIndex = lidx }
    result <- tf body nls                           -- analyze body
    rest   <- tranfromFunc st                       -- analyze rest
    pure $ FunDef fi (map (TypeParam . snd) targs') ps rt result : rest
  where
    -- add binding s to local env
    addBindings [] mp = mp
    addBindings (ParamDef name _ : rest) (env, i) =
        addBindings rest (Map.insert name (Idx name i) env, i + 1)
    -- transform paramdef to idx
    trParams [] _ = []
    trParams ((ParamDef n _, ty) : rs) i =
        ParamDef (Idx n i) ty : trParams rs (i + 1)
    -- | transform expression
    tf :: Expr String -> Local -> Analysis (Expr Idx)
    tf expr env = do
        tb@(TableST s@(SymbolTable dn ty f c) lidx idx) <- lift get
        case expr of
            Variable v -> case Map.lookup v env of
                Nothing -> throwError $ "Unbounded variable : " ++ v
                Just i  -> pure $ Variable i
            LitInt    n      -> pure $ LitInt n
            LitBool   b      -> pure $ LitBool b
            LitString str    -> pure $ LitString str
            LitUnit          -> pure LitUnit
            Plus      ex ex' -> Plus <$> tf ex env <*> tf ex' env
            Minus     ex ex' -> Minus <$> tf ex env <*> tf ex' env
            Mult      ex ex' -> Mult <$> tf ex env <*> tf ex' env
            Div       ex ex' -> Div <$> tf ex env <*> tf ex' env
            Mod       ex ex' -> Mod <$> tf ex env <*> tf ex' env
            LessThan  ex ex' -> LessThan <$> tf ex env <*> tf ex' env
            LessEqual ex ex' -> LessEqual <$> tf ex env <*> tf ex' env
            And       ex ex' -> And <$> tf ex env <*> tf ex' env
            Or        ex ex' -> Or <$> tf ex env <*> tf ex' env
            Equals    ex ex' -> Equals <$> tf ex env <*> tf ex' env
            Concat    ex ex' -> Concat <$> tf ex env <*> tf ex' env
            Seq       ex ex' -> Seq <$> tf ex env <*> tf ex' env
            Not ex           -> Not <$> tf ex env
            Neg ex           -> Neg <$> tf ex env
            Call fun exs     -> case lookupName fun s of
                Nothing -> throwError $ "Unknown function: " ++ fun
                Just fx ->
                    let FunSig _ pms _ = fromJust (Map.lookup fx f)
                    in  if length pms /= length exs
                            then
                                throwError
                                $  "Wrong argument number of function "
                                ++ fun
                            else Call fx <$> traverse (`tf` env) exs
            ConstrCall cs (EnumType ty tas) exs -> case lookupConstr ty cs s of
                Just (cid, ConstrSig _ pms pt) -> if length pms /= length exs
                    then
                        throwError
                        $  "Wrong argument number of constructor "
                        ++ cs
                    else ConstrCall cid pt <$> traverse (`tf` env) exs
                _ ->
                    throwError
                        $  "Unknown constructor "
                        ++ cs
                        ++ " for type "
                        ++ ty
            Let (ParamDef name t) ex ex' -> do
                let name' = Idx name lidx
                e1 <- tf ex env
                lift . put $ tb { localIndex = lidx + 1 }
                Let
                    <$> (ParamDef name' <$> refactorType t env)
                    <*> pure e1
                    <*> tf ex' (Map.insert name name' env)
            IfElse ex ex' ex3 ->
                IfElse <$> tf ex env <*> tf ex' env <*> tf ex3 env
            Match ex mcs -> do
                tb@(TableST s _ _) <- lift get
                Match <$> tf ex env <*> traverse (handleCases env) mcs
            Bottom ex -> Bottom <$> tf ex env
            _         -> throwError "???"
    -- transform types
    refactorType :: AType String -> Local -> Analysis (AType Idx)
    refactorType (TypeParam t) env = do
        case Map.lookup t env of
            Nothing  -> throwError $ "Unbounded type variable: " ++ t
            Just idx -> pure $ TypeParam idx
    refactorType (EnumType name targs) env = do
        TableST s _ _ <- lift get
        case lookupName name s of
            Nothing  -> throwError $ "Unbounded type variable: " ++ name
            Just idx -> EnumType idx <$> traverse (`refactorType` env) targs
    refactorType t env = pure $ fmap (const (Idx "" 0)) t

    handleCases :: Local -> MatchCase String -> Analysis (MatchCase Idx)
    handleCases env (MatchCase pat expr) = do
        (pt, lc) <- handlePat pat env
        rs       <- tf expr (Map.union lc env)
        pure $ MatchCase pt rs

    handlePat :: Pattern String -> Local -> Analysis (Pattern Idx, Local)
    handlePat WildcardPattern    _   = pure (WildcardPattern, Map.empty)
    handlePat (LiteralPattern i) env = do
        rs <- tf i env
        pure (LiteralPattern rs, Map.empty)
    handlePat (IdPattern name) env = do
        tb@(TableST s@(SymbolTable dn ty f c) lidx idx) <- lift get
        case Map.lookup name env of
            Just _  -> throwError $ "Variable already defined: " ++ name
            Nothing -> do
                let name' = Idx name lidx
                lift . put $ tb { localIndex = lidx + 1 }
                pure (IdPattern name', Map.insert name name' env)
    handlePat ~(EnumPattern cn (EnumType tn _) pats) env = do
        tb@(TableST s@(SymbolTable dn ty f c) lidx idx) <- lift get
        case lookupConstr tn cn s of
            Just (cid, ConstrSig _ _ pt) -> do
                (pts, env') <- unzip <$> traverse (`handlePat` env) pats
                -- EnumPattern cid pt
                pure (EnumPattern cid pt pts, foldr Map.union Map.empty env')
            _ ->
                throwError $ "Unknown constructor " ++ cn ++ " for type " ++ tn
-- transform enum definition
tranfromFunc (EnumDef name targs cases : st) = do
    (TableST s@(SymbolTable dn ty f c) lidx _) <- lift get
    let Just nidx                       = Map.lookup name dn
    let Just (TypeSig (EnumType _ ats)) = Map.lookup nidx ty
    let
        Just cs = (`traverse` cases) $ \(CaseDef cname params _) -> do
            (idx, ConstrSig ats ps par) <- lookupConstr name cname s
            pure $ CaseDef
                idx
                (zipWith3 (\(ParamDef n _) i -> ParamDef (Idx n i))
                          params
                          [0 ..]
                          ps
                )
                par
    rest <- tranfromFunc st
    pure $ EnumDef nidx ats cs : rest

-- | do name analysis
analyze :: Program String -> Either String (SymbolTable, Program Idx)
analyze p = evalState
    (runExceptT $ do
        x    <- checkEntry p
        symb <- addTypes x *> addConstrs x *> addFuncSig x
        pg   <- tranfromFunc x
        pure (symb, pg)
    )
    emptyTable
