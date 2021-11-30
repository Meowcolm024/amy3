module NameAnalysis
    ( analyze
    ) where

import           Control.Monad                  ( when )
import           Control.Monad.Except           ( throwError )
import           Control.Monad.Trans.Class      ( MonadTrans(lift) )
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import qualified Data.Map                      as Map
import           Data.Maybe                     ( isJust )
import           SymbolTable
import           Types
import           Utils                          ( isMainDef )

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
    (TableST t _ _ _) <- lift get
    pure t
addTypes (EnumDef name targs _ : ts) = do
    let targs' = addTargs targs 0
    (TableST (SymbolTable dn ty f c) env lidx idx) <- lift get
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
        env
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

-- | discover type constructors
addConstrs :: Program String -> Analysis SymbolTable
addConstrs [] = do
    (TableST t _ _ _) <- lift get
    pure t
addConstrs (EnumDef name targs csts : ts) = do
    let targs' = typeParamBinding targs
    mapM_ (addC targs') csts
    addConstrs ts
  where
    addC :: [(String, Idx)] -> CaseDef String -> Analysis ()
    addC targs' ~(CaseDef caseName params (EnumType name _)) = do
        (TableST s@(SymbolTable dn ty f c) env lidx idx) <- lift get
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
                            env
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
                                env
                                lidx
                                (idx + 1)
addConstrs (_ : ts) = addConstrs ts

-- | add fuction signatures
addFuncSig :: Program String -> Analysis SymbolTable
addFuncSig [] = do
    (TableST t _ _ _) <- lift get
    pure t
addFuncSig (EntryPoint f                   : st) = addFuncSig (f : st)
addFuncSig (FunDef name targs params ret _ : st) = do
    (TableST s@(SymbolTable dn ty f c) env lidx idx) <- lift get
    -- local type vars shadows global types
    let targs' = typeParamBinding targs
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
                env
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
    tb@(TableST s _ _ _) <- lift get
    let targs'           = typeParamBinding targs
    let Right (rt : tps) = checkParams targs' s (ParamDef "ret" ret : params)
    let ps               = trParams (zip params tps) (length targs')
    let Just fi          = lookupName name s
    lift . put $ tb { localIndex = length targs' }  -- reset local index
    lb  <- addBindings params                       -- init local bindings
    tb' <- lift get                                 -- get updated table
    lift . put $ tb' { locals = Map.union lb $ Map.fromList targs' }  -- reset locals
    result <- tf body                               -- analyze body
    rest   <- tranfromFunc st                       -- analyze rest
    pure $ FunDef fi (map (TypeParam . snd) targs') ps rt result : rest
  where
    -- add binding s to local env
    addBindings []                       = pure Map.empty
    addBindings (ParamDef name _ : rest) = do
        t@(TableST _ _ i _) <- lift get
        lift . put $ t { localIndex = i + 1 }
        rest <- addBindings rest
        pure $ Map.insert name (Idx name i) rest
    -- transform paramdef to idx
    trParams [] _ = []
    trParams ((ParamDef n _, ty) : rs) i =
        ParamDef (Idx n i) ty : trParams rs (i + 1)
    -- | transform expression
    tf :: Expr String -> Analysis (Expr Idx)
    tf expr = do
        tb@(TableST s@(SymbolTable dn ty f c) env lidx idx) <- lift get
        case expr of
            Variable v -> case Map.lookup v env of
                Nothing -> throwError $ "Unbounded variable : " ++ v
                Just i  -> pure $ Variable i
            LitInt    n      -> pure $ LitInt n
            LitBool   b      -> pure $ LitBool b
            LitString str    -> pure $ LitString str
            LitUnit          -> pure LitUnit
            Plus      ex ex' -> Plus <$> tf ex <*> tf ex'
            Minus     ex ex' -> Minus <$> tf ex <*> tf ex'
            Mult      ex ex' -> Mult <$> tf ex <*> tf ex'
            Div       ex ex' -> Div <$> tf ex <*> tf ex'
            LessThan  ex ex' -> LessThan <$> tf ex <*> tf ex'
            LessEqual ex ex' -> LessEqual <$> tf ex <*> tf ex'
            And       ex ex' -> And <$> tf ex <*> tf ex'
            Or        ex ex' -> Or <$> tf ex <*> tf ex'
            Equals    ex ex' -> Equals <$> tf ex <*> tf ex'
            Concat    ex ex' -> Concat <$> tf ex <*> tf ex'
            Seq       ex ex' -> Seq <$> tf ex <*> tf ex'
            Not ex           -> Not <$> tf ex
            Neg ex           -> Neg <$> tf ex
            Call fun exs     -> case lookupName fun s of
                Nothing -> throwError $ "Unknown function: " ++ fun
                Just fx -> Call fx <$> traverse tf exs
            ConstrCall cs (EnumType ty tas) exs -> case lookupConstr ty cs s of
                Just (cid, ConstrSig _ _ pt) ->
                    ConstrCall cid pt <$> traverse tf exs
                _ ->
                    throwError
                        $  "Unknown constructor "
                        ++ cs
                        ++ " for type "
                        ++ ty
            Let (ParamDef name t) ex ex' -> do
                let name' = Idx name lidx
                e1 <- tf ex
                lift . put $ tb { locals     = Map.insert name name' env
                                , localIndex = lidx + 1
                                }
                Let
                    <$> (ParamDef name' <$> refactorType t)
                    <*> pure e1
                    <*> tf ex'
            IfElse ex ex' ex3 -> IfElse <$> tf ex <*> tf ex' <*> tf ex3
            Match ex mcs      -> Match <$> tf ex <*> traverse handleCases mcs
            Bottom ex         -> Bottom <$> tf ex
            _                 -> throwError "???"
    -- transform types
    refactorType :: AType String -> Analysis (AType Idx)
    refactorType (TypeParam t) = do
        tb <- lift get
        case Map.lookup t (locals tb) of
            Nothing  -> throwError $ "Unbounded type variable: " ++ t
            Just idx -> pure $ TypeParam idx
    refactorType (EnumType name targs) = do
        TableST s _ _ _ <- lift get
        case lookupName name s of
            Nothing  -> throwError $ "Unbounded type variable: " ++ name
            Just idx -> EnumType idx <$> traverse refactorType targs
    refactorType t = pure $ fmap (const (Idx "" 0)) t

    handleCases :: MatchCase String -> Analysis (MatchCase Idx)
    handleCases (MatchCase pat expr) = MatchCase <$> handlePat pat <*> tf expr

    handlePat :: Pattern String -> Analysis (Pattern Idx)
    handlePat WildcardPattern       = pure WildcardPattern
    handlePat (LiteralPattern i   ) = LiteralPattern <$> tf i
    handlePat (IdPattern      name) = do
        tb@(TableST s@(SymbolTable dn ty f c) env lidx idx) <- lift get
        case Map.lookup name env of
            Just _  -> throwError $ "Variable already defined: " ++ name
            Nothing -> do
                let name' = Idx name lidx
                lift . put $ tb { locals     = Map.insert name name' env
                                , localIndex = lidx + 1
                                }
                pure $ IdPattern name'
    handlePat ~(EnumPattern cn (EnumType tn _) pats) = do
        tb@(TableST s@(SymbolTable dn ty f c) env lidx idx) <- lift get
        case lookupConstr tn cn s of
            Just (cid, ConstrSig _ _ pt) ->
                EnumPattern cid pt <$> traverse handlePat pats
            _ ->
                throwError $ "Unknown constructor " ++ cn ++ " for type " ++ tn
-- transform enum definition
tranfromFunc (EnumDef name targs cases : st) = do
    (TableST s@(SymbolTable dn ty f c) _ _ _) <- lift get
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
