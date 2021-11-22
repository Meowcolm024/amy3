module NameAnalysis where

import           Control.Monad                  ( when )
import           Control.Monad.Except           ( throwError )
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import qualified Data.List                     as List
import qualified Data.Map                      as Map
import           SymbolTable
import           Types
import           Utils

type Anaylsis a = ExceptT String (State TableST) a

-- | check whether there are multiple main functions
checkEntry :: Program String -> Either String (Definition String)
checkEntry defs = if length et == 1 && validEntry (head et)
    then pure $ head et
    else Left "[Fatal] Invalid entry point(s)"
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
addTypes :: Program String -> Anaylsis SymbolTable
addTypes [] = do
    (TableST t _) <- lift get
    pure t
addTypes (EnumDef name targs _ : ts) = do
    let targs' = addTargs targs 0
    (TableST (SymbolTable dn ty f c) idx) <- lift get
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
        (idx + 1)
    addTypes ts
addTypes (_ : ts) = addTypes ts

-- | check name in param
checkParamName :: ParamDef String -> TableST -> Either String (AType Idx)
checkParamName p@(ParamDef _ t) tb@(TableST (SymbolTable dn ty f c) idx) =
    case t of
        EnumType n args -> do
            case Map.lookup n dn of
                Just i -> do
                    let Just (TypeSig (EnumType n targs)) = Map.lookup i ty
                    EnumType i
                        <$> traverse
                                (\x -> checkParamName (ParamDef "" x) tb)
                                args
                Nothing -> Left $ "Unexpect type: " ++ n
        TypeParam a -> case Map.lookup a dn of
            Just i  -> Right $ TypeParam i
            Nothing -> Left $ "Unexpect type: " ++ a
        x -> Right $ fmap (const (Idx "" 0)) x

-- | discover type constructors
addConstrs :: Program String -> Anaylsis SymbolTable
addConstrs [] = do
    (TableST t _) <- lift get
    pure t
addConstrs (EnumDef name targs csts : ts) = do
    let targs' = zipWith (\(TypeParam x) (TypeParam y) -> (x, y))
                         targs
                         (addTargs targs 0)
    mapM_ (addC targs') csts
    addConstrs ts
  where
    addC :: [(String, Idx)] -> CaseDef String -> Anaylsis ()
    addC targs' ~(CaseDef caseName params (EnumType name _)) = do
        (TableST s@(SymbolTable dn ty f c) idx) <- lift get
        -- check if all the params 
        let r = traverse
                (`checkParamName` TableST
                    s { defsByName = Map.fromList targs' `Map.union` dn }
                    idx
                )
                params
        case r of
            Left  ss  -> throwError ss
            Right pts -> case Map.lookup name dn of
                Nothing    -> throwError $ "Unexpect type: " ++ name
                Just parId -> do
                    let Just (TypeSig p@(EnumType _ tags)) =
                            Map.lookup parId ty
                    let
                        nw = Map.singleton
                            (Idx (name ++ "." ++ caseName) idx)
                            (ConstrSig tags pts p)
                    lift . put $ case Map.lookup parId c of
                        Nothing -> TableST
                            (s { constructors = Map.insert parId nw c })
                            (idx + 1)
                        Just m -> TableST
                            (s
                                { constructors = Map.insert parId
                                                            (Map.union nw m)
                                                            c
                                }
                            )
                            (idx + 1)
addConstrs (_ : ts) = addConstrs ts

-- | add fuction signatures
addFuncSig :: Program String -> Anaylsis SymbolTable
addFuncSig [] = do
    (TableST t _) <- lift get
    pure t
addFuncSig (EntryPoint f                   : st) = addFuncSig (f : st)
addFuncSig (FunDef name targs params ret _ : st) = do
    (TableST s@(SymbolTable dn ty f c) idx) <- lift get
    -- local type vars shadows global types
    let targs' = zipWith (\(TypeParam x) (TypeParam y) -> (x, y))
                         targs
                         (addTargs targs 0)
    let r = traverse
            (`checkParamName` TableST
                s { defsByName = Map.fromList targs' `Map.union` dn }
                idx
            )
            (ParamDef "ret" ret : params)
    case r of
        Left  str       -> throwError str
        Right ~(r : ps) -> do
            let nw = FunSig (map (TypeParam . snd) targs') ps r
            lift . put $ TableST
                (s { functions = Map.insert (Idx name idx) nw f })
                (idx + 1)
    addFuncSig st
addFuncSig (_ : st) = addFuncSig st

-- | transform dunction body
tranfromFunc :: Program String -> Anaylsis (Program Idx)
tranfromFunc [] = pure []
tranfromFunc (EntryPoint e : st) =
    (++) <$> ((EntryPoint <$>) <$> tranfromFunc [e]) <*> tranfromFunc st
tranfromFunc (FunDef name targs params ret body : st) = do
    (TableST s@(SymbolTable dn ty f c) idx) <- lift get
    -- TODO
    tranfromFunc st
tranfromFunc (_ : st) = tranfromFunc st

-- testAnalysis :: Program String -> TableST
testAnalysis x = evalState
    (runExceptT $ addTypes x *> addConstrs x *> addFuncSig x)
    emptyTable
