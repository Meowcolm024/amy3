module NameAnalysis where

import qualified Data.Map                      as Map
import           SymbolTable
import           Types
import           Utils

{-
    identifier/variable is counted separated in env
    variable, func param, type variable

    string -> idx

    we need to dynamically add type and functions to symbol table

    first add templates to symbol table
    then add types and functions to symbol table according their actual types

-}

-- | check whether there are multiple main functions
checkEntry :: [Definition String] -> Definition String
checkEntry defs =
    let et = filter isMainDef defs
    in  if length et == 1 && validEntry (head et)
            then head et
            else error "[Fatal] Duplicate/No entry point(s) found!"

-- | check if the main function has valid signature
validEntry :: Definition String -> Bool
validEntry (EntryPoint (FunDef _ [] [] UnitType _)) = True
validEntry _ = False

-- | generate symbol table for types (prototype)
addTypeTh :: [Definition String] -> SymbolMap String -> SymbolMap String
addTypeTh []                     env = env
addTypeTh (EnumDef n tas _ : ds) env = case Map.lookup n env of
    Just c  -> error $ "[Fatal] Multiple definitions for type [" ++ n ++ "]"
    Nothing -> addTypeTh ds (Map.insert n (TypeIns (EnumType n tas)) env)
addTypeTh (_ : ds) env = addTypeTh ds env

-- | add the constructors for that type
addTypeConstrTh :: Definition String -> SymbolMap String -> SymbolMap String
addTypeConstrTh ~(EnumDef n tas cs) te = addConstrTh' cs emptySymbolMap
  where
    -- env with global types and local type variables
    typeEnv = Map.union (typeArgsToEnv tas) te
    -- add constructor 
    addConstrTh' []                       env = env
    addConstrTh' (CaseDef cn pds _ : css) env = case checkTypeArg pds of
        Nothing -> addConstrTh' css $ Map.insert
            cn
            (ConstrSig tas (paramType <$> pds) (EnumType n tas))
            env
        Just t -> error $ "[Fatal] Unexpected type [" ++ show t ++ "]"
    -- check constrctor arguments
    checkTypeArg [] = Nothing
    checkTypeArg (ParamDef _ t : ts) =
        if checkTypeEX t typeEnv then checkTypeArg ts else Just t

-- | generate symbol table for type constructors (prototype)
addConstrTh
    :: [Definition String]      -- ^ program
    -> SymbolMap String         -- ^ type symbol table
    -> SymbolMap String         -- ^ acc
    -> SymbolMap String         -- ^ constr symbol table
addConstrTh [] _ env = env
addConstrTh (d@EnumDef{} : ds) te env =
    addConstrTh ds te (Map.union (addTypeConstrTh d te) env)
addConstrTh (_ : ds) te env = addConstrTh ds te env

-- | generate symbol table for functions (prototype)
addFuncTh
    :: [Definition String]      -- ^ program
    -> SymbolMap String         -- ^ type symbol table
    -> SymbolMap String         -- ^ acc
    -> SymbolMap String         -- ^ func symbol table
addFuncTh [] _ env = env
addFuncTh (FunDef n ta args ret _ : fs) te env =
    if all ((`checkTypeEX` Map.union (typeArgsToEnv ta) te) . paramType) args
        then addFuncTh fs te
            $ Map.insert n (FunSig ta (map paramType args) ret) env
        else error $ "[Fatal] Unexpectd type in definition [" ++ n ++ "]"
addFuncTh (_ : fs) te env = addFuncTh fs te env

-- navie symbol table generation
analysis :: [Definition String] -> SymbolTable String
analysis defs = SymbolTable ts fs cs e
  where
    ts = addTypeTh defs emptySymbolMap
    cs = addConstrTh defs ts emptySymbolMap
    fs = addFuncTh defs ts cs
    e  = checkEntry defs
