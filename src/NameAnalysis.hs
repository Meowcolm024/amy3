module NameAnalysis where

import           Control.Monad.Trans.State
import qualified Data.List                     as List
import qualified Data.Map                      as Map
import           SymbolTable
import           Types
import           Utils

-- | check whether there are multiple main functions
checkEntry :: Program String -> Definition String
checkEntry defs =
    let et = filter isMainDef defs
    in  if length et == 1 && validEntry (head et)
            then head et
            else error "[Fatal] Invalid entry point(s)"

-- | check if the main function has valid signature
validEntry :: Definition String -> Bool
validEntry (EntryPoint (FunDef _ [] [] UnitType _)) = True
validEntry _ = False

-- | generate symbol table for types (prototype)
addTypeTh :: Program String -> SymbolMap String -> SymbolMap String
addTypeTh []                     env = env
addTypeTh (EnumDef n tas _ : ds) env = case Map.lookup n env of
    Just c  -> error $ "[Fatal] Multiple definitions for type [" ++ n ++ "]"
    Nothing -> addTypeTh ds (Map.insert n (TypeIns (EnumType n tas)) env)
addTypeTh (_ : ds) env = addTypeTh ds env

-- | add the constructors for that type
addTypeConstrTh :: Definition String -> SymbolMap String -> SymbolMap String
addTypeConstrTh ~(EnumDef n tas cs) te = addConstrTh' cs Map.empty
  where
    -- env with global types and local type variables
    typeEnv = Map.union (typeArgsToEnv tas) te
    -- add constructor 
    addConstrTh' [] env = env
    addConstrTh' (CaseDef cn _ _ : css) env | Map.member cn env =
        error $ "[Fatal] Redefinition of constructor [" ++ n ++ "." ++ cn ++ "]"
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
    :: Program String                       -- ^ program
    -> SymbolMap String                     -- ^ type symbol table
    -> Map.Map String (SymbolMap String)    -- ^ acc
    -> Map.Map String (SymbolMap String)    -- ^ constr symbol table
addConstrTh [] _ env = env
addConstrTh (d@(EnumDef tn _ _) : ds) te env =
    addConstrTh ds te $ Map.adjust (Map.union (addTypeConstrTh d te)) tn env
addConstrTh (_ : ds) te env = addConstrTh ds te env

-- | generate symbol table for functions (prototype)
addFuncTh
    :: Program String           -- ^ program
    -> SymbolMap String         -- ^ type symbol table
    -> SymbolMap String         -- ^ acc
    -> SymbolMap String         -- ^ func symbol table
addFuncTh [] _ env = env
addFuncTh (FunDef n _ _ _ _ : fs) _ env | Map.member n env =
    error $ "[Fatal] Multiple definitions of function [" ++ n ++ "]"
addFuncTh (FunDef n ta args _ _ : fs) _ _
    | List.nub ta /= ta || List.nub (paramName <$> args) /= (paramName <$> args)
    = error
        $  "[Fatal] Type parameters/arguments have dulplicate names in ["
        ++ n
        ++ "]"
addFuncTh (FunDef n ta args ret _ : fs) te env =
    if all (`checkTypeEX` Map.union (typeArgsToEnv ta) te)
           (ret : map paramType args)       -- return type and argument types
        then addFuncTh fs te
            $ Map.insert n (FunSig ta (map paramType args) ret) env
        else error $ "[Fatal] Unexpectd type in definition [" ++ n ++ "]"
addFuncTh (_ : fs) te env = addFuncTh fs te env

-- symbol table generation
-- for all the prototype definitions
-- unfortunately, we have traversed the list 4 times orz
analyzeDef :: Program String -> TemplateTable
analyzeDef defs = SymbolTable ts fs cs e
  where
    ts = addTypeTh defs Map.empty
    cs = addConstrTh defs ts (Map.map (const Map.empty) ts)
    fs = addFuncTh defs ts Map.empty
    e  = checkEntry defs

-- TODO: do name analysis on function body
