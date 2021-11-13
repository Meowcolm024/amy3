module NameAnalysis where

import           Data.List
import qualified Data.Map                      as Map
import           Debug.Trace
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
addTypeConstrTh ~(EnumDef n tas cs) te = addConstrTh' cs emptySymbolMap
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
    :: Program String           -- ^ program
    -> SymbolMap String         -- ^ type symbol table
    -> SymbolMap String         -- ^ acc
    -> SymbolMap String         -- ^ constr symbol table
addConstrTh [] _ env = env
addConstrTh (d@EnumDef{} : ds) te env =
    addConstrTh ds te (Map.union (addTypeConstrTh d te) env)
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
    | nub ta /= ta || nub (paramName <$> args) /= (paramName <$> args)
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
    ts = addTypeTh defs emptySymbolMap
    cs = addConstrTh defs ts emptySymbolMap
    fs = addFuncTh defs ts cs
    e  = checkEntry defs

-- TODO: do name analysis on function body
-- idea: just like interpreting, go over the tree

-- name analysis on one function
analysisFunc :: Definition String -> Env String -> [String]
analysisFunc ~(FunDef n ta args ret body) env@(Env p g lt lv) = analyze
    body                                                -- function body
    env { localType = Map.union (typeArgsToEnv ta) lt   -- updated local type env
        , localVal  = Map.union (argsToEnv args) lv     -- updated local var env with arguments
        }
  where
    -- | analyze expr in func body
    -- return error message(s) if any
    analyze :: Expr String -> Env String -> [String]
    analyze expr lenv@(Env _ _ lt' lv') = case expr of
        Variable s -> case lookupValFun s lenv of
            Nothing ->
                [ "[Fatal] Unbounded variable ["
                      ++ s
                      ++ "] in function ["
                      ++ n
                      ++ "]"
                ]
            Just (TypeIns t) ->
                [ "[Fatal] Invalid type for [" ++ s ++ "]"
                | not (checkTypeEX t (getTypeEnv lenv))
                ]
            _ -> ["[Fatal] Not a variable [" ++ s ++ "]"]
        LitInt    i      -> []
        LitBool   b      -> []
        LitString s      -> []
        LitUnit          -> []
        Plus      ex ex' -> analyze ex lenv ++ analyze ex' lenv
        Minus     ex ex' -> analyze ex lenv ++ analyze ex' lenv
        Mult      ex ex' -> analyze ex lenv ++ analyze ex' lenv
        Div       ex ex' -> analyze ex lenv ++ analyze ex' lenv
        LessThan  ex ex' -> analyze ex lenv ++ analyze ex' lenv
        LessEqual ex ex' -> analyze ex lenv ++ analyze ex' lenv
        And       ex ex' -> analyze ex lenv ++ analyze ex' lenv
        Or        ex ex' -> analyze ex lenv ++ analyze ex' lenv
        Equals    ex ex' -> analyze ex lenv ++ analyze ex' lenv
        Concat    ex ex' -> analyze ex lenv ++ analyze ex' lenv
        Seq       ex ex' -> analyze ex lenv ++ analyze ex' lenv
        Not ex           -> analyze ex lenv
        Neg ex           -> analyze ex lenv
        Call s ats exs   -> case lookupValFun s lenv of
            Just (FunSig fats fargs _) ->
                -- check type application and argument count
                if length ats == length fats && length exs == length fargs
                    -- check valid type application
                    then if all (`checkTypeEX` getTypeEnv lenv) ats
                        -- check all the arguments
                        then concatMap (`analyze` lenv) exs
                        else ["[Fatal] Invalid type in function [" ++ s ++ "]"]
                    else
                        ["[Fatal] Incorrect parameter number for [" ++ s ++ "]"]
            Nothing -> ["[Fatal] Unbounded function [" ++ s ++ "]"]
            Just _  -> ["[Fatal] Not a function [" ++ s ++ "]"]
        ConstrCall s (EnumType _ ats) exs -> case lookupConstr s lenv of
            Just (ConstrSig cats cargs _) ->
                -- check type application and argument count
                if length ats == length cats && length exs == length cargs
                     -- check valid type application
                    then if all (`checkTypeEX` getTypeEnv lenv) ats
                        -- check all the arguments
                        then concatMap (`analyze` lenv) exs
                        else
                            [ "[Fatal] Invalid type in constructor ["
                              ++ s
                              ++ "]"
                            ]
                    else
                        ["[Fatal] Incorrect parameter number for [" ++ s ++ "]"]
            Nothing -> ["[Fatal] Unbounded constructor [" ++ s ++ "]"]
            Just _  -> ["[Fatal] Not a constructor [" ++ s ++ "]"]
        ConstrCall{}       -> error "*** Unknown error"
        Let (ln, la, e) ex -> if checkTypeEX la (getTypeEnv lenv)
            then analyze e lenv
                ++ analyze ex lenv { localVal = Map.insert ln (TypeIns la) lv' }
            else ["[Fatal] Invalid type for [" ++ ln ++ "]"]
        IfElse ex ex' ex3 -> concatMap (`analyze` lenv) [ex, ex', ex3]
        Match ex mcs -> analyze ex lenv ++ concatMap (`analyzeMatch` lenv) mcs
        Bottom ex -> analyze ex lenv

    analyzeMatch :: MatchCase String -> Env String -> [String]
    analyzeMatch (MatchCase pat ex) lenv@(Env _ _ lt' lv') =
        analyze ex (analyzePattern pat lenv)

    analyzePattern :: Pattern String -> Env String -> Env String
    analyzePattern pats lenv@(Env _ _ _ lv') = case pats of
        WildcardPattern                    -> lenv
        IdPattern s -> lenv { localVal = Map.insert s (TypeIns Unknown) lv' }
        LiteralPattern _                   -> lenv
        EnumPattern s (EnumType f _) pats' -> case lookupConstr s lenv of
            Just (ConstrSig _ cargs _) -> if length cargs == length pats'
                then
                    foldl   -- join all the pattern matched bindings
                            (\(Env _ _ _ x) acc@(Env _ _ _ y) ->
                                acc { localVal = Map.union x y }
                            )
                            lenv
                        $ map (`analyzePattern` lenv) pats'
                else
                    error
                    $  "[Fatal] Incorrect parameter number for ["
                    ++ s
                    ++ "]"
            _ ->
                error $ "[Fatal] Invalid constructor [" ++ f ++ "." ++ s ++ "]"
        _ -> error "*** Unknown error"

-- | analyze all functions using global env
analyzeAllFunc :: Program String -> Env String -> Program String
analyzeAllFunc defs env =
    let result = concatMap
            (`analysisFunc` env)
            ((mainFunc . entry . global) env : filter isFuncDef defs)
    in  case result of
            []  -> defs
            ers -> error $ "Name anaysis error summary:\n" ++ unlines ers

-- | name analysis on the whole program
-- returning the original program and the prototype symbol table
analyzeProgram :: Program String -> (Program String, TemplateTable)
analyzeProgram defs = let z = analyzeAllFunc defs env in z `seq` (z, tt)
  where
    tt  = analyzeDef defs
    env = Env primitives tt emptySymbolMap emptySymbolMap
