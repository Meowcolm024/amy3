module Interpreter
    ( interpret
    , execMain
    ) where

import           Control.Monad
import           Data.Functor                   ( ($>) )
import qualified Data.Map                      as Map
import           SymbolTable
import           System.IO
import           Text.Read                      ( readMaybe )
import           Types
import           Utils

type Env = Map.Map Idx (Expr Idx)

-- execMain :: Definition Idx -> IO ()
execMain :: Program Idx -> SymbolTable -> FuncTable -> IO ()
execMain [] _ _ = pure ()
execMain (EntryPoint (FunDef _ _ _ _ body) : _) st ft =
    void $ interpret body Map.empty st ft
execMain (_ : r) st ft = execMain r st ft

interpret :: Expr Idx -> Env -> SymbolTable -> FuncTable -> IO (Expr Idx)
interpret ex ev st ft = eval ex ev
  where
    eval :: Expr Idx -> Env -> IO (Expr Idx)
    eval expr env = case expr of
        Variable idx -> case Map.lookup idx env of
            Just ex' -> pure ex'
            Nothing  -> evalError $ "Unbounded variable " ++ nameIdx idx
        LitInt    n  -> pure $ LitInt n
        LitBool   b  -> pure $ LitBool b
        LitString s  -> pure $ LitString s
        LitUnit      -> pure LitUnit
        Plus ex' ex3 -> do
            LitInt lhs <- eval ex' env
            LitInt rhs <- eval ex3 env
            pure $ LitInt (lhs + rhs)
        Minus ex' ex3 -> do
            LitInt lhs <- eval ex' env
            LitInt rhs <- eval ex3 env
            pure $ LitInt (lhs - rhs)
        Mult ex' ex3 -> do
            LitInt lhs <- eval ex' env
            LitInt rhs <- eval ex3 env
            pure $ LitInt (lhs * rhs)
        Div ex' ex3 -> do
            LitInt lhs <- eval ex' env
            LitInt rhs <- eval ex3 env
            case rhs of
                0 -> evalError "Divided by Zero"
                _ -> pure $ LitInt (lhs `div` rhs)
        Mod ex' ex3 -> do
            LitInt lhs <- eval ex' env
            LitInt rhs <- eval ex3 env
            case rhs of
                0 -> evalError "Divided by Zero"
                _ -> pure $ LitInt (lhs `mod` rhs)
        LessThan ex' ex3 -> do
            LitInt lhs <- eval ex' env
            LitInt rhs <- eval ex3 env
            pure $ LitBool (lhs < rhs)
        LessEqual ex' ex3 -> do
            LitInt lhs <- eval ex' env
            LitInt rhs <- eval ex3 env
            pure $ LitBool (lhs <= rhs)
        And ex' ex3 -> do
            LitBool lhs <- eval ex' env
            if lhs then eval ex3 env else pure $ LitBool False
        Or ex' ex3 -> do
            LitBool lhs <- eval ex' env
            if lhs then pure $ LitBool True else eval ex3 env
        Equals ex' ex3 -> do
            lhs <- eval ex' env
            rhs <- eval ex3 env
            pure $ LitBool (lhs == rhs)
        Concat ex' ex3 -> do
            LitString lhs <- eval ex' env
            LitString rhs <- eval ex3 env
            pure $ LitString (lhs ++ rhs)
        Seq ex' ex3 -> eval ex' env *> eval ex3 env
        Not ex'     -> do
            LitBool b <- eval ex' env
            pure $ LitBool (not b)
        Neg ex' -> do
            LitInt i <- eval ex' env
            pure $ LitInt (negate i)
        Call idx exs -> if isPrimitive (nameIdx idx)
            then evalPrimitive (nameIdx idx) =<< mapM (`eval` env) exs
            else case Map.lookup idx ft of
                Just (FunDef _ _ params _ body) ->
                    eval body
                        =<< Map.union
                        <$> (   Map.fromList
                            <$> zipWithM
                                    (\(ParamDef n _) ex -> do
                                        e' <- eval ex env
                                        pure (n, e')
                                    )
                                    params
                                    exs
                            )
                        <*> pure env
                _ -> evalError $ "Unknown function " ++ nameIdx idx
        ConstrCall idx p@(EnumType ty _) exs ->
            case Map.lookup ty (constructors st) >>= Map.lookup idx of
                Just (ConstrSig _ params _) ->
                    ConstrCall idx p <$> mapM (`eval` env) exs
                _ -> evalError $ "Unknown constructor " ++ nameIdx idx
        Let pd ex' ex3 -> do
            bd <- eval ex' env
            eval ex3 $ Map.insert (paramName pd) bd env
        IfElse ex' ex3 ex4 -> do
            LitBool p <- eval ex' env
            if p then eval ex3 env else eval ex4 env
        Match ex' mcs -> handleCases mcs env =<< eval ex' env
        Bottom ex'    -> do
            LitString s <- eval ex' env
            evalError s
        _ -> evalError "Unreachable"
      where
        handleCases [] _ _ = evalError "Match case not exclusive"
        handleCases (MatchCase pat body : msc) env ex =
            case handlePattern pat ex of
                Just bnd -> eval body (Map.union (Map.fromList bnd) env)
                _        -> handleCases msc env ex

        handlePattern :: Pattern Idx -> Expr Idx -> Maybe [(Idx, Expr Idx)]
        handlePattern pat ex = case pat of
            WildcardPattern        -> Just []
            IdPattern      idx     -> Just [(idx, ex)]
            LiteralPattern ex'     -> if ex == ex' then Just [] else Nothing
            EnumPattern idx _ pats -> case ex of
                ConstrCall idx' _ exs' | idx == idx' ->
                    concat <$> zipWithM handlePattern pats exs'
                _ -> Nothing

evalPrimitive :: String -> [Expr Idx] -> IO (Expr Idx)
evalPrimitive "print"    [LitString s] = putStr s *> hFlush stdout $> LitUnit
evalPrimitive "println"  [LitString s] = putStrLn s *> hFlush stdout $> LitUnit
evalPrimitive "readLine" []            = LitString <$> getLine
evalPrimitive "toInt"    [LitString s] = case readMaybe s :: Maybe Integer of
    Nothing -> evalError $ "Cannot parse Int: " ++ s
    Just i  -> pure $ LitInt i
evalPrimitive "toString" [s] = pure $ LitString $ removeQuot s
evalPrimitive f          _   = evalError $ "Unknown primitive function " ++ f
