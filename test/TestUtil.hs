module TestUtil where

import           Data.Bifunctor                 ( first )
import qualified Data.Map                      as Map
import           Interpreter
import           NameAnalysis                   ( analyze )
import           Parser                         ( parseProgram )
import           SymbolTable
import           System.IO
import           TypeChecker
import           Types
import Utils (removeQuot)

loadProgram :: String -> Either String (Program Idx, SymbolTable, FuncTable)
loadProgram p = do
    des      <- first show (parseProgram p)
    (st, pg) <- analyze des
    _        <- checkType pg st
    pure (pg, st, buildFuncTable pg)

printAll :: FilePath -> IO ()
printAll p = do
    handle   <- openFile p ReadMode
    contents <- hGetContents handle
    case parseProgram contents of
        Left  pe  -> print pe
        Right des -> case analyze des of
            Left  msg      -> print msg
            Right (st, pg) -> do
                print st
                putStrLn "\n<Program>\n"
                mapM_ print pg
                putStrLn "\n<Check types>\n"
                let tcs = runConstraint pg st
                putStrLn "\n<Constraints>\n"
                mapM_ (\x -> mapM_ print x *> putStrLn "--------") tcs
                putStrLn "\n<Solved>\n"
                print $ checkType pg st
    hClose handle

getTest :: String -> Program Idx -> Either String (Definition Idx)
getTest _ []            = Left "Test function not found!"
getTest name (f@(FunDef (Idx name' _) _ _ _ _) : rest) | name == name' = Right f
getTest name (_ : rest) = getTest name rest

runInterpret :: String -> String -> IO (Either String (Expr Idx))
runInterpret fun f = do
    case loadProgram f of
        Left  s             -> pure $ Left s
        Right (pgs, st, ft) -> case getTest fun pgs of
            Left  s  -> pure $ Left s
            Right pg -> Right <$> ri pg st ft
    where ri ~(FunDef _ _ _ _ body) = interpret body Map.empty

printExpr :: Expr Idx -> String
printExpr = removeQuot
