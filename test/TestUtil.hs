module TestUtil where

import           CodeGen                        ( codeGen )
import           Data.Bifunctor                 ( first )
import qualified Data.Map                      as Map
import           Data.Text                      ( unpack )
import           Interpreter
import           NameAnalysis                   ( analyze )
import           Optimizer                      ( optimize )
import           Parser                         ( parseProgram )
import           SymbolTable
import           System.IO
import           TypeChecker
import           Types
import           Utils                          ( removeQuot )

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

runInterpret :: Bool -> String -> String -> IO (Either String (Expr Idx))
runInterpret opt fun f = do
    case loadProgram f of
        Left  s             -> pure $ Left s
        Right (pgs, st, ft) -> case getTest fun pgs of
            Left  s  -> pure $ Left s
            Right pg -> Right <$> ri pg st ft
    where ri ~(FunDef _ _ _ _ body) = interpret (optimize opt body) Map.empty

runCodeGen :: String -> String
runCodeGen file = do
    case loadProgram file of
        Left  s             -> s
        Right (pgs, st, ft) -> unpack $ codeGen False pgs st ft

printExpr :: Expr Idx -> String
printExpr = removeQuot
