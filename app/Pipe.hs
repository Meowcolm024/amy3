module Pipe(codeGenMode, interpretMode) where

import           Data.Bifunctor                 ( first )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Interpreter                    ( execMain )
import           NameAnalysis                   ( analyze )
import           Parser                         ( parseProgram )
import           SymbolTable                    ( SymbolTable(SymbolTable)
                                                , buildFuncTable
                                                )
import           TypeChecker                    ( checkType )
import           Types                          ( Idx(Idx)
                                                , Program
                                                )
import           Utils                          ( evalError )
import CodeGen

interpretMode :: [String] -> IO ()
interpretMode filenames = do
    rawText <- mapM TIO.readFile filenames
    case checkProgram rawText of
        Left  s        -> evalError s
        Right (st, pg) -> execMain pg st (buildFuncTable pg)

checkProgram :: [T.Text] -> Either String (SymbolTable, Program Idx)
checkProgram p = do
    des      <- concat <$> first show (traverse (parseProgram . T.unpack) p)
    (st, pg) <- analyze des
    _        <- checkType pg st
    pure (st, pg)

codeGenMode :: Bool -> [String] -> IO T.Text
codeGenMode opt filenames = do
    rawText <- mapM TIO.readFile filenames
    case checkProgram rawText of
        Left  s        -> evalError s
        Right (st, pg) -> pure $ codeGen opt pg st (buildFuncTable pg)
