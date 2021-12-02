module Pipe where

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

interpretMode :: [String] -> IO ()
interpretMode filenames = do
    rawText <- T.concat <$> mapM TIO.readFile filenames
    let contents = T.unpack rawText
    case checkProgram contents of
        Left  s        -> evalError s
        Right (st, pg) -> execMain pg st (buildFuncTable pg)

checkProgram :: String -> Either String (SymbolTable, Program Idx)
checkProgram p = do
    des      <- first show $ parseProgram p
    (st, pg) <- analyze des
    _        <- checkType pg st
    pure (st, pg)
