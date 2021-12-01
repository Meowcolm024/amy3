module Main where

import           Interpreter                    ( execMain, evalError )
import           NameAnalysis                   ( analyze )
import           Parser                         ( parseProgram )
import           SymbolTable                    ( buildFuncTable )
import           System.IO
import           TypeChecker                    ( checkType )

main :: IO ()
main = interpretMode "examples/Hello.scala"

interpretMode :: String -> IO ()
interpretMode filename = do
    handle   <- openFile filename ReadMode
    contents <- hGetContents handle
    case parseProgram contents of
        Left  pe  -> evalError (show pe) 
        Right des -> case analyze des of
            Left  s        -> evalError (show s)
            Right (st, pg) -> case checkType pg st of
                Left  s -> evalError (show s)
                Right _ -> execMain pg st (buildFuncTable pg)
    hClose handle

