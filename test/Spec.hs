import           Control.Monad
import qualified Data.Map                      as Map
import           NameAnalysis
import           Parser
import           System.Exit                    ( exitFailure )
import           System.IO
import           TypeChecker

main :: IO ()
main = do
    handle   <- openFile "examples/Hello.scala" ReadMode
    contents <- hGetContents handle
    case parseProgram contents of
        Left  pe  -> error $ show pe
        Right des -> case analyze des of
            Right (st, pg) -> do
                print st
                putStrLn "\n<Program>\n"
                mapM_ print pg
                putStrLn "\n<Check types>\n"
                print $ checkEnum pg st
                putStrLn "\n<Constraints>\n"
                mapM_ (\x -> mapM_ print x *> putStrLn "--------")
                    $ testCons pg st
            Left msg -> hPutStrLn stderr msg *> exitFailure

    hClose handle
