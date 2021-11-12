import           NameAnalysis
import           Parser
import           SymbolTable
import           System.IO

main :: IO ()
main = do
    handle   <- openFile "examples/Hello.scala" ReadMode
    contents <- hGetContents handle
    case regularParse program contents of
        Left pe -> error $ show pe
        Right des ->
            let SymbolTable ts fs cs e = analysis des
            in  do
                    putStrLn "<Types>" *> mapM_ print ts
                    putStrLn "<Functions>" *> mapM_ print fs
                    putStrLn "<Constructors>" *> mapM_ print cs
                    putStrLn "<Main>" *> print e
    hClose handle
