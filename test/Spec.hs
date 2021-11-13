import           NameAnalysis
import           Parser
import           SymbolTable
import           System.IO

main :: IO ()
main = do
    handle   <- openFile "examples/Hello.scala" ReadMode
    contents <- hGetContents handle
    case regularParse program contents of
        Left  pe  -> error $ show pe
        Right des -> do
            let (p, t) = analyzeProgram des
            let SymbolTable ts fs cs e = t
            putStrLn "<Types>" *> mapM_ print ts
            putStrLn "\n<Functions>" *> mapM_ print fs
            putStrLn "\n<Constructors>" *> mapM_ print cs
            putStrLn "\n<Main>" *> print e
            putStrLn "\n --- \n" *> print p
    hClose handle
