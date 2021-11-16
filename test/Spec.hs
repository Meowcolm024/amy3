import           Control.Monad
import qualified Data.Map                      as Map
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
            let SymbolTable ts fs cs e = analyzeDef des
            putStrLn "<Types>" *> mapM_ print ts
            putStrLn "\n<Functions>" *> mapM_ print fs
            putStrLn "\n<Constructors>"
            forM_ (Map.toList cs) $ \(k, v) -> do
                putStrLn $ k ++ " -> " ++ show v
            putStrLn "\n<Main>" *> print e
    hClose handle
