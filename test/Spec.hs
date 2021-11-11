import Parser
import           System.IO

main :: IO ()
main = do
        handle   <- openFile "examples/Hello.scala" ReadMode
        contents <- hGetContents handle
        case regularParse program contents of
            Left  pe  -> error $ show pe
            Right des -> mapM_ print des
        hClose handle
