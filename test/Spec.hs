import           Control.Monad
import qualified Data.Map                      as Map
import           NameAnalysis
import           Parser
import           System.IO

main :: IO ()
main = do
    handle   <- openFile "examples/Hello.scala" ReadMode
    contents <- hGetContents handle
    case regularParse program contents of
        Left  pe  -> error $ show pe
        Right des -> case analyze des of
            Right (st, pg) -> do
                print st
                mapM_ print pg
            Left msg -> fail msg

    hClose handle
