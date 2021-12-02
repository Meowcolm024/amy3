module Utils where

import           System.Exit                    ( exitFailure )
import           System.IO
import           Types

isMainDef :: Definition a -> Bool
isMainDef EntryPoint{} = True
isMainDef _            = False

isEnumPat :: Pattern a -> Bool
isEnumPat EnumPattern{} = True
isEnumPat _             = False

evalError :: String -> IO a
evalError msg = hPutStrLn stderr ("[Error] " ++ msg) *> exitFailure
