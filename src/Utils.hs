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

removeQuot :: (Show (f String), Functor f) => f Idx -> String
removeQuot expr = filter (/= '"') . show $ nameIdx <$> expr

isLit :: Expr a -> Bool
isLit (LitBool   _) = True
isLit (LitInt    _) = True
isLit (LitString _) = True
isLit LitUnit       = True
isLit _             = False
