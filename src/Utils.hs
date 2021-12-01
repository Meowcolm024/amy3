module Utils where

import           Types

isMainDef :: Definition a -> Bool
isMainDef EntryPoint{} = True
isMainDef _            = False

isEnumPat :: Pattern a -> Bool
isEnumPat EnumPattern{} = True
isEnumPat _             = False
