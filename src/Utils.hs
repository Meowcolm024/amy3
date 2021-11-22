module Utils where

import           Types

isMainDef :: Definition a -> Bool
isMainDef (EntryPoint _) = True
isMainDef _              = False
