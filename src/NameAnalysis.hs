module NameAnalysis where

import qualified Data.Map                      as Map
import           Types

-- | the env
type Env = Map.Map String Idx

{-
    identifier/variable is counted separated in env
    variable, func param, type variable

    string -> idx

    we need to dynamically add type and functions to symbol table

    first add templates to symbol table
    then add types and functions to symbol table according their actual types

-}
