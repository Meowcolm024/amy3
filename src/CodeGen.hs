{-# LANGUAGE OverloadedStrings #-}
module CodeGen
    ( codeGen
    ) where

import qualified Data.Map                      as Map
import qualified Data.Text                     as T
import           Js                             ( preJs )
import           SymbolTable
import           Types

data Pack = Pack
    { insRet :: Bool
    , env    :: Map.Map Idx (Expr Idx)
    }
    deriving Show

-- | generate JavaScript
--   opt: enable optimization
codeGen :: Bool -> Program Idx -> SymbolTable -> FuncTable -> T.Text
codeGen opt pg st ft = preJs <> T.concat (map (genDef opt st ft) pg)

genDef :: Bool -> SymbolTable -> FuncTable -> Definition Idx -> T.Text
genDef = error "not implemented"

cgExpr :: Pack -> Expr Idx -> [T.Text]
cgExpr = undefined
