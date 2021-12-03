{-# LANGUAGE OverloadedStrings #-}
module CodeGen
    ( codeGen
    ) where

import qualified Data.Map                      as Map
import qualified Data.Text                     as T
import           Js                             ( preJs )
import           SymbolTable
import           Types
import           Utils                          ( isMainDef )

data Pack = Pack
    { insRet :: Bool
    , env    :: Map.Map Idx (Expr Idx)
    }
    deriving Show

-- | generate JavaScript for the program
--   opt: enable optimization
codeGen :: Bool -> Program Idx -> SymbolTable -> FuncTable -> T.Text
codeGen opt pg st ft =
    preJs <> T.concat (map (genDef opt st ft) pg) <> T.pack runEntry
  where
    runEntry = case filter isMainDef pg of
        (EntryPoint de) : _ -> "\n" <> nameIdx (funName de) <> "()"
        _                   -> ""

-- | generate one definitation
genDef :: Bool -> SymbolTable -> FuncTable -> Definition Idx -> T.Text
genDef opt st ft (EntryPoint def             ) = genDef opt st ft def
genDef opt st ft (FunDef name _ params _ body) = error "not implemented"
genDef _   _  _  _                             = undefined

-- | convert AST to JavaScript
cgExpr :: SymbolTable -> FuncTable -> Expr Idx -> [T.Text]
cgExpr = undefined
