{-# LANGUAGE OverloadedStrings #-}
module CodeGen
    ( codeGen
    ) where

import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromJust )
import qualified Data.Text                     as T
import           Js                             ( preJs )
import           Optimizer                      ( optimize )
import           SymbolTable
import           Types
import           Utils                          ( isMainDef )

data Pack = Pack
    { insRet :: Bool
    , env    :: Map.Map Idx T.Text
    }
    deriving Show

newPack :: Map.Map Idx T.Text -> Pack
newPack = Pack True

-- | generate JavaScript for the program
--   opt: enable optimization
codeGen :: Bool -> Program Idx -> SymbolTable -> FuncTable -> T.Text
codeGen opt pg st ft =
    preJs <> T.unlines (map (genDef opt st ft) pg) <> T.pack runEntry
  where
    runEntry = case filter isMainDef pg of
        (EntryPoint de) : _ -> "\n" <> nameIdx (funName de) <> "()"
        _                   -> ""

-- | generate one definitation
genDef :: Bool -> SymbolTable -> FuncTable -> Definition Idx -> T.Text
genDef opt st ft (EntryPoint def) = genDef opt st ft def
genDef opt st ft (FunDef name _ params _ body) =
    "function "
        <> T.pack (nameIdx name)
        <> "("
        <> T.intercalate
               ","
               (map (\(ParamDef n _) -> T.pack $ nameIdx n) params)
        <> "){"
        <> cgExpr st ft params (optimize opt body)
        <> "}"
genDef _ _ _ _ = ""

-- | convert AST to JavaScript
cgExpr :: SymbolTable -> FuncTable -> [ParamDef Idx] -> Expr Idx -> T.Text
cgExpr st ft params = cg (newPack mkEnv)
  where
    mkEnv = Map.fromList
        $ map (\(ParamDef name _) -> (name, T.pack $ nameIdx name)) params
    mvRet p = p { insRet = False }
    cgRet p = if insRet p then "return " else ""
    cg :: Pack -> Expr Idx -> T.Text
    cg p expr = case expr of
        Variable  idx -> cgRet p <> T.pack (show idx)
        LitInt    n   -> cgRet p <> T.pack (show n)
        LitBool   b   -> cgRet p <> if b then "true" else "false"
        LitString s   -> cgRet p <> T.pack (show s)
        LitUnit       -> cgRet p <> "null"
        Plus ex ex' ->
            cgRet p
                <> "("
                <> cg (mvRet p) ex
                <> ")+("
                <> cg (mvRet p) ex'
                <> ")"
        Minus ex ex' ->
            cgRet p
                <> "("
                <> cg (mvRet p) ex
                <> ")-("
                <> cg (mvRet p) ex'
                <> ")"
        Mult ex ex' ->
            cgRet p
                <> "("
                <> cg (mvRet p) ex
                <> ")*("
                <> cg (mvRet p) ex'
                <> ")"
        Div ex ex' ->
            cgRet p
                <> "Math.floor(("
                <> cg (mvRet p) ex
                <> ")/("
                <> cg (mvRet p) ex'
                <> "))"
        Mod ex ex' ->
            cgRet p
                <> "("
                <> cg (mvRet p) ex
                <> ")%("
                <> cg (mvRet p) ex'
                <> ")"
        LessThan ex ex' ->
            cgRet p
                <> "("
                <> cg (mvRet p) ex
                <> ")<("
                <> cg (mvRet p) ex'
                <> ")"
        LessEqual ex ex' ->
            cgRet p
                <> "("
                <> cg (mvRet p) ex
                <> ")<=("
                <> cg (mvRet p) ex'
                <> ")"
        And ex ex' ->
            cgRet p
                <> "("
                <> cg (mvRet p) ex
                <> ")&&("
                <> cg (mvRet p) ex'
                <> ")"
        Or ex ex' ->
            cgRet p
                <> "("
                <> cg (mvRet p) ex
                <> ")||("
                <> cg (mvRet p) ex'
                <> ")"
        Equals ex ex' ->
            cgRet p
                <> "("
                <> cg (mvRet p) ex
                <> ")==("
                <> cg (mvRet p) ex'
                <> ")"
        Concat ex ex' ->
            cgRet p
                <> "("
                <> cg (mvRet p) ex
                <> ")+("
                <> cg (mvRet p) ex'
                <> ")"
        Seq ex ex' -> cg (mvRet p) ex <> ";\n" <> cg p ex'
        Not ex     -> cgRet p <> "!(" <> cg (mvRet p) ex <> ")"
        Neg ex     -> cgRet p <> "-(" <> cg (mvRet p) ex <> ")"
        Call idx exs ->
            cgRet p
                <> T.pack (nameIdx idx)
                <> "("
                <> T.intercalate "," (map (cg (mvRet p)) exs)
                <> ")"
        ConstrCall idx (EnumType ty _) exs ->
            case Map.lookup ty (constructors st) >>= Map.lookup idx of
                Just sig -> cgRet p <> mkConstr idx (mvRet p) sig exs
                Nothing  -> error "???"
        Let (ParamDef n _) ex ex' ->
            "let "
                <> T.pack (show n)
                <> " = "
                <> cg (mvRet p) ex
                <> ";\n"
                <> cg p ex'
        IfElse ex ex' ex3 ->
            cgRet p
                <> "(() => {"
                <> cg p { insRet = True } ex
                <> "})() ?\n (() => {"
                <> cg p { insRet = True } ex'
                <> "})() :\n (() => {"
                <> cg p { insRet = True } ex3
                <> "})()\n"
        Match ex mcs ->
            cgRet p
                <> "((__match__) => {"
                <> handleCases mcs (p { insRet = True }) "__match__"
                <> "})("
                <> cg (mvRet p) ex
                <> ")\n"
        Bottom ex -> "error(" <> cg (mvRet p) ex <> ")"
        _         -> error "???"

    mkConstr :: Idx -> Pack -> Signature Idx -> [Expr Idx] -> T.Text
    mkConstr name p ~(ConstrSig _ params (EnumType ty _)) exs =
        "{"
            <> T.intercalate
                   ","
                   ([ty', name'] ++ zipWith ps1 [0 ..] (map (cg p) exs))
            <> "}"
      where
        ty'   = "\"type\":\"" <> T.pack (nameIdx ty) <> "\""
        name' = "\"constr\":\"" <> T.pack (nameIdx name) <> "\""
        ps1 i v = "\"_" <> T.pack (show i) <> "\":" <> v
    handleCases :: [MatchCase Idx] -> Pack -> T.Text -> T.Text
    handleCases [] _ _ = "error(\"Match case not exclusive\")"
    handleCases (MatchCase pat body : mcs) p expr =
        gen (matchAndBind pat expr) (cg p body) <> handleCases mcs p expr
      where
        gen :: ([T.Text], [T.Text]) -> T.Text -> T.Text
        gen (cnds, bnds) body =
            T.concat (map (\c -> "if" <> c <> "{") cnds)
                <> T.concat bnds
                <> body
                <> T.replicate (length cnds) "}"
        matchAndBind :: Pattern Idx -> T.Text -> ([T.Text], [T.Text])
        matchAndBind pat ex = case pat of
            WildcardPattern -> (["(true)"], [])
            IdPattern idx ->
                ( ["(true)"]
                , ["let " <> T.pack (nameIdx idx) <> "=" <> ex <> ";"]
                )
            LiteralPattern ex' ->
                (["(" <> cg (mvRet p) ex' <> "==" <> ex <> ")"], [])
            EnumPattern idx _ pats ->
                let (cnds, bnds) = unzip $ zipWith
                        (\p m -> matchAndBind p (ex <> "._" <> T.pack (show m)))
                        pats
                        [0 ..]
                in
                    ( [ "("
                        <> ex
                        <> ".constr==\""
                        <> T.pack (nameIdx idx)
                        <> "\")"
                      ]
                        <> concat cnds
                    , concat bnds
                    )
