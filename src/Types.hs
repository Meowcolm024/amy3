{-# LANGUAGE DeriveFunctor #-}
module Types where

import           Data.List                      ( intercalate )
import           GHC.OldList                    ( intercalate )

-- a program is a collection of definitions
type Program a = [Definition a]

data AType a = IntType
             | BooleanType
             | StringType
             | UnitType
             | EnumType a [AType a]     -- ^ typename, type parameters
             | TypeParam a              -- ^ type parameter
             | Counted Int              -- ^ used only in constrint generation
             | AnyType                  -- ^ used only after type erasure
             deriving (Functor, Eq)

-- | definitions
data Definition a =
    -- | enum definition
    EnumDef {
        typeName :: a,
        typeArgs :: [AType a],
        caseEnums :: [CaseDef a]
    } |
    -- | function definition
    FunDef {
        funName :: a,
        funTypeArgs :: [AType a],
        argTypes :: [ParamDef a],
        retType :: AType a,
        body :: Expr a
    } |
    -- | main function
    EntryPoint {
        mainFunc :: Definition a
    }
    deriving (Functor)

-- | type constructor definition
data CaseDef a = CaseDef
    { caseName :: a                 -- ^ constr name
    , proTypes :: [ParamDef a]      -- ^ argument types
    , parType  :: AType a           -- ^ true type 
    }
    deriving (Functor, Eq)

-- | paramter definition
data ParamDef a = ParamDef
    { paramName :: a
    , paramType :: AType a
    }
    deriving (Functor, Eq)

data Expr a
    = Variable a
    | LitInt Integer
    | LitBool Bool
    | LitString String
    | LitUnit
    | Plus (Expr a) (Expr a)
    | Minus (Expr a) (Expr a)
    | Mult (Expr a) (Expr a)
    | Div (Expr a) (Expr a)
    | Mod (Expr a) (Expr a)
    | LessThan (Expr a) (Expr a)
    | LessEqual (Expr a) (Expr a)
    | And (Expr a) (Expr a)
    | Or (Expr a) (Expr a)
    | Equals (Expr a) (Expr a)
    | Concat (Expr a) (Expr a)
    | Seq (Expr a) (Expr a)
    | Not (Expr a)
    | Neg (Expr a)
    | -- | fun/constr call
        Call a [Expr a]
    | -- | constrctor call
        ConstrCall a (AType a) [Expr a]
    | -- | let binding: val a: t = x ; e
        Let (ParamDef a) (Expr a) (Expr a)
    | -- | if then else
        IfElse (Expr a) (Expr a) (Expr a)
    | -- | pattern matching
        Match (Expr a) [MatchCase a]
    | -- | error expr
        Bottom (Expr a)
    deriving (Functor, Eq)

data MatchCase a = MatchCase (Pattern a) (Expr a)
    deriving (Functor, Eq)

-- | patterns
data Pattern a
    = WildcardPattern
    | IdPattern a
    | LiteralPattern (Expr a)
    | EnumPattern a (AType a) [Pattern a]   -- ^ constr name, type name, pattern
    deriving (Functor, Eq)

-- * "not" pretty printing

instance Show a => Show (AType a) where
    show IntType        = "Int"
    show BooleanType    = "Boolean"
    show StringType     = "String"
    show UnitType       = "Unit"
    show (EnumType t i) = show t ++ "[" ++ intercalate ", " (map show i) ++ "]"
    show (TypeParam a ) = "~" ++ show a
    show (Counted   i ) = "Counter " ++ show i
    show AnyType        = "Any"

instance Show a => Show (Definition a) where
    show (EnumDef n a c) =
        "enum "
            ++ show n
            ++ "["
            ++ intercalate ", " (map show a)
            ++ "] {\n  case "
            ++ intercalate "\n  case " (map show c)
            ++ "\n}"
    show (FunDef n a p r b) =
        "def "
            ++ show n
            ++ "["
            ++ intercalate ", " (map show a)
            ++ "]("
            ++ intercalate ", " (map show p)
            ++ "): "
            ++ show r
            ++ " = {\n"
            ++ show b
            ++ "\n}"
    show (EntryPoint f) = "[<main>]\n" ++ show f

instance Show a => Show (CaseDef a) where
    show (CaseDef n ts p) =
        show n ++ "(" ++ intercalate ", " (map show ts) ++ ") : " ++ show p

instance Show a => Show (ParamDef a) where
    show (ParamDef n t) = show n ++ ": " ++ show t

instance Show a => Show (Expr a) where
    show e = case e of
        Variable  a      -> show a
        LitInt    n      -> show n
        LitBool   b      -> show b
        LitString s      -> show s
        LitUnit          -> "()"
        Plus      ex ex' -> "(" ++ show ex ++ ") + (" ++ show ex' ++ ")"
        Minus     ex ex' -> "(" ++ show ex ++ ") - (" ++ show ex' ++ ")"
        Mult      ex ex' -> "(" ++ show ex ++ ") * (" ++ show ex' ++ ")"
        Div       ex ex' -> "(" ++ show ex ++ ") / (" ++ show ex' ++ ")"
        LessThan  ex ex' -> "(" ++ show ex ++ ") < (" ++ show ex' ++ ")"
        LessEqual ex ex' -> "(" ++ show ex ++ ") <= (" ++ show ex' ++ ")"
        And       ex ex' -> "(" ++ show ex ++ ") && (" ++ show ex' ++ ")"
        Or        ex ex' -> "(" ++ show ex ++ ") || (" ++ show ex' ++ ")"
        Equals    ex ex' -> "(" ++ show ex ++ ") == (" ++ show ex' ++ ")"
        Concat    ex ex' -> "(" ++ show ex ++ ") ++ (" ++ show ex' ++ ")"
        Seq       ex ex' -> show ex ++ ";\n" ++ show ex'
        Not ex           -> "!(" ++ show ex ++ ")"
        Neg ex           -> "-(" ++ show ex ++ ")"
        Call a exs -> show a ++ "(" ++ intercalate ", " (map show exs) ++ ")"
        ConstrCall a (EnumType n _) exs ->
            show n
                ++ "."
                ++ show a
                ++ "("
                ++ intercalate ", " (map show exs)
                ++ ")"
        Let pd ex ex' ->
            "val " ++ show pd ++ " = " ++ show ex ++ ";\n" ++ show ex'
        IfElse ex ex' ex_a ->
            "if ("
                ++ show ex
                ++ ") {\n"
                ++ show ex'
                ++ "\n} else {\n"
                ++ show ex_a
                ++ "\n}"
        Match ex mcs ->
            "(" ++ show ex ++ ") match {\n" ++ unlines (map show mcs) ++ "}"
        Bottom ex -> "error(" ++ show ex ++ ")"
        _         -> "???"

instance Show a => Show (MatchCase a) where
    show (MatchCase p e) = "case " ++ show p ++ " => " ++ show e

instance Show a => Show (Pattern a) where
    show pat = case pat of
        WildcardPattern   -> "_"
        IdPattern      a  -> show a
        LiteralPattern ex -> show ex
        EnumPattern a (EnumType n _) pats ->
            show n
                ++ "."
                ++ show a
                ++ "("
                ++ intercalate ", " (map show pats)
                ++ ")"
        _ -> "???"

-- | unique id for each names
-- replace the String in Expr after name analysis
data Idx = Idx
    { nameIdx :: String
    , idIdx   :: Int
    }

instance Show Idx where
    show (Idx n i) = n ++ "_" ++ show i

instance Eq Idx where
    (Idx _ i) == (Idx _ j) = i == j

instance Ord Idx where
    compare (Idx _ i) (Idx _ j) = compare i j
