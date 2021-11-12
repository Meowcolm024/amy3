{-# LANGUAGE DeriveFunctor #-}
module Types where

import           Data.List                      ( intercalate )

-- a program is a collection of definitions
type Program a = [Definition a]

data AType a = IntType
             | BooleanType
             | StringType
             | UnitType
             | EnumType a [AType a]     -- ^ typename, type parameters
             | TypeParam a              -- ^ type parameter
             | Unknown                  -- ^ need to be infered (in pttern matching)
             deriving (Functor, Eq)

instance Show a => Show (AType a) where
    show IntType        = "Int"
    show BooleanType    = "Boolean"
    show StringType     = "String"
    show UnitType       = "Unit"
    show (EnumType t i) = show t ++ "[" ++ (i >>= show) ++ "]"
    show (TypeParam a ) = show a
    show Unknown        = "Unknown"

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

data CaseDef a = CaseDef
    { caseName :: a                 -- ^ constr name
    , proTypes :: [ParamDef a]      -- ^ argument types
    , parType  :: AType a           -- ^ true type 
    }
    deriving Functor

data ParamDef a = ParamDef
    { paramName :: a
    , paramType :: AType a
    }
    deriving Functor

instance Show a => Show (Definition a) where
    show (EnumDef n a c) = show n ++ if null a
        then ""
        else
            "["
            ++ intercalate ", " (map show a)
            ++ "] { case "
            ++ intercalate "case " (map show c)
            ++ "}"
    show (FunDef n a p r b) =
        "def "
            ++ show n
            ++ "["
            ++ intercalate ", " (map show a)
            ++ "]("
            ++ intercalate ", " (map show p)
            ++ "): "
            ++ show r
            ++ " = {"
            ++ show b
            ++ "}"
    show (EntryPoint f) = "[<main>]\n" ++ show f

instance Show a => Show (CaseDef a) where
    show (CaseDef n ts p) =
        show n ++ "(" ++ intercalate ", " (map show ts) ++ ") : " ++ show p

instance Show a => Show (ParamDef a) where
    show (ParamDef n t) = show n ++ ": " ++ show t


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
        Call a [AType a] [Expr a]
    | -- | constrctor call
        ConstrCall a (AType a) [Expr a]
    | -- | let binding: val a: t = x ; e
        Let (a, AType a, Expr a) (Expr a)
    | -- | if then else
        IfElse (Expr a) (Expr a) (Expr a)
    | -- | pattern matching
        Match (Expr a) [MatchCase a]
    | -- | error expr
        Bottom (Expr a)
    deriving (Show, Functor)

data MatchCase a = MatchCase (Pattern a) (Expr a)
    deriving (Show, Functor)

data Pattern a
    = WildcardPattern
    | IdPattern a
    | LiteralPattern (Expr a)
    | EnumPattern a (AType a) [Pattern a]   -- ^ constr name, type name, pattern
    deriving (Show, Functor)

-- | unique id for each names
data Idx = Idx
    { getName :: String
    , getId   :: Int
    }
    deriving Show
