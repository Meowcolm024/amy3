module Parser where

import           Data.Functor                   ( ($>) )
import           Data.Maybe                     ( fromMaybe )
import           Lexer
import           Text.Parsec
import qualified Text.Parsec.Token             as P
import           Text.ParserCombinators.Parsec  ( Parser )
import           Types

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p "amy3"

-- * definitions

-- | parse the whole program
--   main function must be defined at the end, and only one
program :: Parser [Definition String]
program =
    (++) <$> many (enumDef <|> funDef) <*> option [] (pure <$> entryPoint)

-- | main function
entryPoint :: Parser (Definition String)
entryPoint = EntryPoint <$> (reserved "@main" *> funDef)

-- | function def
funDef :: Parser (Definition String)
funDef = do
    reserved "def"
    f   <- identifier
    tas <- option [] $ brackets $ commaSep1 (TypeParam <$> identifier)
    fps <- option [] $ parens $ option [] $ commaSep1 paramDef
    colon
    ret <- parseType
    reservedOp "="
    body <- braces expr
    pure $ FunDef f tas fps ret body

-- | enum def
enumDef :: Parser (Definition String)
enumDef = do
    reserved "enum"
    ty  <- identifier
    tas <- option [] $ brackets $ commaSep1 (TypeParam <$> identifier)
    cs  <- braces . many1 $ caseDef (EnumType ty tas)
    pure $ EnumDef ty tas cs

caseDef :: AType String -> Parser (CaseDef String)
caseDef parent = do
    reserved "case"
    cs   <- identifier
    args <- parens $ option [] $ commaSep paramDef
    pure $ CaseDef cs args parent

-- | parse type variable
typeVars :: Parser [AType String]
typeVars = commaSep1 $ parseType <|> (TypeParam <$> identifier)

-- | parse type, primitive or enum
parseType :: Parser (AType String)
parseType = primitiveTypes <|> do
    ty <- identifier
    tp <- option [] $ brackets typeVars
    pure $ EnumType ty tp

-- | parameter def
paramDef :: Parser (ParamDef String)
paramDef = ParamDef <$> identifier <*> (colon *> parseType)

-- * expresions

-- | constructor call (qualified)
constrCall :: Parser (Expr String)
constrCall = do
    f <- identifier
    dot
    cst  <- identifier
    tys  <- option [] $ brackets typeVars
    args <- parens (option [] $ commaSep expr)
    pure $ ConstrCall cst (EnumType f tys) args

-- | function call
call :: Parser (Expr String)
call = do
    f    <- identifier
    tys  <- option [] $ brackets typeVars
    args <- parens (option [] $ commaSep expr)
    pure $ Call f tys args

-- | if then else
ifElse :: Parser (Expr String)
ifElse = do
    reserved "if"
    p <- parens expr
    x <- braces expr
    reserved "else"
    IfElse p x <$> braces expr

-- | let binding
letIn :: Parser (String, AType String, Expr String)
letIn = do
    reserved "val"
    (ParamDef n t) <- paramDef
    reservedOp "="
    v <- expr'
    pure (n, t, v)

-- | pattern matching
matches :: Parser [MatchCase String]
matches = reserved "match" *> braces (reserved "case" *> patterns)

patterns :: Parser [MatchCase String]
patterns = sepBy1 patCase (reserved "case")
  where
    patCase = do
        pat <- singlePattern
        reservedOp "=>"
        MatchCase pat <$> expr

singlePattern :: Parser (Pattern String)
singlePattern =
    wildcardPattern <|> literalPattern <|> try customPattern <|> idPattern
  where
    wildcardPattern = reserved "_" $> WildcardPattern
    literalPattern  = LiteralPattern <$> literals
    idPattern       = IdPattern <$> identifier
    customPattern   = do
        f <- identifier         -- type
        dot
        cst  <- identifier      -- constr
        -- ! pattern matched polymorphic type must be inferred
        -- tys  <- optionMaybe $ brackets typeVars
        pats <- parens (option [] $ commaSep1 singlePattern)
        pure $ EnumPattern cst (EnumType f [Unknown]) pats

-- | literals
literals :: Parser (Expr a)
literals = primitiveValues

-- | error type
bottom :: Parser (Expr String)
bottom = Bottom <$> (reserved "error" *> expr)

-- | variable
variable :: Parser (Expr String)
variable = Variable <$> identifier

-- | unary operator
uOps :: Parser (Expr String) -> Parser (Expr String)
uOps term =
    Not <$> (reservedOp "!" *> term) <|> Neg <$> (reservedOp "-" *> term)

-- | seq and let binding
seq' :: Parser (Expr String)
seq' = bind <|> se
  where
    bind = do
        bnd <- letIn
        res <- seqRest
        case res of
            Nothing -> parserFail "Error: let-binding without following expr"
            Just e  -> pure $ Let bnd e
    se = do
        ex  <- expr'
        res <- seqRest
        pure $ case res of
            Nothing -> ex
            Just e  -> Seq ex e

seqRest :: Parser (Maybe (Expr String))
seqRest = optionMaybe $ semi *> seq'

-- | expression
expr :: Parser (Expr String)
expr = seq'

-- | simple expr and matches
expr' :: Parser (Expr String)
expr' = do
    e  <- ifElse <|> term0
    ms <- many matches
    pure $ case ms of
        [] -> e
        xs -> foldl Match e xs

-- | binary operators
term0 = term1 `chainl1` (reservedOp "||" $> Or)
term1 = term2 `chainl1` (reservedOp "&&" $> And)
term2 = term3 `chainl1` (reservedOp "==" $> Equals)
term3 =
    term4
        `chainl1` (reservedOp "<" $> LessThan <|> reservedOp "<=" $> LessEqual)
term4 =
    term5
        `chainl1` (   reservedOp "+"
                  $>  Plus
                  <|> reservedOp "-"
                  $>  Minus
                  <|> reservedOp "++"
                  $>  Concat
                  )
term5 = term6 `chainl1` (reservedOp "*" $> Mult <|> reservedOp "/" $> Div)
term6 = uOps term7 <|> term7
term7 =
    (try (reserved "()") $> LitUnit)
        <|> parens expr
        <|> literals
        <|> try constrCall
        <|> try call
        <|> variable
