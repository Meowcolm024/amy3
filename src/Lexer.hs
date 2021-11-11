module Lexer where

import           Data.Functor                   ( ($>) )
import           Text.Parsec
import qualified Text.Parsec.Token             as P
import           Text.ParserCombinators.Parsec  ( Parser )
import           Types

langDef :: P.LanguageDef a
langDef = P.LanguageDef
    { P.commentStart    = "/*"
    , P.commentEnd      = "*/"
    , P.commentLine     = "//"      -- it is the case in FSharp
    , P.nestedComments  = False
    , P.identStart      = letter
    , P.identLetter     = alphaNum
    , P.opStart         = oneOf "+-*/=:<>|&!"
    , P.opLetter        = oneOf "+-*/=:<>|&!"
    , P.reservedNames   = [ "enum"
                          , "case"
                          , "val"
                          , "def"
                          , "match"
                          , "if"
                          , "else"
                          , "true"
                          , "false"
                          , "Boolean"
                          , "Int"
                          , "String"
                          , "Unit"
                          , "error"
                          , "_"     -- wild card
                          , "()"    -- unit type
                          , "@main"
                          ]
    , P.reservedOpNames = [ "=>"    -- case _ =>
                          , "++"    -- string concat
                          , "||"    -- or
                          , "&&"    -- and
                          , "+"     -- add
                          , "-"     -- minus / neg
                          , "*"     -- mult
                          , "/"     -- div
                          , "=="    -- equal
                          , "="     -- binding
                          , "<"     -- lt
                          , "<="    -- le
                          ]
    , P.caseSensitive   = True
    }

lexer = P.makeTokenParser langDef

-- | parse ()
parens :: Parser a -> Parser a
parens = P.parens lexer

-- | parse []
brackets :: Parser a -> Parser a
brackets = P.brackets lexer

-- | parse {}
braces :: Parser a -> Parser a
braces = P.braces lexer

dot :: Parser String
dot = P.dot lexer

semi :: Parser String
semi = P.semi lexer

colon :: Parser String
colon = P.colon lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = P.commaSep1 lexer

commaSep :: Parser a -> Parser [a]
commaSep = P.commaSep1 lexer

semiSep1 :: Parser a -> Parser [a]
semiSep1 = P.semiSep1 lexer

identifier :: Parser String
identifier = P.identifier lexer

reserved :: String -> Parser ()
reserved = P.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

primitiveTypes :: Parser (AType a)
primitiveTypes =
    reserved "Int"
        $>  IntType
        <|> reserved "Boolean"
        $>  BooleanType
        <|> reserved "String"
        $>  StringType
        <|> reserved "Unit"
        $>  UnitType


primitiveValues :: Parser (Expr a)
primitiveValues =
    reserved "true"
        $>  LitBool True
        <|> reserved "false"
        $>  LitBool False
        <|> LitInt
        <$> P.natural lexer
        <|> LitString
        <$> P.stringLiteral lexer
