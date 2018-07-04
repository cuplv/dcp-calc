module Parser (
  parseExpr
  ) where

import Syntax

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Data.Functor.Identity

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart = "{-"
  , Tok.commentEnd   = "-}"
  , Tok.commentLine  = "--"
  , Tok.nestedComments = True
  , Tok.identStart = letter
  , Tok.identLetter = alphaNum <|> oneOf "_'?"
  , Tok.opStart = oneOf ":!#$%&*+.?<=>?@\\^|-~"
  , Tok.opLetter = oneOf ":!#$%&*+.?<=>?@\\^|-~"
  , Tok.reservedNames = []
  , Tok.reservedOpNames = []
  , Tok.caseSensitive = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

prefixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
prefixOp s f = Ex.Prefix (reservedOp s >> return f)

binaryOp :: String -> (a -> a -> a) -> Ex.Assoc -> Ex.Operator String () Identity a
binaryOp s f = Ex.Infix (reservedOp s >> return f)

-- Operators
table :: [[Ex.Operator String () Identity Expr]]
table = [ [binaryOp "*" ETimes Ex.AssocLeft, binaryOp "/" EDivide Ex.AssocLeft]
        , [binaryOp "%" EMod Ex.AssocLeft]
        , [binaryOp "+" EPlus Ex.AssocLeft, binaryOp "-" EMinus Ex.AssocLeft]
        , [prefixOp "not" ENot]
        , [binaryOp "<" ELt Ex.AssocNone
          , binaryOp ">" EGt Ex.AssocNone
          , binaryOp "<=" ELeq Ex.AssocNone
          , binaryOp ">=" EGeq Ex.AssocNone
          , binaryOp "==" EEq Ex.AssocNone
          , binaryOp "<>" ENeq Ex.AssocNone
          ]
        , [binaryOp "&&" EAnd Ex.AssocLeft]
        , [binaryOp "||" EOr Ex.AssocLeft]
        ]

-- Conditional
ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reservedOp "then"
  trueBranch <- expr
  reservedOp "else"
  falseBranch <- expr
  return (EIf cond trueBranch falseBranch)

-- Constants
true, false, unit :: Parser Expr
true = reserved "true" >> return ETrue
false = reserved "false" >> return EFalse
unit = reserved "()" >> return EUnit

-- Integer
int :: Parser Expr
int = do
  n <- Tok.integer lexer
  return (EInt n)

-- Integer
str :: Parser Expr
str = do
  s <- Tok.stringLiteral lexer
  return (EString s)

-- List
list :: Parser Expr
list = do
  xs <- commaSep expr
  return (EList xs)

-- Pair
pair :: Parser Expr
pair = do
  fst <- expr
  char ','
  snd <- expr
  return (EPair fst snd)
  
expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

factor :: Parser Expr
factor =
      true
  <|> false
  <|> unit
  <|> int
  <|> int
  <|> str
  <|> parens pair
  <|> parens expr
  <|> brackets list

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = semiSep expr

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s
