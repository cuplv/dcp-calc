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
  , Tok.reservedNames = [ "let"
                        , "in"
                        , "lam"
                        , "rd"
                        , "wr"
                        , "nu"
                        , "thunk"
                        , "force"
                        ]
  , Tok.reservedOpNames = []
  , Tok.caseSensitive = True
  }

-- | Wrapper functions
lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

identifier :: Parser Name
identifier = Tok.identifier lexer

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

commaSep1 :: Parser a -> Parser [a]
commaSep1 = Tok.commaSep1 lexer

(<:>) :: (Applicative f) => f a -> f [a] -> f [a]
(<:>) a b = (:) <$> a <*> b

commaSep2 :: Parser a -> Parser [a]
commaSep2 p = (p <* char ',') <:> commaSep1 p

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

prefixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
prefixOp s f = Ex.Prefix (reservedOp s >> return f)

binaryOp :: String -> (a -> a -> a) -> Ex.Assoc -> Ex.Operator String () Identity a
binaryOp s f = Ex.Infix (reservedOp s >> return f)

-- | EVar
evar :: Parser Expr
evar = do
  x <- identifier
  return (EVar x)

-- | EChan

-- | EImpChan

-- | EInt
eint :: Parser Expr
eint = do
  n <- Tok.integer lexer
  return (EInt n)

-- | EBool, EUnit

etrue = reserved "true" >> return (EBool True)
efalse = reserved "false" >> return (EBool False)

ebool = etrue <|> efalse
eunit = reserved "()" >> return EUnit

-- | EString
estring :: Parser Expr
estring = do
  s <- Tok.stringLiteral lexer
  return (EString s)

-- | ETag  

-- | EList
elist :: Parser Expr
elist = do
  xs <- brackets $ commaSep expr
  return (EList xs)

-- | ETuple
etuple :: Parser Expr
etuple = do
  xs <- parens $ commaSep2 expr
  return (ETuple xs)
  
-- | Arithmetic operators, logical operators
ops :: [[Ex.Operator String () Identity Expr]]
ops = [ [binaryOp "*" ETimes Ex.AssocLeft, binaryOp "/" EDivide Ex.AssocLeft]
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
        , [binaryOp ";" ESeq Ex.AssocLeft]
        ]

-- | EIf
eif :: Parser Expr
eif = do
  reserved "if"
  cond <- expr
  reservedOp "then"
  trueBranch <- expr
  reservedOp "else"
  falseBranch <- expr
  return (EIf cond trueBranch falseBranch)

-- | EMatch

-- | Pattern

pvar = do
  x <- identifier
  return (PVar x)

pint = do
  n <- Tok.integer lexer
  return (PInt n)

ptrue = reserved "true" >> return (PBool True)
pfalse = reserved "false" >> return (PBool False)
pbool = ptrue <|> pfalse
punit = reserved "()" >> return PUnit
pwildcard = reserved "_" >> return PWildcard

pstring :: Parser Pattern
pstring = do
  s <- Tok.stringLiteral lexer
  return (PString s)

-- | PTag  

-- | PList
plist :: Parser Pattern
plist = do
  ps <- brackets $ commaSep pat
  return (PList ps)

-- | PTuple
ptuple :: Parser Pattern
ptuple = do
  ps <- parens $ commaSep2 pat
  return (PTuple ps)

-- | PSet


-- | PCons

pat =
      pvar
  <|> pint
  <|> pbool
  <|> pstring
  <|> plist
  <|> try ptuple
  <|> punit
  <|> pwildcard

-- | ELet
elet :: Parser Expr
elet = do
  reserved "let"
  p <- pat
  reserved "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return (ELet p e1 e2)

-- | ELetRec
-- | EAssign
-- | ERef
-- | EDeref
-- | ELam
elam :: Parser Expr
elam = do
  reserved "lam"
  x <- eatom
  reserved "."
  e <- expr
  return (ELam x e)

eatom =
      eint
  <|> ebool
  <|> estring
  <|> elist
  <|> try etuple
  <|> eunit
  <|> evar
  <|> parens expr


-- |  EApp
-- TODO
eapp :: Parser Expr
eapp = do
  f <- eatom
  x <- eatom
  return (EApp f x)

-- | ERd
erd = do
  reserved "rd"
  c <- expr
  return (ERd c)

-- | EWr
ewr = do
  reserved "wr"
  e <- expr
  reserved "->"
  c <- expr
  return (EWr e c)

-- | ENu
enu = do
  reserved "nu"
  c <- expr
  reserved "."
  e <- expr
  return (ENu c e)

-- | ERepl
erepl = do
  reserved "!"
  e <- eatom
  return (ERepl e)

-- | EFork
efork = do
  reserved "|>"
  e <- eatom
  return (EFork e)

pic =
      erd
  <|> ewr
  <|> enu
  <|> erepl
  <|> efork

ethunk = do
  reserved "thunk"
  e <- eatom
  return (EThunk e)

eforce = do
  reserved "force"
  e <- eatom
  return (EForce e)
  
expr :: Parser Expr
expr = Ex.buildExpressionParser ops factor

appexpr =
      eapp
  <|> ethunk
  <|> eforce

factor :: Parser Expr
factor =
      try eapp
  <|> eatom
  <|> elet
  <|> eif
  <|> elam
  <|> eforce
  <|> ethunk
  <|> pic


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
