module Parser
    (
      parser
    ) where

import           Data.Functor.Identity

import           Text.Parsec
import qualified Text.Parsec.Expr      as Ex
import           Text.Parsec.Language  (emptyDef)
import           Text.Parsec.String    (Parser)
import qualified Text.Parsec.Token     as Tok

import           Syntax

-- | Lexer
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
                        , "not"
                        , "if"
                        , "then"
                        , "else"
                        , "true"
                        , "false"
                        ]
  , Tok.reservedOpNames = [ "+"
                          , "-"
                          , "*"
                          , "/"
                          , "%"
                          , "&&"
                          , "||"
                          , "<"
                          , ">"
                          , "<="
                          , ">="
                          , "=="
                          , "<>"
                          , "!"
                          , "|>"
                          , ";"
                          , ";;"
                          ]
  , Tok.caseSensitive = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

identifier :: Parser Name
identifier = Tok.identifier lexer

-- TODO: Fix this to parse only negative sign.
integer :: Parser Integer
integer = Tok.natural lexer

stringLit :: Parser String
stringLit = Tok.stringLiteral lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

comma :: Parser String
comma = Tok.comma lexer

semi :: Parser String
semi = Tok.semi lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = Tok.commaSep1 lexer

(<:>) :: (Applicative f) => f a -> f [a] -> f [a]
(<:>) a b = (:) <$> a <*> b

commaSep2 :: Parser a -> Parser [a]
commaSep2 p = (p <* comma) <:> commaSep1 p

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

prefixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
prefixOp s f = Ex.Prefix (reservedOp s >> return f)

binaryOp :: String -> (a -> a -> a) -> Ex.Assoc -> Ex.Operator String () Identity a
binaryOp s f = Ex.Infix (reservedOp s >> return f)

-- | Patterns

pVar = do
  x <- identifier
  return $ PVar x

pInt = do
  n <- integer
  return $ PInt n

pBool = pTrue <|> pFalse
  where
    pTrue = reserved "true" >> return (PBool True)
    pFalse = reserved "false" >> return (PBool False)

pString = do
  s <- Tok.stringLiteral lexer
  return $ PString s

pTag = do
  char '\''
  x <- identifier
  return $ PTag x
  
pList = do
  ps <- brackets $ commaSep pat
  return $ PList ps

-- pCons

pSet = do
  ps <- braces $ commaSep pat
  return $ PSet ps

pTuple = do
  ps <- parens $ commaSep2 pat
  return $ PTuple ps
    
pUnit = reserved "()" >> return PUnit

pWildcard = reserved "_" >> return PWildcard

pat =
      pVar
  <|> pInt
  <|> pBool
  <|> pString
  <|> pList
  <|> pSet
  <|> try pUnit
  <|> try pTuple
  <|> pWildcard

-- | Expressions

eVar = do
  x <- identifier
  return $ EVar x

eImpVar = do
  char '?'
  x <- identifier
  return $ EImpVar x

eInt = do
  n <- integer
  return $ EInt n

eBool = eTrue <|> eFalse
  where
    eTrue  = reserved "true"  >> return (EBool True)
    eFalse = reserved "false" >> return (EBool False)
  
eString = do
  s <- stringLit
  return $ EString s

-- TODO
eTag = do
  char '\''
  x <- identifier
  return $ ETag x

eList = do
  xs <- brackets $ commaSep expr
  return $ EList xs

eSet = do
  xs <- braces $ commaSep expr
  return $ ESet xs

eTuple = do
  xs <- parens $ commaSep2 expr
  return $ ETuple xs

eUnit = reserved "()" >> return EUnit
  
-- | Arithmetic operators, logical operators, sequence
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
--        , [binaryOp ";" ESeq Ex.AssocLeft]
        ]

eIf = do
  reserved "if"
  cond <- expr
  reservedOp "then"
  trueBranch <- expr
  reservedOp "else"
  falseBranch <- expr
  return $ EIf cond trueBranch falseBranch

-- eMatch

eLet = do
  reserved "let"
  p <- pat
  reserved "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return $ ELet p e1 e2

-- ELetRec
-- EAssign
-- ERef
-- EDeref

eLam = do
  reserved "lam"
  x <- atomExpr
  reserved "."
  e <- expr
  return $ ELam x e

-- TODO
eApp = do
  f <- atomExpr
  x <- atomExpr
  return $ EApp f x

eRd = do
  reserved "rd"
  c <- expr
  return $ ERd c

eWr = do
  reserved "wr"
  e <- expr
  reserved "->"
  c <- expr
  return $ EWr e c

eNu = do
  reserved "nu"
  c <- expr
  reserved "."
  e <- expr
  return $ ENu c e

eRepl = do
  reserved "!"
  e <- atomExpr
  return $ ERepl e

eFork = do
  reserved "|>"
  e <- atomExpr
  return $ EFork e

eThunk = do
  reserved "thunk"
  e <- atomExpr
  return $ EThunk e

eForce = do
  reserved "force"
  e <- atomExpr
  return $ EForce e

eSeq = do
  e1 <- expr'
  reserved ";"
  e2 <- expr
  return $ ESeq e1 e2
{-  es <- sepBy1 expr' semi
  return $ ESeq es-}

-- | Commands

cExpr = do
  e <- expr
  optional $ reserved ";;"
  return $ CExpr e

cDef = do
  reserved "let"
  x <- pat
  reserved "="
  e <- expr
  optional $ reserved ";;"
  return $ CDef x e

cmd = try cExpr <|> cDef

-- | Parser

expr' :: Parser Expr
expr' = Ex.buildExpressionParser table term

expr =
      try eSeq
  <|> expr'

piExpr =
      eRd
  <|> eWr
  <|> eNu
  <|> eRepl
  <|> eFork

lazyExpr = eThunk <|> eForce

atomExpr =
      eVar
  <|> eImpVar
  <|> eInt
  <|> eBool
  <|> eString
  <|> eList
  <|> eSet
  <|> try eUnit
  <|> try eTuple
  <|> parens expr

term :: Parser Expr
term =
      try eApp
  <|> atomExpr
  <|> eLet
  <|> eIf
  <|> eLam
  <|> lazyExpr
  <|> piExpr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Command]
toplevel = many1 cmd

parser :: String -> Either ParseError [Command]
parser s = parse (contents toplevel) "<stdin>" s
