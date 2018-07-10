module Parser
    (
      parser
    ) where

import Data.Functor.Identity

import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

import Syntax

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
                          , "letrec"
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
                          , "match"
                          , "with"
                          , "ref"
                          , "when"
                          , "print"
                          , "Int"
                          , "Bool"
                          , "String"
                          , "Chan"
                          , "Rd"
                          , "Wr"
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
                            , ":"
                            , "::"
                            , ":="
                            , "@"
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

mklexer e p = p >>= \x -> return (e x)

-- | Types

tInt = reserved "Int" >> return TInt

tBool = reserved "Bool" >> return TBool

tString = reserved "String" >> return TString

tChan = reserved "Chan" >> return TChan

tProd = mklexer TProd $ parens $ commaSep2 ty

tRd = mklexer TRd $ reserved "Rd" >> ty

tWr = mklexer TWr $ reserved "Wr" >> ty

tArrow = do
    t1 <- ty'
    reserved "->"
    t2 <- ty
    return $ TArrow t1 t2

tList = mklexer TList $ brackets $ ty    

ty = try tArrow <|> ty'
ty' = tInt
  <|> tBool
  <|> tString
  <|> tChan
  <|> tProd
  <|> tList
  <|> tRd
  <|> tWr
 
-- | Patterns

pVar = mklexer PVar identifier

pInt = mklexer PInt integer

pBool = pTrue <|> pFalse
  where
    pTrue = reserved "true" >> return (PBool True)
    pFalse = reserved "false" >> return (PBool False)

pString = mklexer PString stringLit

pTag = mklexer PTag $ char '\'' >> identifier
  
pList = mklexer PList $ brackets $ commaSep pat

pCons = do
    hd <- pat'
    spaces
    string ":"
    spaces
    tl <- pat
    return (PCons hd tl)

pSet = mklexer PSet $ braces $ commaSep pat

pTuple = mklexer PTuple $ parens $ commaSep2 pat

pUnit = reserved "()" >> return PUnit

pWildcard = reserved "_" >> return PWildcard

pat = try pCons <|> pat'

pat' = pVar
  <|> pInt
  <|> pBool
  <|> pString
  <|> pTag
  <|> pList
  <|> pSet
  <|> try pUnit
  <|> pTuple
  <|> pWildcard

-- | Expressions

eVar = mklexer EVar identifier

eImpVar = mklexer EImpVar $ char '?' >> identifier

eInt = mklexer EInt integer

eBool = eTrue <|> eFalse
  where
    eTrue  = reserved "true"  >> return (EBool True)
    eFalse = reserved "false" >> return (EBool False)

eString = mklexer EString stringLit  

eTag = mklexer ETag $ char '\'' >> identifier

eList = mklexer EList $ brackets $ commaSep expr

eSet = mklexer ESet $ braces $ commaSep expr

eTuple = mklexer ETuple $ parens $ commaSep2 expr

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
        ]

eIf = do
    reserved "if"
    b <- expr
    reserved "then"
    e1 <- expr
    reserved "else"
    e2 <- expr
    return $ EIf b e1 e2

eBranch = do
    reservedOp "|"
    p <- pat
    g <- option (EBool True) eGuard
    reservedOp "=>"
    e <- expr
    return (p, g, e)

eGuard = do
    reserved "when"
    e <- expr
    return e
  
eMatch = do
    reserved "match"
    e <- expr
    reserved "with"
    bs <- many1 eBranch
    return $ EMatch e bs

eLet = do
    reserved "let"
    ps <- commaSep1 pat
    reservedOp "="
    e1 <- expr
    reserved "in"
    e2 <- expr
    return $ curry e2 ps e1
  where
    curry acc (p:[]) e = ELet p e acc
    curry acc (p:ps) e = curry (ELet p e acc) ps e

eFun = do
    reserved "let"
    p1 <- pat
    args <- many1 pat
    reservedOp "="
    e1 <- expr
    reserved "in"
    e2 <- expr
    return $ EFun p1 (curry e1 $ reverse args) e2
  where
    curry acc (x:[]) = ELam x acc
    curry acc (x:xs) = curry (ELam x acc) xs

eAssign = do
    reserved "let"
    p <- pat
    reservedOp ":="
    e <- expr
    return $ EAssign p e

eRef = mklexer ERef $ reserved "ref" >> atomExpr

eDeref = mklexer EDeref $ reservedOp "@" >> atomExpr

eLam = do
    reserved "lam"
    x <- pat
    reserved "."
    e <- expr
    return $ ELam x e

eApp = do
    f <- atomExpr
    args <- many1 atomExpr
    return $ curry f args
  where
    curry acc (x:[]) = EApp acc x
    curry acc (x:xs) = curry (EApp acc x) xs

eRd = mklexer ERd $ reserved "rd" >> expr

eWr = do
    reserved "wr"
    e <- expr
    reserved "->"
    c <- expr
    return $ EWr e c

eNu = do
    reserved "nu"
    c <- identifier
    reserved "."
    e <- expr
    return $ ENu c e

eRepl = mklexer ERepl $ reservedOp "!" >> atomExpr  

eFork = mklexer EFork $ reservedOp "|>" >> atomExpr

eThunk = mklexer EThunk $ reserved "thunk" >> atomExpr

eForce = mklexer EForce $ reserved "force" >> atomExpr

eSeq = do
    e1 <- expr'
    reserved ";"
    e2 <- expr
    return $ ESeq e1 e2

ePrint = mklexer EPrint $ reserved "print" >> atomExpr    

-- | Commands

cMain = do
  reserved "let"
  reserved "main"
  reserved "="
  e <- expr
  return $ CExpr e

cExpr = do
    e <- expr
    optional $ reserved ";;"
    return $ CExpr e

cDefLet = do
    reserved "let"
    x <- identifier
    reserved "="
    e <- expr
    optional $ reserved ";;"
    return $ CDef x e

cDefFun = do
    reserved "let"
    x <- identifier
    ps <- many pat
    reserved "="
    e <- expr
    optional $ reserved ";;"
    return $ CDef x (curry e $ reverse ps)
  where
    curry acc (p:[]) = ELam p acc
    curry acc (p:ps) = curry (ELam p acc) ps

cDef = try cDefLet <|> cDefFun    

cTySig = do
  x <- identifier
  reserved "::"
  t <- ty
  return $ CTySig x t

cmd = try cMain <|> try cTySig <|> try cExpr <|> try cDef -- ^ Fix

-- | Parser

expr = try eSeq <|> expr'

expr' = Ex.buildExpressionParser table term

piExpr = eRd
     <|> eWr
     <|> eNu
     <|> eRepl
     <|> eFork

lazyExpr = eThunk
       <|> eForce

atomExpr = eVar
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
term = try eApp
   <|> atomExpr
   <|> try eAssign
   <|> eIf
   <|> eMatch
   <|> try eFun
   <|> eLet
   <|> eRef
   <|> eDeref
   <|> eLam
   <|> lazyExpr
   <|> piExpr
   <|> ePrint

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
