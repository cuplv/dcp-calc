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

import Lexer
import Syntax

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

-- | Types

{-tInt = reserved "Int" >> return TInt

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
  <|> tWr-}
 
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

eInt = mklexer (ELit . LInt) integer

eBool = eTrue <|> eFalse
  where
    eTrue  = reserved "true"  >> return (ELit $ LBool True)
    eFalse = reserved "false" >> return (ELit $ LBool False)

eString = mklexer (ELit . LString) stringLit  

eTag = mklexer (ELit . LTag) $ char '\'' >> identifier

eList = mklexer EList $ brackets $ commaSep expr

eSet = mklexer ESet $ braces $ commaSep expr

eTuple = mklexer ETuple $ parens $ commaSep2 expr

eUnit = reserved "()" >> return (ELit LUnit)
  
-- | Arithmetic operators, logical operators, relations
table :: [[Ex.Operator String () Identity Expr]]
table = [ [binaryOp "*" (EBin Mul) Ex.AssocLeft, binaryOp "/" (EBin Div) Ex.AssocLeft]
        , [binaryOp "%" (EBin Mod) Ex.AssocLeft]
        , [binaryOp "+" (EBin Add) Ex.AssocLeft, binaryOp "-" (EBin Sub) Ex.AssocLeft]
        , [prefixOp "not" (EUn Not)]
        , [binaryOp "<" (EBin Lt) Ex.AssocNone
          , binaryOp ">" (EBin Gt) Ex.AssocNone
          , binaryOp "<=" (EBin Leq) Ex.AssocNone
          , binaryOp ">=" (EBin Geq) Ex.AssocNone
          , binaryOp "==" (EBin Eql) Ex.AssocNone
          , binaryOp "<>" (EBin Neq) Ex.AssocNone
          ]
        , [binaryOp "&&" (EBin And) Ex.AssocLeft]
        , [binaryOp "||" (EBin Or) Ex.AssocLeft]
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
    g <- option (ELit $ LBool True) eGuard
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
    args <- reverse <$> many1 pat
    reservedOp "="
    e1 <- expr
    reserved "in"
    e2 <- expr
    return $ EFun p1 (curry e1 args) e2
  where
    curry acc (x:[]) = ELam x acc
    curry acc (x:xs) = curry (ELam x acc) xs

eAssign = do
    reserved "let"
    x <- identifier
    reservedOp ":="
    e <- expr
    return $ EAssign x e

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

-- | Declarations

{-dMain = do
  reserved "let"
  reserved "main"
  reserved "="
  e <- expr
  return $ DExpr e-}

dExpr = do
    e <- expr
    optional $ reserved ";;"
    return $ DExpr e

dDeclLet = do
    reserved "let"
    x <- identifier
    reserved "="
    e <- expr
    optional $ reserved ";;"
    return $ DDecl x e

dDeclFun = do
    reserved "let"
    x <- identifier
    ps <- reverse <$> many1 pat
    reserved "="
    e <- expr
    optional $ reserved ";;"
    return $ DDecl x (curry e ps)
  where
    curry acc (p:[]) = ELam p acc
    curry acc (p:ps) = curry (ELam p acc) ps

dDecl = try dDeclLet <|> dDeclFun
  
{-cTySig = do
  x <- identifier
  reserved "::"
  t <- ty
  return $ CTySig x t

cmd = try cMain <|> try cTySig <|> try cExpr <|> try cDef -- ^ Fix-}

decl = try dExpr <|> try dDecl

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
       <|> eTag
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

toplevel :: Parser [Decl]
toplevel = many1 decl

parserD :: String -> Either ParseError Decl
parserD s = parse (contents

parser :: String -> Either ParseError [Decl]
parser s = parse (contents toplevel) "<stdin>" s
