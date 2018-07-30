module Parser
    (
      parser
    ) where

import Data.Functor.Identity (Identity)
import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)

import Lexer
import Syntax

--------------------------------------------------------------------------------
-- Parse types
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Parse patterns
--------------------------------------------------------------------------------

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
    colon
    tl <- pat
    return $ PCons hd tl

pSet = mklexer PSet $ braces $ commaSep pat

pTuple = mklexer PTuple $ parens $ commaSep2 pat

pUnit = reserved "()" >> return PUnit

pWildcard = reserved "_" >> return PWildcard

-- TODO: Try using chainl1?

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

--------------------------------------------------------------------------------
-- Parse expressions
--------------------------------------------------------------------------------

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

branch = do
    reservedOp "|"
    p <- pat
    g <- option (ELit $ LBool True) guard
    reservedOp "=>"
    e <- expr
    return (p, g, e)

guard = do
    reserved "when"
    e <- expr
    return e
  
eMatch = do
    reserved "match"
    e <- expr
    reserved "with"
    bs <- many1 branch
    return $ EMatch e bs

eLet = do
    reserved "let"
    ps <- commaSep1 pat
    reservedOp "="
    e1 <- expr
    reserved "in"
    e2 <- expr
    return $ foldr (\p -> ELet p e1) e2 ps

eFun = do
    reserved "let"
    x <- identifier
    args <- many1 pat
    reservedOp "="
    e1 <- expr
    reserved "in"
    e2 <- expr
    return $ EFun x (foldr ELam e1 args) e2

eFunRec = do
    reserved "letrec"
    p <- pat
    args <- many1 pat
    reservedOp "="
    e1 <- expr
    reserved "in"
    e2 <- expr
    return $ ELet p (EFix $ foldr ELam e1 (p:args)) e2

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
    return $ foldl EApp f args

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

eCons = do
    x <- atomExpr
    colon
    xs <- atomExpr
    return $ ECons x xs

expr = try eSeq <|> expr'

expr' = Ex.buildExpressionParser table term

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

term = try eApp
   <|> try eCons
   <|> atomExpr
   <|> try eAssign
   <|> eIf
   <|> eMatch
   <|> try eFunRec
   <|> try eFun
   <|> eLet
   <|> eRd
   <|> eWr
   <|> eNu
   <|> eRepl
   <|> eFork
   <|> eRef
   <|> eDeref
   <|> eLam
   <|> eThunk
   <|> eForce
   <|> ePrint

--------------------------------------------------------------------------------
-- Parse declarations
--------------------------------------------------------------------------------

dExpr = do
    e <- expr
    optional $ reserved ";;"
    return $ ("it", e)

dDeclLet = do
    reserved "let"
    x <- identifier
    reserved "="
    e <- expr
    optional $ reserved ";;"
    return (x, e)

dDeclLetRec = do
    reserved "letrec"
    x <- identifier
    ps <- many1 pat
    reserved "="
    e <- expr
    optional $ reserved ";;"
    return (x, EFix $ foldr ELam e ((PVar x):ps))

-- TODO: Fix
dDeclFun = do
    reserved "let"
    x <- identifier
    ps <- many1 pat
    reserved "="
    e <- expr
    optional $ reserved ";;"
    return (x, foldr ELam e ps)

{-tySig = do
  x <- identifier
  reserved "::"
  t <- ty
  return $ TySig x t-}

decl = try dExpr <|> try dDeclLetRec <|> try dDeclLet <|> dDeclFun

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

contents :: Parser a -> Parser a
contents p = mklexer id $ whitespace *> p <* eof

toplevel :: Parser [Decl]
toplevel = many1 decl

parser :: String -> Either ParseError [Decl]
parser s = parse (contents toplevel) "<stdin>" s
