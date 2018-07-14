module Syntax where

import Control.Concurrent.Chan
import Data.IORef
import qualified Data.Map.Strict as Map

type Name = String

data Expr
    = EVar Name
    | EImpVar Name
    | ELit Lit
    | ETuple [Expr]
    | EList [Expr]
    | ESet [Expr]
    | EBin Binop Expr Expr
    | EUn Unop Expr
    | EIf Expr Expr Expr
    | EMatch Expr [(Pattern, Expr, Expr)] -- Guard into Maybe?
    | ELet Pattern Expr Expr
    | EFun Pattern Expr Expr
    | EAssign Name Expr
    | ELam Pattern Expr
    | EApp Expr Expr
    | ERd Expr
    | EWr Expr Expr
    | ENu Name Expr
    | ERepl Expr
    | EFork Expr
    | ESeq Expr Expr
    | ERef Expr
    | EDeref Expr
    | EThunk Expr
    | EForce Expr
    | EPrint Expr
    deriving (Eq, Show)

data Pattern
    = PVar Name
    | PInt Integer
    | PBool Bool
    | PString String
    | PTag String
    | PTuple [Pattern]
    | PList [Pattern]
    | PCons Pattern Pattern
    | PSet [Pattern]
    | PUnit
    | PWildcard
    deriving (Eq, Show)

data Lit
    = LInt Integer
    | LBool Bool
    | LString String
    | LTag String
    | LUnit
    deriving (Eq, Show)

data Binop
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    | And
    | Or
    | Lt
    | Gt
    | Leq
    | Geq
    | Eql
    | Neq
    deriving (Eq, Show)

data Unop
    = Not
    deriving (Eq, Show)

type Decl = (Name, Expr)

{-data Decl
  = DExpr Expr
  | DDecl Name Expr
  deriving (Eq, Show)-}

data Program = Program [Decl] Expr
    deriving (Eq, Show)

{-data Command
    = CExpr Expr
    | CDef Name Expr
    -- | CTySig Name Type
    deriving (Eq, Show)-}


