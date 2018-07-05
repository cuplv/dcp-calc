module Syntax where

type Name = String

type Env = [(Name, Value)]

data Type
    = TInt
    | TBool
    | TString
    | TPair Type Type
    | TArrow Type Type
    | TList Type
    deriving (Eq, Show)

data Pattern
    = PVar Name
    | PInt Integer
    | PBool Bool
    | PString String
    | PTag String
    | PList [Pattern]
    | PSet [Pattern]
    | PCons Pattern Pattern
    | PTuple [Pattern]
    | PUnit
    | PWildcard
    deriving (Eq, Show)

data Expr
    = EVar Name
    | EImpVar Name
    | EInt Integer
    | EBool Bool
    | EString String
    | ETag String
    | EList [Expr]
    | ESet [Expr]
    | ETuple [Expr]
    | EUnit
    | EPlus Expr Expr
    | EMinus Expr Expr
    | ETimes Expr Expr
    | EDivide Expr Expr
    | EMod Expr Expr
    | ENot Expr
    | EAnd Expr Expr
    | EOr Expr Expr
    | ELt Expr Expr
    | EGt Expr Expr
    | ELeq Expr Expr
    | EGeq Expr Expr
    | EEq Expr Expr
    | ENeq Expr Expr
    | EIf Expr Expr Expr
    | EMatch Expr [(Pattern, Expr)]
    | ELet Pattern Expr Expr
    | ELetRec Pattern Expr Expr
    | EAssign Pattern Expr
    | ERef Expr
    | EDeref Expr
    | ELam Expr Expr
    | EApp Expr Expr
    | ERd Expr
    | EWr Expr Expr
    | ENu Expr Expr
    | ERepl Expr
    | EFork Expr
    | EThunk Expr
    | EForce Expr
    | ESeq Expr Expr
    deriving (Eq, Show)

data Value
    = VInt Integer
    | VBool Bool
    | VString String
    | VPair Value Value
    | VList [Value]
    | VTuple [Value]
    | VUnit
    | VClosure Env Expr
    deriving (Eq, Show)
