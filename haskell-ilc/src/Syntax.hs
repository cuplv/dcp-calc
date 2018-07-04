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

data Expr
    = EInt Integer
    | EBool Bool
    | EString String
    | EPair Expr Expr
    | ENil
    | EList [Expr]
    | ETuple [Expr]
    | EUnit
    | EIf Expr Expr Expr
    | EPlus Expr Expr
    | EMinus Expr Expr
    | ETimes Expr Expr
    | EDivide Expr Expr
    | EMod Expr Expr
    | EOr Expr Expr
    | EAnd Expr Expr
    | ENot Expr
    | ELt Expr Expr
    | EGt Expr Expr
    | ELeq Expr Expr
    | EGeq Expr Expr
    | EEq Expr Expr
    | ENeq Expr Expr
    deriving (Eq, Show)

data Value
    = VInt Integer
    | VBool Bool
    | VString String
    | VPair Value Value
    | VClosure Env Expr
    deriving (Eq, Show)
