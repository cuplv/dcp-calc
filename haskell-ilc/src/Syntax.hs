module Syntax where

data Expr =
    EInt Integer
  | EString String
  | ENil
  | EList [Expr]
  | ETuple [Expr]
  | EPair Expr Expr
  | EUnit
  | ETrue
  | EFalse
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
