module Syntax where

import qualified Data.Map.Strict as Map

type Name = String

data Type
    = TInt
   | TBool
   | TString
   | TChan
   | TProd [Type]
   | TArrow Type Type
   | TList Type
   | TRd Type
   | TWr Type
   deriving (Eq, Show)

data Pattern
    = PVar Name
    | PInt Integer
    | PBool Bool
    | PString String
    | PTag String
    | PList [Pattern]
    | PCons Pattern Pattern
    | PSet [Pattern]
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
    | EMatch Expr [(Pattern, Expr, Expr)]
    | ELet Pattern Expr Expr
    | ELetRec Pattern Expr Expr
    | EAssign Pattern Expr
    | ERef Expr
    | EDeref Expr
    | ELam Pattern Expr
    | EApp Expr Expr
    | ERd Expr
    | EWr Expr Expr
    | ENu Expr Expr
    | ERepl Expr
    | EFork Expr
    | EThunk Expr
    | EForce Expr
    | ESeq Expr Expr
    | EPrint Expr
    deriving (Eq, Show)

data Value
    = VInt Integer
    | VBool Bool
    | VString String
    | VTag String
    | VList [Value]
    | VSet [Value]
    | VTuple [Value]
    | VUnit
    | VClosure (Maybe Name) Environment Expr
    | VThunk Environment Expr
    deriving (Eq, Show)

data Command
    = CExpr Expr
    | CDef Name Expr
    | CTySig Name Type
    deriving (Eq, Show)

type Environment = Map.Map Name Value

emptyEnv :: Environment
emptyEnv = Map.empty

extend :: Environment -> Name -> Value -> Environment
extend env x v = Map.insert x v env

update :: Environment -> [(Name, Value)] -> Environment
update env env' = foldl f env env'
  where
    f env (x, v) = Map.insert x v env
