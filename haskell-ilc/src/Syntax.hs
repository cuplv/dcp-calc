module Syntax where

import Control.Concurrent.Chan
import Data.IORef
import qualified Data.Map.Strict as Map

type Name = String

data Type
    = TInt
    | TBool
    | TString
    | TTag
    | TUnit
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
    | EFun Pattern Expr Expr
    | EAssign Name Expr
    | ERef Expr
    | EDeref Expr
    | ELam Pattern Expr
    | EApp Expr Expr
    | ERd Expr
    | EWr Expr Expr
    | ENu Name Expr -- ^ Name?
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
    | VChannel Name (Chan Value)
    | VRef (IORef Value)
    deriving (Eq)

data Command
    = CExpr Expr
    | CDef Name Expr
    | CTySig Name Type
    deriving (Eq, Show)

data Program
  = Program [Command]

instance Show Value where
    show (VInt n) = show n
    show (VBool b) = show b
    show (VString s) = show s
    show (VTag t) = show t
    show (VList vs) = show vs
    show (VSet vs) = show vs
    show (VTuple vs) = show vs
    show VUnit = show ()
    show (VClosure _ _ _) = "closure"
    show (VThunk _ _) = "thunk"
    show (VChannel x _) = x
    show (VRef x) = show x

instance Show (IORef a) where
    show _ = "ref"

type Environment = Map.Map Name Value

emptyEnv :: Environment
emptyEnv = Map.empty

extendEnv :: Environment -> Name -> Value -> Environment
extendEnv env x v = Map.insert x v env

updateEnv :: Environment -> [(Name, Value)] -> Environment
updateEnv env env' = foldl f env env'
  where
    f env (x, v) = Map.insert x v env

{-type Context = Map.Map Name Type

emptyCtx :: Context
emptyCtx = Map.empty

extendCtx :: Context -> Name -> Type -> Context
extendCtx env x v = Map.insert x v env

updateCtx :: Context -> [(Name, Type)] -> Context
updateCtx env env' = foldl f env env'
  where
    f env (x, v) = Map.insert x v env-}
