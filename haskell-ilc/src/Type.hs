module Type where

import Syntax

import Data.Monoid
import qualified Data.Map as Map

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Ty
    = TVar TVar
    | TCon String
    | TArr Ty Ty
    deriving (Show, Eq, Ord)

infixr `TArr`

data Scheme = Forall [TVar] Ty
    deriving (Show, Eq, Ord)

typeInt, typeBool :: Ty
typeInt  = TCon "Int"
typeBool = TCon "Bool"

data Env = TypeEnv { types :: Map.Map Name Scheme }
    deriving (Eq, Show)

empty :: Env
empty = TypeEnv Map.empty

remove :: Env -> Name -> Env
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

extend :: Env -> (Name, Scheme) -> Env
extend env (x, s) = env { types = Map.insert x s (types env) }

lookup :: Name -> Env -> Maybe Scheme
lookup key (TypeEnv tys) = Map.lookup key tys

merge :: Env -> Env -> Env
merge (TypeEnv a) (TypeEnv b) = TypeEnv (Map.union a b)

instance Monoid Env where
    mempty = empty
    mappend = merge
