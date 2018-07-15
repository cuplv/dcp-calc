module Type where

import Syntax

import Data.Monoid
import qualified Data.Map as Map

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Type
    = TVar TVar
    | TCon String
    | TArr Type Type
    deriving (Show, Eq, Ord)

infixr `TArr`

data Scheme = Forall [TVar] Type
    deriving (Show, Eq, Ord)

typeInt, typeBool :: Type
typeInt  = TCon "Int"
typeBool = TCon "Bool"

data TypeEnv = TypeEnv { types :: Map.Map Name Scheme }
    deriving (Eq, Show)

emptyTyEnv :: TypeEnv
emptyTyEnv = TypeEnv Map.empty

remove :: TypeEnv -> Name -> TypeEnv
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

extend :: TypeEnv -> (Name, Scheme) -> TypeEnv
extend env (x, s) = env { types = Map.insert x s (types env) }

lookup :: Name -> TypeEnv -> Maybe Scheme
lookup key (TypeEnv tys) = Map.lookup key tys

merge :: TypeEnv -> TypeEnv -> TypeEnv
merge (TypeEnv a) (TypeEnv b) = TypeEnv (Map.union a b)

instance Monoid TypeEnv where
    mempty = emptyTyEnv
    mappend = merge
