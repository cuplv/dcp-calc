module Type where

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
