module Type where

import qualified Data.Map.Strict as Map

import Syntax

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Ty
    = TVar TVar
    | TCon String
    | TArr Ty Ty
    deriving (Show, Eq, Ord)

typeInt, typeBool :: Ty
typeInt  = TCon "Int"
typeBool = TCon "Bool"

data Scheme = Forall [TVar] Ty

newtype TypeEnv = TypeEnv (Map.Map Name Scheme)

extend :: TypeEnv -> (Name, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env



{-check :: Context -> Type -> Expr -> Either String Type
check ctx ty expr | ty == (typeOf ctx expr) = Right ty
                  | otherwise               = Left "type error"

typeOf :: Context -> Expr -> Type
typeOf ctx (EVar x) = ctx Map.! x
typeOf _   (EInt _) = TInt
typeOf _   (EBool _) = TBool
typeOf _   (EString _) = TString
typeOf _   (ETag _) = TTag -- ^ Necessary?
-- typeOf ctx (EList []) =
{-typeOf ctx (EList (e:es)) = 
  where
    tyE = typeOf ctx e
    tyEs = foldl (\ty1 ty2 -> (map (check ctx tyE) es)
    tyCheck = all (==tyE) $ map (check ctx tyE) es-}
-- typeOf ctx (ESet []) =                      
-- typeOf ctx (ESet (e:es)) =
-- typeOf ctx (ETuple []) =
-- typeOf ctx (ETuple (e:es)) =
typeOf _ EUnit = TUnit
-- typeOf ctx (EPlus e1 e2) = -}
