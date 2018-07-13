{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- Look up what this means.

module Infer where

import Control.Monad.State
import Control.Monad.Except
import Data.Monoid
import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Syntax
import Type

newtype TypeEnv = TypeEnv (Map.Map Name Scheme)
    deriving Monoid

data Unique = Unique { count :: Int }

type Infer = ExceptT TypeError (State Unique)
type Subst = Map.Map TVar Ty

data TypeError
    = UnificationFail Ty Ty
    | InfiniteType TVar Ty
    | UnboundVariable Name

runInfer :: Infer (Subst, Ty) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) initUnique of
    Left err  -> Left err
    Right res -> Right $ closeOver res

closeOver :: (Map.Map TVar Ty, Ty) -> Scheme -- ?
closeOver (sub, ty) = normalize sc
    where sc = generalize emptyTyenv (apply sub ty)

initUnique :: Unique
initUnique = Unique { count = 0 }

extend :: TypeEnv -> (Name, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

emptyTyenv :: TypeEnv
emptyTyenv = TypeEnv Map.empty

typeof :: TypeEnv -> Name -> Maybe Scheme
typeof (TypeEnv env) name = Map.lookup name env

class Substitutable a where
    apply :: Subst -> a -> a
    ftv   :: a -> Set.Set TVar

instance Substitutable Ty where
    apply _ (TCon a) = TCon a
    apply s t@(TVar a) = Map.findWithDefault t a s
    apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2

    ftv TCon{} = Set.empty  -- What do the braces mean?
    ftv (TVar a) = Set.singleton a
    ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Scheme where
    apply s (Forall as t) = Forall as $ apply s' t
                            where s' = foldr Map.delete s as

    ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable a => Substitutable [a] where
    apply = fmap . apply
    ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
    apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
    ftv (TypeEnv env) = ftv $ Map.elems env

nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Ty
fresh = do
    s <- get
    put s{count = count s + 1}
    return $ TVar $ TV (letters !! count s)

{-occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

unify :: Ty -> Ty -> Infer Subst
unify (TCon a) (TCon b) | a == b = return nullSubst -- ^ Uni-Const?
unify (TVar a) t = bind a t -- ^ Uni-VarLeft
unify t (Tvar a) = bind a t -- ^ Uni-VarRight
unify (l `TArr` r) (l' `TArr` r') = do -- ^ Uni-Arrow
    s1 <- unify l l'
    s2 <- unify (apply s1 r) (apply s1 r')
    return (s2 `compose` s1)

unify t1 t2 = throwError $ UnificationFail t1 t2


bind :: TVar -> Ty -> Infer Subst
bind a t | t == TVar a     = return nullSubst -- ^ Uni-Var
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = return $ Map.singleton a t

instantiate :: Scheme -> Infer Ty -- ^ T-Inst
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = Map.fromList $ zip as as'
    return $ apply s t

generalize :: TypeEnv -> Ty -> Scheme -- ^ T-Gen
generalize env t = Forall as t
    where as = Set.toList $ ftv t `Set.difference` ftv env

infer :: TypeEnv -> Expr -> Infer (Subst, Ty)
infer env ex = case ex of
    EVar x -> lookupEnv env x -- ^ T-Var
    -- TODO: Unit?
    ELam (PVar x) e -> do  -- ^ T-Lam
        tv <- fresh
        let env' = env `extend` (x, Forall [] tv)
        (s1, t1) <- infer env' e
        return (s1, apply s1 tv `TArr` t1)
    EApp e1 e2 -> do
        tv <- fresh
        (s1, t1) <- infer env e1
        (s2, t2) <- infer (apply s1 env) e2
        s3       <- unify (apply s2 t1) (TArr t2 tv)
        return (s3 `compose` s2 `compose` s1, apply s3 tv)
    ELet (PVar x) e1 e2 -> do
        (s1, t1) <- infer env e1
        let env' = apply s1 env
              t' = generalize env' t1
        (s2, t2) <- infer (env' `extend` (x, t')) e2
        return (s1 `compose` s2, t2)
    EInt _ -> return (nullSubst, typeInt)
    EBool _ -> return (nullSubst, typeBool)

lookupEnv :: TypeEnv -> Name -> Infer (Subst, Ty)
lookupEnv (TypeEnv env) x = do
    case Map.lookup x env of
        Nothing -> throwError $ UnboundVariable (show x)
        Just s  -> do t <- instantiate s
                      return (nullSubst, t)
-}
