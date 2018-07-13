module Infer where

import Control.Monad.State
import Control.Monad.Except

import Syntax
import Type

type Infer a = ExceptT TypeError (State Unique a)

runInfer :: Infer (Subst, Ty) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) initUnique of
    Left err -> Left err
    Right res -> Right $ closeOver res

type Subst = Map.Map TVar Ty

nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

class Substitutable a where
    apply :: Subst -> a -> a
    ftv   :: a -> Set.Set TVar

instance Substitutable Ty where
    apply _ (TCon a) = TCon a
    apply s t@(TVar a) = Map.findWithDefault t a s
    apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2

    ftv TCon{} = Set.empty -- Wot
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

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
    s <- get
    put s{count = count s + 1}
    return $ TVar $ TV (letters !! count s)
