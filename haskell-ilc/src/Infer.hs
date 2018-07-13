{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Infer where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Monoid
import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Syntax
import Type

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

-- | Inference monad
type Infer a = (ReaderT
                  Env
                  (StateT
                  InferState
                  (Except
                    TypeError))
                  a)

-- | Inference state
data InferState = InferState { count :: Int }

-- | Initial inference state
initInfer :: InferState
initInfer = InferState { count = 0 }

type Constraint = (Ty, Ty)

type Unifier = (Subst, [Constraint])

-- | Constraint solver monad
type Solve a = ExceptT TypeError Identity a

newtype Subst = Subst (Map.Map TVar Ty)
    deriving (Eq, Ord, Show, Monoid)

class Substitutable a where
    apply :: Subst -> a -> a
    ftv   :: a -> Set.Set TVar

instance Substitutable Ty where
    apply _ (TCon a) = TCon a
    apply (Subst s) t@(TVar a) = Map.findWithDefault t a s
    apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2

    ftv TCon{} = Set.empty  -- What do the braces mean?
    ftv (TVar a) = Set.singleton a
    ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Scheme where
    apply (Subst s) (Forall as t) = Forall as $ apply s' t
                            where s' = Subst $ foldr Map.delete s as
    ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable Constraint where
    apply s (t1, t2) = (apply s t1, apply s t2)
    ftv (t1, t2) = ftv t1 `Set.union` ftv t2

instance Substitutable a => Substitutable [a] where
    apply = fmap . apply
    ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable Env where
    apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
    ftv (TypeEnv env) = ftv $ Map.elems env

data TypeError
    = UnificationFail Ty Ty
    | InfiniteType TVar Ty
    | UnboundVariable Name
    | Ambiguous [Constraint]
    | UnificationMismatch [Ty] [Ty]
    deriving (Show)

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

-- | Run the inference monad
runInfer :: Env -> Infer (Ty, [Constraint]) -> Either TypeError (Ty, [Constraint])
runInfer env m = runExcept $ evalStateT (runReaderT m env) initInfer

-- | Solve for toplevel type of an expression in a give environment
inferExpr :: Env -> Expr -> Either TypeError Scheme
inferExpr env ex = case runInfer env (infer ex) of
    Left err       -> Left err
    Right (ty, cs) -> case runSolve cs of
        Left err    -> Left err
        Right subst -> Right $ closeOver $ apply subst ty

-- | Return internal constraints used in solving for type of expression
constraintsExpr :: Env -> Expr -> Either TypeError ([Constraint], Subst, Ty, Scheme)
constraintsExpr env ex = case runInfer env (infer ex) of
    Left       err -> Left err
    Right (ty, cs) -> case runSolve cs of
        Left err    -> Left err
        Right subst -> Right (cs, subst, ty, sc)
          where sc = closeOver $ apply subst ty

closeOver :: Ty -> Scheme
closeOver = normalize . generalize Type.empty

inEnv :: (Name, Scheme) -> Infer a -> Infer a
inEnv (x, sc) m = do
    let scope e = (remove e x) `extend` (x, sc)
    local scope m

lookupEnv :: Name -> Infer Ty
lookupEnv x = do
    (TypeEnv env) <- ask
    case Map.lookup x env of
        Nothing -> throwError $ UnboundVariable x
        Just s  -> do t <- instantiate s
                      return t

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Ty
fresh = do
    s <- get
    put s{count = count s + 1}
    return $ TVar $ TV (letters !! count s)

instantiate :: Scheme -> Infer Ty -- ^ T-Inst
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = Subst $ Map.fromList $ zip as as'
    return $ apply s t

generalize :: Env -> Ty -> Scheme -- ^ T-Gen
generalize env t = Forall as t
    where as = Set.toList $ ftv t `Set.difference` ftv env

infer :: Expr -> Infer (Ty, [Constraint])
infer expr = case expr of
    EInt _ -> return (typeInt, [])
    EBool _ -> return (typeBool, [])
    
    EVar x -> do
        t <- lookupEnv x
        return (t, [])

    ELam (PVar x) e -> do
        tv <- fresh
        (t, c) <- inEnv (x, Forall [] tv) (infer e)
        return (tv `TArr` t, c)
        
    EApp e1 e2 -> do
        (t1, c1) <- infer e1
        (t2, c2) <- infer e2
        tv <- fresh
        return (tv, c1 ++ c2 ++ [(t1, t2 `TArr` tv)])
        
    ELet (PVar x) e1 e2 -> do
        env <- ask
        (t1, c1) <- infer e1
        case runSolve c1 of
            Left err -> throwError err
            Right sub -> do
                let sc = generalize (apply sub env) (apply sub t1)
                (t2, c2) <- inEnv (x, sc) $ local (apply sub) (infer e2)
                return (t2, c1 ++ c2)

inferTop :: Env -> [(String, Expr)] -> Either TypeError Env
inferTop env [] = Right env
inferTop env ((name, ex):xs) = case inferExpr env ex of
    Left err -> Left err
    Right ty -> inferTop (extend env (name, ty)) xs

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (map snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (map TV letters)

    fv (TVar a) = [a]
    fv (TArr a b) = fv a ++ fv b
    fv (TCon _) = []

    normtype (TArr a b) = TArr (normtype a) (normtype b)
    normtype (TCon a)   = TCon a
    normtype (TVar a)   =
        case Prelude.lookup a ord of
            Just x -> TVar x
            Nothing -> error "type variable not in signature"

-------------------------------------------------------------------------------
-- Constraint Solver
-------------------------------------------------------------------------------

emptySubst :: Subst
emptySubst = mempty

compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runIdentity $ runExceptT $ solver st
    where st = (emptySubst, cs)

unifyMany :: [Ty] -> [Ty] -> Solve Subst
unifyMany [] []  = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) =
    do su1 <- unifies t1 t2
       su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
       return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

unifies :: Ty -> Ty -> Solve Subst
unifies t1 t2 | t1 == t2 = return emptySubst
unifies (TVar v)t  = v `bind` t
unifies t (TVar v) = v `bind` t
unifies (TArr t1 t2) (TArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwError $ UnificationFail t1 t2

solver :: Unifier -> Solve Subst
solver (su, cs) =
    case cs of
        [] -> return su
        ((t1, t2) : cs0) -> do
          su1 <- unifies t1 t2
          solver (su1 `compose` su, apply su1 cs0)

bind :: TVar -> Ty -> Solve Subst
bind a t | t == TVar a     = return emptySubst
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = return (Subst $ Map.singleton a t)

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t
