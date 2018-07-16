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
                  TypeEnv
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

type Constraint = (Type, Type)

type Unifier = (Subst, [Constraint])

-- | Constraint solver monad
type Solve a = ExceptT TypeError Identity a

newtype Subst = Subst (Map.Map TVar Type)
    deriving (Eq, Ord, Show, Monoid)

class Substitutable a where
    apply :: Subst -> a -> a
    ftv   :: a -> Set.Set TVar

instance Substitutable Type where
    apply _ (TCon a) = TCon a
    apply (Subst s) t@(TVar a) = Map.findWithDefault t a s
    apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2
    apply s (TList t) = TList (apply s t)
    apply s (TProd ts) = TProd (apply s ts)
    apply s (TSet t) = TSet (apply s t)
    apply s (TRef t) = TRef (apply s t)
    apply s (TThunk t) = TThunk (apply s t)

    ftv (TVar a) = Set.singleton a
    ftv TCon{} = Set.empty
    ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2
    ftv (TList t) = ftv t
    ftv (TProd ts) = ftv ts
    ftv (TSet t) = ftv t
    ftv (TRef t) = ftv t
    ftv (TThunk t) = ftv t

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

instance Substitutable TypeEnv where
    apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
    ftv (TypeEnv env) = ftv $ Map.elems env

data TypeError
    = UnificationFail Type Type
    | InfiniteType TVar Type
    | UnboundVariable Name
    | Ambiguous [Constraint]
    | UnificationMismatch [Type] [Type]

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

-- | Run the inference monad
runInfer :: TypeEnv -> Infer (Type, [Constraint]) -> Either TypeError (Type, [Constraint])
runInfer env m = runExcept $ evalStateT (runReaderT m env) initInfer

-- | Solve for toplevel type of an expression in a give environment
inferExpr :: TypeEnv -> Expr -> Either TypeError Scheme
inferExpr env ex = case runInfer env (infer ex) of
    Left err       -> Left err
    Right (ty, cs) -> case runSolve cs of
        Left err    -> Left err
        Right subst -> Right $ closeOver $ apply subst ty

-- | Return internal constraints used in solving for type of expression
constraintsExpr :: TypeEnv -> Expr -> Either TypeError ([Constraint], Subst, Type, Scheme)
constraintsExpr env ex = case runInfer env (infer ex) of
    Left       err -> Left err
    Right (ty, cs) -> case runSolve cs of
        Left err    -> Left err
        Right subst -> Right (cs, subst, ty, sc)
          where sc = closeOver $ apply subst ty

closeOver :: Type-> Scheme
closeOver = normalize . generalize emptyTyEnv

inEnv :: (Name, Scheme) -> Infer a -> Infer a
inEnv (x, sc) m = do
    let scope e = (remove e x) `extend` (x, sc)
    local scope m

lookupEnv :: Name -> Infer Type
lookupEnv x = do
    (TypeEnv env) <- ask
    case Map.lookup x env of
        Nothing -> throwError $ UnboundVariable x
        Just s  -> do t <- instantiate s
                      return t

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
    s <- get
    put s{count = count s + 1}
    return $ TVar $ TV (letters !! count s)

instantiate :: Scheme -> Infer Type-- ^ T-Inst
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = Subst $ Map.fromList $ zip as as'
    return $ apply s t

generalize :: TypeEnv -> Type-> Scheme -- ^ T-Gen
generalize env t = Forall as t
    where as = Set.toList $ ftv t `Set.difference` ftv env

binops :: Binop -> Type
binops Add = tyInt  `TArr` (tyInt  `TArr` tyInt)
binops Sub = tyInt  `TArr` (tyInt  `TArr` tyInt)
binops Mul = tyInt  `TArr` (tyInt  `TArr` tyInt)
binops Div = tyInt  `TArr` (tyInt  `TArr` tyInt)
binops Mod = tyInt  `TArr` (tyInt  `TArr` tyInt)
binops And = tyBool `TArr` (tyBool `TArr` tyBool)
binops Or  = tyBool `TArr` (tyBool `TArr` tyBool)
binops Lt  = tyInt  `TArr` (tyInt  `TArr` tyBool)
binops Gt  = tyInt  `TArr` (tyInt  `TArr` tyBool)
binops Leq = tyInt  `TArr` (tyInt  `TArr` tyBool)
binops Geq = tyInt  `TArr` (tyInt  `TArr` tyBool)
-- TODO: Make polymorphic
binops Eql = tyInt  `TArr` (tyInt  `TArr` tyBool)
binops Neq = tyInt  `TArr` (tyInt  `TArr` tyBool)

unops :: Unop -> Type
unops Not = tyBool  `TArr` tyBool

infer :: Expr -> Infer (Type, [Constraint])
infer expr = case expr of
    EVar x -> do
        t <- lookupEnv x
        return (t, [])

    -- EImpVar x
        
    ELit (LInt _) -> return (tyInt, [])
    ELit (LBool _) -> return (tyBool, [])
    ELit (LString _) -> return (tyString, [])
    ELit (LTag _) -> return (tyTag, [])
    ELit LUnit -> return (tyUnit, [])

    ETuple es -> do
        tcs <- mapM infer es
        let ts = foldr ((:) . fst) [] tcs
            cs = foldr ((++) . snd) [] tcs
        return (TProd ts, cs)
        
    EList [] -> do
        ty <- fresh
        return (TList ty, [])
        
    EList es -> do
        tcs <- mapM infer es
        let tyFst = fst $ head tcs
            cs    = foldr ((++) . snd) [] tcs
            cs'   = map (\x -> (tyFst, fst x)) tcs
        return (TList tyFst, cs ++ cs')

    ESet [] -> do
        ty <- fresh
        return (TSet ty, [])
        
    ESet es -> do
        tcs <- mapM infer es
        let tyFst = fst $ head tcs
            cs    = foldr ((++) . snd) [] tcs
            cs'   = map (\x -> (tyFst, fst x)) tcs
        return (TSet tyFst, cs ++ cs')
    
    EBin op e1 e2 -> do
        (t1, c1) <- infer e1
        (t2, c2) <- infer e2
        tv <- fresh
        let u1 = t1 `TArr` (t2 `TArr` tv)
            u2 = binops op
        return (tv, c1 ++ c2 ++ [(u1, u2)])

    EUn op e -> do
        (t, c) <- infer e
        tv <- fresh
        let u1 = t `TArr` tv
            u2 = unops op
        return (tv, c ++ [(u1, u2)])

    EIf e1 e2 e3 -> do
        (t1, c1) <- infer e1
        (t2, c2) <- infer e2
        (t3, c3) <- infer e3
        return (t2, c1 ++ c2 ++ c3 ++ [(t1, tyBool), (t2, t3)])

    -- TODO
    -- EMatch e bs ->

    -- TODO: Other patterns
    ELet (PVar x) e1 e2 -> do
        env <- ask
        (t1, c1) <- infer e1
        case runSolve c1 of
            Left err -> throwError err
            Right sub -> do
                let sc = generalize (apply sub env) (apply sub t1)
                (t2, c2) <- inEnv (x, sc) $ local (apply sub) (infer e2)
                return (t2, c1 ++ c2)

    EFun (PVar x) e1 e2 -> do
        env <- ask
        (t1, c1) <- infer e1
        case runSolve c1 of
            Left err -> throwError err
            Right sub -> do
                let sc = generalize (apply sub env) (apply sub t1)
                (t2, c2) <- inEnv (x, sc) $ local (apply sub) (infer e2)
                return (t2, c1 ++ c2)

    ELam (PVar x) e -> do
        tv <- fresh
        (t, c) <- inEnv (x, Forall [] tv) (infer e)
        return (tv `TArr` t, c)

    ELam PUnit e -> do
        (t, c) <- (infer e)
        return (tyUnit `TArr` t, c)
        
    EApp e1 e2 -> do
        (t1, c1) <- infer e1
        (t2, c2) <- infer e2
        tv <- fresh
        return (tv, c1 ++ c2 ++ [(t1, t2 `TArr` tv)])

    -- TODO: Cannot infer type of rd e
    ERd e -> do
        (t, c) <- infer e
        ty <- fresh
        return (ty, c ++ [(t, tyChan)])
    
    EWr e1 e2 -> do
        (t1, c1) <- infer e1
        (t2, c2) <- infer e2
        return (tyUnit, c1 ++ c2 ++ [(t2, tyChan)])
        
    ENu x e -> do
        env <- ask
        let sc = generalize env tyChan
        (t, c) <- inEnv (x, sc) $ local (apply emptySubst) (infer e)
        return (t, c)

    ERepl e -> do
        (t, c) <- infer e
        return (tyUnit, c)
    
    EFork e -> do
        (t, c) <- infer e
        return (tyUnit, c)

    -- TODO: Warning if t1 is not of type tyUnit
    ESeq e1 e2 -> do
        (t1, c1) <- infer e1
        (t2, c2) <- infer e2
        return (t2, c1 ++ c2)

    -- TODO: Unop?
    ERef e -> do
        (t, c) <- infer e
        tv <- fresh
        return (tv, c ++ [(tv, TRef t)])

    EDeref e -> do
        (t, c) <- infer e
        tv <- fresh
        return (tv, c ++ [(TRef tv, t)])

    EAssign x e -> do
        t1 <- lookupEnv x
        (t2, c2) <- infer e
        return (tyUnit, c2 ++ [(t1, TRef t2)])

    EThunk e -> do
        (t, c) <- infer e
        tv <- fresh
        return (tv, c ++ [(tv, TThunk t)])

    EForce e -> do
        (t, c) <- infer e
        tv <- fresh
        return (tv, c ++ [(TThunk tv, t)])

    EPrint e -> do
       (t, c) <- infer e
       return (tyUnit, c)

inferTop :: TypeEnv -> [(Name, Expr)] -> Either TypeError TypeEnv
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
    fv (TList a) = fv a
    -- TODO: Refactor?
    fv (TProd as) = foldr ((++) . fv) [] as
    fv (TSet a) = fv a
    fv (TRef a) = fv a
    fv (TThunk a) = fv a
    
    normtype (TVar a)   =
        case Prelude.lookup a ord of
            Just x -> TVar x
            Nothing -> error "type variable not in signature"
    normtype (TCon a)   = TCon a
    normtype (TArr a b) = TArr (normtype a) (normtype b)
    normtype (TList a)   = TList (normtype a)
    normtype (TProd as)   = TProd (map normtype as)
    normtype (TSet a)   = TSet (normtype a)
    normtype (TRef a)   = TRef (normtype a)
    normtype (TThunk a)   = TThunk (normtype a)



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

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] []  = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) =
    do su1 <- unifies t1 t2
       su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
       return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

unifies :: Type-> Type-> Solve Subst
unifies t1 t2 | t1 == t2 = return emptySubst
unifies (TVar v) t = v `bind` t
unifies t (TVar v) = v `bind` t
unifies (TArr t1 t2) (TArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies (TList t1) (TList t2) = unifies t1 t2
unifies (TProd ts1) (TProd ts2) = unifyMany ts1 ts2
unifies (TSet t1) (TSet t2) = unifies t1 t2
unifies (TRef t1) (TRef t2) = unifies t1 t2
unifies (TThunk t1) (TThunk t2) = unifies t1 t2
unifies t1 t2 = throwError $ UnificationFail t1 t2

solver :: Unifier -> Solve Subst
solver (su, cs) =
    case cs of
        [] -> return su
        ((t1, t2) : cs0) -> do
          su1 <- unifies t1 t2
          solver (su1 `compose` su, apply su1 cs0)

bind :: TVar -> Type-> Solve Subst
bind a t | t == TVar a     = return emptySubst
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = return (Subst $ Map.singleton a t)

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t
