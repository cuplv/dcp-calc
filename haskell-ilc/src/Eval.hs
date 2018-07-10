module Eval where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import qualified Data.Map.Strict as Map
import Data.Maybe

import Syntax

evalSub :: Environment -> Expr -> IO Value
evalSub env e = newEmptyMVar >>= \x ->
                eval' env x e >>
                takeMVar x >>= return
                
evalSubs :: Environment -> Expr -> Expr -> IO (Value, Value)
evalSubs env e1 e2 = evalSub env e1 >>= \v1 ->
                     evalSub env e2 >>= \v2 ->
                     return (v1, v2)

evalBinOp :: ((Value, Value) -> IO Value)
          -> Environment
          -> MVar Value
          -> Expr
          -> Expr
          -> IO ()
evalBinOp f env x e1 e2 =
    evalSubs env e1 e2 >>= f >>= putMVar x
    
evalArith :: (Integer -> Integer -> Integer)
          -> Environment
          -> MVar Value
          -> Expr
          -> Expr
          -> IO ()
evalArith op = evalBinOp (go op)
  where
    go op v = return $ case v of
        (VInt n1, VInt n2) -> VInt (op n1 n2)
        _                  -> error "expected integer operands"

evalBool :: (Bool -> Bool -> Bool)
          -> Environment
          -> MVar Value
          -> Expr
          -> Expr
          -> IO ()
evalBool op = evalBinOp (go op)
  where
    go op v = return $ case v of
        (VBool b1, VBool b2) -> VBool (op b1 b2)
        _                  -> error "expected boolean operands"

evalRel :: (Integer -> Integer -> Bool)
          -> Environment
          -> MVar Value
          -> Expr
          -> Expr
          -> IO ()
evalRel op = evalBinOp (go op)
  where
    go op v = return $ case v of
        (VInt b1, VInt b2) -> VBool (op b1 b2)
        _                  -> error "expected integer operands"

eval :: Environment -> Expr -> IO ()
eval env e = newEmptyMVar >>= \v ->
         eval' env v e >>
         takeMVar v >>= putStrLn . show

eval' env x (EVar x') = putMVar x $ env Map.! x'

--eval' env x (EImpVar x') = 
  
eval' env x (EInt n) = putMVar x $ VInt n
    
eval' env x (EBool b) = putMVar x $ VBool b

eval' env x (EString s) = putMVar x $ VString s

eval' env x (ETag s) = putMVar x $ VTag s

-- eval env (EList es) = VList $ map (eval env) es
--eval' env x (EList es) = putMVar x $ VTag s

eval' env x (EPlus e1 e2) = evalArith (+) env x e1 e2

eval' env x (EMinus e1 e2) = evalArith (-) env x e1 e2

eval' env x (ETimes e1 e2) = evalArith (*) env x e1 e2

eval' env x (EDivide e1 e2) = evalArith quot env x e1 e2

eval' env x (EMod e1 e2) = evalArith mod env x e1 e2

eval' env x (ENot e) = evalSub env e >>= go >>= putMVar x
  where
    go v = return $ case v of
        VBool b -> VBool $ not b
        _       -> error "expected boolean"

eval' env x (EAnd e1 e2) = evalBool (&&) env x e1 e2

eval' env x (EOr e1 e2) = evalBool (||) env x e1 e2

eval' env x (ELt e1 e2) = evalRel (<) env x e1 e2

eval' env x (EGt e1 e2) = evalRel (>) env x e1 e2

eval' env x (ELeq e1 e2) = evalRel (<=) env x e1 e2

eval' env x (EGeq e1 e2) = evalRel (>=) env x e1 e2

eval' env x (EEq e1 e2) = evalRel (==) env x e1 e2

eval' env x (ENeq e1 e2) = evalRel (/=) env x e1 e2

eval' env x (EIf e1 e2 e3) =
    evalSub env e1 >>= \cond ->
    (case cond of
        VBool True  -> evalSub env e2
        VBool False -> evalSub env e3) >>=
    putMVar x 

{-eval env (EMatch e bs) = pmThenEval env (eval env e) bs

eval env (ELet p e1 e2) = eval env' e2
  where
    env' = update env binds
    v = eval env e1
    binds = fromMaybe (error "let pattern matching failed") $
            pmEnv v p
eval env (EFun p e1 e2) = eval env' e2
  where
    env' = update env binds
    binds = fromMaybe (error "let pattern matching failed") $
            pmEnv f p
    f = case (p, eval env e1) of
            (PVar x, VClosure arg env e) -> VClosure arg (extend env x f) e
            _                            -> error "expected closure"
-- eval env (EAssign p e) =
-- eval env (ERef e)
-- eval env (EDeref e)
eval env (ELam p e) = VClosure name env e
  where
    name = case p of
        PVar x -> Just x
        _      -> Nothing
eval env (EApp e1 e2) =
    case (eval env e1, eval env e2) of
        (VClosure x env e, v) -> eval env' e
          where
            env' = case x of
                Just x  -> extend env x v
                Nothing -> error "wot"
        _                     -> error "expected closure"-}

eval' env x (ENu c e) =
    newChan >>= \c' ->
    evalSub (extend env c $ VChannel c c') e >>= putMVar x
    
eval' env x (ERd e) =
    evalSub env e >>= \v ->
    return (case v of
        VChannel _ c -> c) >>= readChan >>= putMVar x

eval' env x (EWr e1 e2) =
    evalSub env e1 >>= \v1 ->
    evalSub env e2 >>= \v2 ->
    return (case v2 of
        VChannel _ c -> c) >>= \c ->
    writeChan c v1 >>
    putMVar x VUnit
    
eval' env x (EFork e) = do
    x' <- newEmptyMVar
    forkIO $ do eval' env x' e
    putMVar x VUnit

eval' env x (ERepl e) = do
    x' <- newEmptyMVar
    forkIO $ forever $ do eval' env x' e
    putMVar x VUnit

eval' env x (EThunk e) = putMVar x $ VThunk env e

eval' env x (EForce e) =
    evalSub env e >>= \v ->
    (case v of
         VThunk env' e' -> evalSub env' e'
         _              -> error "expected thunk") >>= putMVar x
    
eval' env x (ESeq e1 e2) =
    evalSub env e1 >> evalSub env e2 >>= putMVar x

eval' env x (EPrint e) =
    evalSub env e >>= putStrLn . show >> putMVar x VUnit

{-pmThenEval :: Environment -> Value -> [(Pattern, Expr, Expr)] -> Value
pmThenEval env v [] = error "pattern match failed" -- ^ Better error handling?
pmThenEval env v ((p, g, e):bs) =
    case (pmEnv v p) of
        Just env' -> if g' == VBool True
                     then eval env'' e
                     else pmThenEval env v bs
          where
            env'' = update env env'
            g'    = eval env'' g
        Nothing   -> pmThenEval env v bs

(<:>) :: Applicative f => f [a] -> f [a] -> f [a]
(<:>) a b = pure (++) <*> a <*> b

pmEnv :: Value -> Pattern -> Maybe [(Name, Value)]
pmEnv v p = go [] v p
  where
    go acc v (PVar x) = Just ((x, v) : acc)
    go acc (VInt n) (PInt n') | n == n'   = Just acc
                              | otherwise = Nothing
    go acc (VBool b) (PBool b') | b == b'   = Just acc
                                | otherwise = Nothing
    go acc (VString s) (PString s') | s == s'   = Just acc
                                    | otherwise = Nothing
    go acc (VTag t) (PTag t') | t == t'   = Just acc
                              | otherwise = Nothing
    go acc (VList vs) (PList ps) = gos acc vs ps
    go acc (VList (v:vs)) (PCons p ps) = foldl1 (<:>) [acc1, acc2, Just acc]
      where
        acc1 = pmEnv v p
        acc2 = pmEnv (VList vs) ps
    go acc (VList []) (PCons _ _) = Nothing
    -- TODO: Set pattern matching not implemented.
    go acc (VSet vs) (PSet ps) = gos acc vs ps
    go acc (VTuple vs) (PTuple ps) = gos acc vs ps
    go acc VUnit PUnit = Just acc
    go acc _ PWildcard = Just acc

    gos acc vs ps | length vs == length ps = foldl (<:>) (Just []) accs
                  | otherwise              = Nothing
      where
        accs  = map (\pair -> case pair of (v, p) -> go acc v p) vp
        vp    = zip vs ps-}

exec :: [Command] -> IO ()
exec cmds = go emptyEnv cmds
  where
    go env ((CExpr e):[] ) = do
      eval env e
    go env ((CExpr e):rest) = do
      eval env e
      go env rest
    -- TODO: 
    go env ((CDef x e):rest) = do
      go (extend env x (VClosure Nothing env e)) rest
    go env ((CTySig _ _):rest) = do
      go env rest
    go env [] = do
      return ()

