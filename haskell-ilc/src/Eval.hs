module Eval where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe

import Syntax

evalSub :: Environment -> Expr -> IO Value
evalSub env e = newEmptyMVar >>= \m ->
                eval' env m e >>
                takeMVar m >>= return
                
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
evalBinOp f env m e1 e2 =
    evalSubs env e1 e2 >>= f >>= putMVar m
    
evalArith :: (Integer -> Integer -> Integer)
          -> Environment
          -> MVar Value
          -> Expr
          -> Expr
          -> IO ()
evalArith op = evalBinOp $ f op
  where
    f op (VInt n1, VInt n2) = return $ VInt (op n1 n2)
    f op _                  = error "expected integer operands"

evalBool :: (Bool -> Bool -> Bool)
          -> Environment
          -> MVar Value
          -> Expr
          -> Expr
          -> IO ()
evalBool op = evalBinOp $ f op
  where
    f op (VBool b1, VBool b2) = return $ VBool (op b1 b2)
    f op _                  = error "expected boolean operands"

evalRel :: (Integer -> Integer -> Bool)
          -> Environment
          -> MVar Value
          -> Expr
          -> Expr
          -> IO ()
evalRel op = evalBinOp $ f op
  where
    f op (VInt n1, VInt n2) = return $ VBool (op n1 n2)
    f op _                  = error "expected integer operands"


evalList env m con es = mapM (evalSub env) es >>= return . con >>= putMVar m

evalPatMatch :: Environment -> [(Pattern, Expr, Expr)] -> Value -> IO Value
evalPatMatch env ((p, g, e):bs) v =
    case (getBinds p v) of
        Just binds -> let env' = update env binds
                      in evalSub env' g >>= \v ->
                      case v of
                          VBool True  -> evalSub env' e
                          VBool False -> evalPatMatch env bs v
        Nothing    -> evalPatMatch env bs v

(<:>) :: Applicative f => f [a] -> f [a] -> f [a]
(<:>) a b = (++) <$> a <*> b

letBinds p v = fromMaybe (error "let pattern matching failed") $
               getBinds p v
               
getBinds :: Pattern -> Value -> Maybe [(Name, Value)]
getBinds p v = go [] p v
  where
    go acc (PVar x) v= Just ((x, v) : acc)
    go acc (PInt n) (VInt n') | n == n'   = Just acc
                              | otherwise = Nothing
    go acc (PBool b) (VBool b') | b == b'   = Just acc
                                | otherwise = Nothing
    go acc (PString s) (VString s') | s == s'   = Just acc
                                    | otherwise = Nothing
    go acc (PTag t) (VTag t') | t == t'   = Just acc
                              | otherwise = Nothing
    go acc (PList ps) (VList vs) = gos acc ps vs
    go acc (PCons p ps) (VList (v:vs)) = foldl1 (<:>) [acc1, acc2, Just acc]
      where
        acc1 = getBinds p v
        acc2 = getBinds ps (VList vs)
    go acc (PCons _ _) (VList []) = Nothing
    -- TODO: Set pattern matching not implemented.
    go acc (PSet ps) (VSet vs) = gos acc ps vs
    go acc (PTuple ps) (VTuple vs) = gos acc ps vs
    go acc PUnit VUnit = Just acc
    go acc PWildcard _ = Just acc
    gos acc vs ps | length vs == length ps = foldl (<:>) (Just []) accs
                  | otherwise              = Nothing
      where
        accs  = map (\pair -> case pair of (v, p) -> go acc v p) vp
        vp    = zip vs ps


eval :: Environment -> Expr -> IO Value
eval env e = newEmptyMVar >>= \v ->
             eval' env v e >>
             takeMVar v
         
eval' env m (EVar x) = putMVar m $ env Map.! x
--eval' env m (EImpVar x) = 
eval' env m (EInt n) = putMVar m $ VInt n
eval' env m (EBool b) = putMVar m $ VBool b
eval' env m (EString s) = putMVar m $ VString s
eval' env m (ETag s) = putMVar m $ VTag s
eval' env m (EList es) = evalList env m VList es
eval' env m (ESet es) = evalList env m VSet es
eval' env m (ETuple es) = evalList env m VTuple es
eval' env m (EPlus e1 e2) = evalArith (+) env m e1 e2
eval' env m (EMinus e1 e2) = evalArith (-) env m e1 e2
eval' env m (ETimes e1 e2) = evalArith (*) env m e1 e2
eval' env m (EDivide e1 e2) = evalArith quot env m e1 e2
eval' env m (EMod e1 e2) = evalArith mod env m e1 e2
eval' env m (ENot e) = evalSub env e >>= neg >>= putMVar m
  where
    neg (VBool b) = return $ VBool $ not b
    neg _         = error "expected boolean"
eval' env m (EAnd e1 e2) = evalBool (&&) env m e1 e2
eval' env m (EOr e1 e2) = evalBool (||) env m e1 e2
eval' env m (ELt e1 e2) = evalRel (<) env m e1 e2
eval' env m (EGt e1 e2) = evalRel (>) env m e1 e2
eval' env m (ELeq e1 e2) = evalRel (<=) env m e1 e2
eval' env m (EGeq e1 e2) = evalRel (>=) env m e1 e2
eval' env m (EEq e1 e2) = evalRel (==) env m e1 e2
eval' env m (ENeq e1 e2) = evalRel (/=) env m e1 e2
eval' env m (EIf e1 e2 e3) = evalSub env e1 >>= evalBranch >>= putMVar m
  where
    evalBranch (VBool True)  = evalSub env e2
    evalBranch (VBool False) = evalSub env e3
eval' env m (EMatch e bs) = evalSub env e >>=
                            evalPatMatch env bs >>=
                            putMVar m 
eval' env m (ELet p e1 e2) = evalSub env e1 >>= \v1 ->
                             let binds = letBinds p v1
                                 env'  = update env binds
                             in evalSub env' e2 >>= putMVar m
eval' env m (EFun p e1 e2) =
    evalSub env e1 >>= \v1 ->
    let env'  = update env binds
        binds = letBinds p f
        f     = case (p, v1) of
                (PVar x, VClosure arg env e) -> VClosure arg (extend env x f) e
                _                            -> error "expected closure"
    in evalSub env' e2 >>= putMVar m
eval' env m (EAssign x e) = getRef (env Map.! x) >>= \r ->
                            evalSub env e >>=
                            writeIORef r >> putMVar m VUnit
  where
    getRef (VRef r) = return r
    getRef _        = error "expected reference"
eval' env m (ERef e) = evalSub env e >>= newIORef >>= return . VRef >>= putMVar m
eval' env m (EDeref e) = evalSub env e >>= getRef >>= readIORef >>= putMVar m
  where
    getRef (VRef r) = return r
    getRef _        = error "expected reference"
-- TODO: Handle unit argument.
eval' env m (ELam p e) = putMVar m $ VClosure (f p) env e
  where
    f (PVar x) = Just x
    f _        = Nothing
eval' env m (EApp e1 e2) = evalSub env e1 >>= \v1 ->
                           evalSub env e2 >>= \v2 ->
                           evalApp v1 v2 >>= putMVar m
  where
    evalApp (VClosure x env e) v = let env' = extend env x' v
                                       x'   = fromMaybe (error "") x
                                   in evalSub env' e
    evalApp _                  _ = error "expected closure"
eval' env m (ENu x e) = newChan >>= \c ->
                        let env' = extend env x $ VChannel x c
                        in evalSub env' e >>= putMVar m
eval' env m (ERd e) = evalSub env e >>= getChan >>= readChan >>= putMVar m
  where
    getChan (VChannel _ c) = return c
    getChan _              = error "expected channel"
eval' env m (EWr e1 e2) = evalSub env e2 >>= getChan >>= \c ->
                          evalSub env e1 >>= writeChan c >> putMVar m VUnit
  where
    getChan (VChannel _ c) = return c
    getChan _              = error "expected channel"
eval' env m (EFork e) = newEmptyMVar >>= \m' ->
                        forkIO (eval' env m' e) >>
                        putMVar m VUnit
-- TODO: fork forever or forever fork?                        
eval' env m (ERepl e) = newEmptyMVar >>= \m' ->
                        forkIO (forever $ eval' env m' e) >>
                        putMVar m VUnit
eval' env m (EThunk e) = putMVar m $ VThunk env e
eval' env m (EForce e) = evalSub env e >>= force >>= putMVar m
  where
    force (VThunk env e) = evalSub env e
    force _              = error "expected thunk"
eval' env m (ESeq e1 e2) =
    evalSub env e1 >> evalSub env e2 >>= putMVar m
eval' env m (EPrint e) =
    evalSub env e >>= putStrLn . show >> putMVar m VUnit

-- TODO: Types    
exec :: [Command] -> IO Value
exec cmds = go emptyEnv cmds
  where
    go env ((CExpr e):[] ) = eval env e
    go env ((CExpr e):rest) = eval env e >>
                              go env rest
    go env ((CDef x e):[]) = eval env e
    go env ((CDef x e):rest) = eval env e >>= \v ->
                               let env' = extend env x v
                               in go env' rest
    go env ((CTySig _ _):rest) = go env rest
