module Eval where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Identity
import Data.IORef
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe

import Syntax

data Value
    = VInt Integer
    | VBool Bool
    | VString String
    | VTag String
    | VList [Value]
    | VSet [Value]
    | VTuple [Value]
    | VUnit
    | VClosure (Maybe Name) TermEnv Expr
    | VThunk TermEnv Expr
    | VChannel Name (Chan Value)
    | VRef (IORef Value)
    deriving (Eq)

instance Show Value where
    show (VInt n) = show n
    show (VBool b) = show b
    show (VString s) = show s
    show (VTag t) = show t
    show (VList vs) = show vs
    show (VSet vs) = show vs
    show (VTuple vs) = show vs
    show VUnit = show ()
    show VClosure {} = "closure"
    show VThunk {} = "thunk"
    show (VChannel x _) = x
    show (VRef x) = show x

instance Show (IORef a) where
    show _ = "ref"

type TermEnv = Map.Map Name Value

emptyTmEnv :: TermEnv
emptyTmEnv = Map.empty

extendEnv :: TermEnv -> Name -> Value -> TermEnv
extendEnv env x v = Map.insert x v env

updateEnv :: TermEnv -> [(Name, Value)] -> TermEnv
updateEnv env env' = foldl f env env'
  where
    f env (x, v) = Map.insert x v env

evalSub :: TermEnv -> Expr -> IO Value
evalSub env e = newEmptyMVar >>= \m ->
                eval' env m e >>
                takeMVar m >>= return
                
evalSubs :: TermEnv -> Expr -> Expr -> IO (Value, Value)
evalSubs env e1 e2 = evalSub env e1 >>= \v1 ->
                     evalSub env e2 >>= \v2 ->
                     return (v1, v2)

evalBinOp f env m e1 e2 =
    evalSubs env e1 e2 >>= f >>= putMVar m
    
evalArith op = evalBinOp $ f op
  where
    f op (VInt n1, VInt n2) = return $ VInt (op n1 n2)

evalBool op = evalBinOp $ f op
  where
    f op (VBool b1, VBool b2) = return $ VBool (op b1 b2)

evalRel op = evalBinOp $ f op
  where
    f op (VInt n1, VInt n2) = return $ VBool (op n1 n2)

evalList env m con es = mapM (evalSub env) es >>= return . con >>= putMVar m

evalPatMatch :: TermEnv -> [(Pattern, Expr, Expr)] -> Value -> IO Value
evalPatMatch env ((p, g, e):bs) v =
    case (getBinds p v) of
        Just binds -> let env' = updateEnv env binds
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
        
eval :: TermEnv -> Expr -> IO Value
eval env e = newEmptyMVar >>= \v ->
             eval' env v e >>
             takeMVar v

eval' :: TermEnv -> MVar Value -> Expr -> IO ()
eval' env m expr = case expr of
    EVar x -> putMVar m $ env Map.! x  -- TODO: Change to lookup
    -- EImpVar x ->
    ELit (LInt n) -> putMVar m $ VInt n
    ELit (LBool b) -> putMVar m $ VBool b
    ELit (LString s) -> putMVar m $ VString s
    ELit (LTag t) -> putMVar m $ VTag t
    ELit LUnit -> putMVar m $ VUnit
    
    ETuple es -> evalList env m VTuple es
    EList es -> evalList env m VList es
    ESet es -> evalList env m VSet $ nub es  -- TODO: Use Set

    -- TODO: Refactor
    EBin Add e1 e2 -> evalArith (+) env m e1 e2
    EBin Sub e1 e2 -> evalArith (-) env m e1 e2
    EBin Mul e1 e2 -> evalArith (*) env m e1 e2
    EBin Div e1 e2 -> evalArith quot env m e1 e2
    EBin Mod e1 e2 -> evalArith mod env m e1 e2
    EBin And e1 e2 -> evalBool (&&) env m e1 e2
    EBin Or e1 e2 -> evalBool (||) env m e1 e2
    EBin Lt e1 e2 -> evalRel (<) env m e1 e2
    EBin Gt e1 e2 -> evalRel (>) env m e1 e2
    EBin Leq e1 e2 -> evalRel (<=) env m e1 e2
    EBin Geq e1 e2 -> evalRel (>=) env m e1 e2
    EBin Eql e1 e2 -> evalRel (==) env m e1 e2
    EBin Neq e1 e2 -> evalRel (/=) env m e1 e2

    EUn Not e -> evalSub env e >>= neg >>= putMVar m
      where
        neg (VBool b) = return $ VBool $ not b
    
    EIf e1 e2 e3 -> evalSub env e1 >>= evalBranch >>= putMVar m
      where
        evalBranch (VBool True)  = evalSub env e2
        evalBranch (VBool False) = evalSub env e3
        
    EMatch e bs -> evalSub env e >>=
                   evalPatMatch env bs >>=
                   putMVar m
                   
    ELet p e1 e2 -> evalSub env e1 >>= \v1 ->
                    let binds = letBinds p v1
                        env'  = updateEnv env binds
                    in evalSub env' e2 >>= putMVar m
                    
    EFun p e1 e2 ->
        evalSub env e1 >>= \v1 ->
        let env'  = updateEnv env binds
            binds = letBinds p f
            f     = case (p, v1) of
                    (PVar x, VClosure arg env e) -> VClosure arg (extendEnv env x f) e
        in evalSub env' e2 >>= putMVar m

    -- TODO: Applying operation twice
    EAssign x e -> getRef (env Map.! x) >>= \r ->
                   evalSub env e >>=
                   writeIORef r >> putMVar m VUnit
      where
        getRef (VRef r) = return r

    ERef e -> evalSub env e >>= newIORef >>= return . VRef >>= putMVar m

    EDeref e -> evalSub env e >>= getRef >>= readIORef >>= putMVar m
      where
        getRef (VRef r) = return r

    -- TODO: Handle unit argument.
    ELam p e -> putMVar m $ VClosure (f p) env e
      where
        f (PVar x) = Just x
        f _        = Nothing
    EApp e1 e2 -> evalSub env e1 >>= \v1 ->
                  evalSub env e2 >>= \v2 ->
                  evalApp v1 v2 >>= putMVar m
      where
        evalApp (VClosure x env e) v = let env' = extendEnv env x' v
                                           x'   = fromMaybe (error "") x
                                       in evalSub env' e

    ENu x e -> newChan >>= \c ->
               let env' = extendEnv env x $ VChannel x c
               in evalSub env' e >>= putMVar m

    ERd e -> evalSub env e >>= getChan >>= readChan >>= putMVar m
      where
        getChan (VChannel _ c) = return c

    EWr e1 e2 -> evalSub env e2 >>= getChan >>= \c ->
                 evalSub env e1 >>= writeChan c >> putMVar m VUnit
      where
        getChan (VChannel _ c) = return c

    EFork e -> newEmptyMVar >>= \m' ->
               forkIO (eval' env m' e) >>
               putMVar m VUnit

    ERepl e -> newEmptyMVar >>= \m' ->
               forkIO (forever $ eval' env m' e) >>
               putMVar m VUnit

    EThunk e -> putMVar m $ VThunk env e

    EForce e -> evalSub env e >>= force >>= putMVar m
      where
        force (VThunk env e) = evalSub env e

    ESeq e1 e2 -> evalSub env e1 >> evalSub env e2 >>= putMVar m

    EPrint e -> evalSub env e >>= putStrLn . show >> putMVar m VUnit

-- TODO: Types    
exec :: [Decl] -> IO Value
exec cmds = go emptyTmEnv cmds
  where
{-    go env ((DExpr e):[] ) = eval env e
    go env ((DExpr e):rest) = eval env e >>
                              go env rest-}
    go env ((x, e):[]) = eval env e
    go env ((x, e):rest) = eval env e >>= \v ->
                               let env' = extendEnv env x v
                               in go env' rest
    -- go env ((CTySig _ _):rest) = go env rest
