module Eval where

import qualified Data.Map.Strict as Map
import Data.Maybe

import Syntax

eval :: Environment -> Expr -> Value       
eval env (EVar x) = env Map.! x
--eval env (EImpVar x) = 
eval env (EInt n) = VInt n
eval env (EBool b) = VBool b
eval env (EString s) = VString s
eval env (ETag s) = VTag s
eval env (EList es) = VList $ map (eval env) es
eval env (ESet es) = VSet $ map (eval env) es
eval env (ETuple es) = VTuple $ map (eval env) es
eval env EUnit = VUnit
eval env (EPlus e1 e2)  =
    case (eval env e1, eval env e2) of
        (VInt n1, VInt n2) -> VInt (n1 + n2)
        _                  -> error "add"
eval env (EMinus e1 e2)  =
    case (eval env e1, eval env e2) of
        (VInt n1, VInt n2) -> VInt (n1 - n2)
        _                  -> error "sub"
eval env (ETimes e1 e2)  =
    case (eval env e1, eval env e2) of
        (VInt n1, VInt n2) -> VInt (n1 * n2)
        _                  -> error "mul"
eval env (EDivide e1 e2)  =
    case (eval env e1, eval env e2) of
        (VInt n1, VInt n2) -> VInt (n1 `quot` n2)
        _                  -> error "div"
eval env (EMod e1 e2)  =
    case (eval env e1, eval env e2) of
        (VInt n1, VInt n2) -> VInt (n1 `mod` n2)
        _                  -> error "mod"
eval env (ENot e)       =
    case (eval env e) of
        VBool b -> VBool (not b)
        _       -> error "not"
eval env (EAnd e1 e2)   =
    case (eval env e1, eval env e2) of
        (VBool b1, VBool b2) -> VBool (b1 && b2)
        _                    -> error "and"
eval env (EOr  e1 e2)   =
    case (eval env e1, eval env e2) of
        (VBool b1, VBool b2) -> VBool (b1 || b2)
        _                    -> error "or"
eval env (ELt  e1 e2)   =
    case (eval env e1, eval env e2) of
        (VInt b1, VInt b2) -> VBool (b1 < b2)
        _                    -> error "lt"
eval env (EGt  e1 e2)   =
    case (eval env e1, eval env e2) of
        (VInt b1, VInt b2) -> VBool (b1 > b2)
        _                    -> error "gt"
eval env (ELeq e1 e2)   =
    case (eval env e1, eval env e2) of
        (VInt b1, VInt b2) -> VBool (b1 <= b2)
        _                    -> error "leq"
eval env (EGeq e1 e2)   =
    case (eval env e1, eval env e2) of
        (VInt b1, VInt b2) -> VBool (b1 >= b2)
        _                    -> error "geq"
eval env (EEq e1 e2)   =
    case (eval env e1, eval env e2) of
        (VInt b1, VInt b2) -> VBool (b1 == b2)
        _                    -> error "eq"
eval env (ENeq e1 e2)   =
    case (eval env e1, eval env e2) of
        (VInt b1, VInt b2) -> VBool (b1 /= b2)
        _                    -> error "neq"
eval env (EIf e1 e2 e3) =
    case (eval env e1) of
        VBool True        -> eval env e2
        VBool False       -> eval env e3
eval env (EMatch e bs) = pmThenEval env (eval env e) bs
eval env (ELet p e1 e2) = eval env' e2
  where
    env' = update env binds
    v = eval env e1
    binds = fromMaybe (error "let pattern matching failed") $
            pmEnv v p
-- eval env (ELetRec p e1 e2) =
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
        _                     -> error "expected closure"
-- eval env (ERd e)
-- eval env (EWr e1 e2)
-- eval env (ERepl e)
-- eval env (EFork e)
eval env (EThunk e) = VThunk env e
eval env (EForce e) =
    case (eval env e) of
        (VThunk env' e') -> eval env' e'
        _                -> error "expected thunk"
eval env (ESeq e1 e2) =
    case (eval env e1) of
        _ -> eval env e2
-- eval env (EPrint e) =

pmThenEval :: Environment -> Value -> [(Pattern, Expr, Expr)] -> Value
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
        vp    = zip vs ps

exec :: [Command] -> Environment -> Maybe Value
exec ((CExpr e):[] ) env = Just (eval env e)
exec ((CExpr e):rest) env = exec rest env
  where
    v = eval env e
exec ((CDef x e):rest) env = exec rest env'
  where
    v = eval env e
    env' = extend env x v
exec ((CTySig _ _):rest) env = exec rest env
exec [] env = Nothing

run :: [Command] -> Maybe Value
run cmds = exec cmds emptyEnv
