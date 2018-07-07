module Eval where

import qualified Data.Map.Strict as Map
import Data.Maybe

import Syntax

{-eval env (Var x)      =
    fromMaybe (error "Variable not found") $ lookup x env-}
       
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
eval env (EMatch e bs) = 
    patternMatch env (eval env e) bs
-- eval env (ELet p e1 e2) =
-- eval env (ELetRec p e1 e2) =
-- eval env (EAssign p e) =
-- eval env (ERef e)
-- eval env (EDeref e)
-- eval env (ELam e1 e2)
-- eval env (EApp e1 e2)
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

patternMatch env v ((PVar x, g, e):bs) = eval env e -- ^ [v/x]e

patternMatch env v@(VInt n) ((PInt n', g, e):bs) | n == n' = eval env e
                                                 | otherwise = patternMatch env v bs
patternMatch env v ((PInt _, _, _):bs) = patternMatch env v bs

patternMatch env v@(VString s) ((PString s', g, e):bs) | s == s' = eval env e
                                                       | otherwise = patternMatch env v bs
patternMatch env v ((PString _, _, _):bs) = patternMatch env v bs

patternMatch env v@(VTag s) ((PTag s', g, e):bs) | s == s' = eval env e
                                                 | otherwise = patternMatch env v bs
patternMatch env v ((PTag _, _, _):bs) = patternMatch env v bs

{-patternMatch env v@(VList (v:vs)) ((PList (p:ps), g, e):bs) | s == s' = eval env e
                                                 | otherwise = patternMatch env v bs

patternMatch env v@(VUnit) ((PUnit, g, e):bs) = eval env e
patternMatch env v ((PUnit _, _, _):bs) = patternMatch env v bs

patternMatch env v ((PWildcard, g, e):bs) = eval env e-}

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
