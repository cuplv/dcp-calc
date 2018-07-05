module Eval where

import Syntax

import Data.Maybe
import Data.Functor

{-eval env (Var x)      =
    fromMaybe (error "Variable not found") $ lookup x env-}
       
eval :: Env -> Expr -> Value       
eval env (EInt n) = VInt n
eval env (EBool b) = VBool b
eval env (EString s) = VString s
eval env (EList es) = VList $ map (eval env) es
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
