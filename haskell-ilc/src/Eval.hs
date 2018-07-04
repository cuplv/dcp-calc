module Eval where

import Syntax

import Data.Maybe
import Data.Functor

isVal :: Expr -> Bool
isVal ETrue    = True
isVal EFalse   = True
isVal EUnit    = True
isVal ENil     = True
isVal (EInt _) = True
isVal (EString _) = True
isVal (EList es) = all isVal es
isVal (EPair e1 e2) = isVal e1 && isVal e2
isVal _        = False


-- Small step semantics
eval' :: Expr -> Maybe Expr
eval' e = case e of
  EPair e1 e2 | not (isVal e1) -> Just $ EPair (nf e1) e2
  EPair e1 e2 | not (isVal e2) -> Just $ EPair e1 (nf e2)
  
  EIf ETrue e2 e3 -> Just e2
  EIf EFalse e2 e3 -> Just e3
  EIf e1 e2 e3 -> (\e1' -> EIf e1' e2 e3) <$> eval' e1
  
  EPlus (EInt n1) (EInt n2) -> Just $ EInt (n1 + n2)
  EPlus e1@(EInt _) e2 -> (\e2' -> EPlus e1 e2') <$> eval' e2
  EPlus e1 e2 -> (\e1' -> EPlus e1' e2) <$> eval' e1

  EMinus (EInt n1) (EInt n2) -> Just $ EInt (n1 - n2)
  EMinus e1@(EInt _) e2 -> (\e2' -> EMinus e1 e2') <$> eval' e2
  EMinus e1 e2 -> (\e1' -> EMinus e1' e2) <$> eval' e1

  ETimes (EInt n1) (EInt n2) -> Just $ EInt (n1 * n2)
  ETimes e1@(EInt _) e2 -> (\e2' -> ETimes e1 e2') <$> eval' e2
  ETimes e1 e2 -> (\e1' -> ETimes e1' e2) <$> eval' e1

  EDivide (EInt n1) (EInt n2) -> Just $ EInt (n1 `quot` n2)
  EDivide e1@(EInt _) e2 -> (\e2' -> EDivide e1 e2') <$> eval' e2
  EDivide e1 e2 -> (\e1' -> EDivide e1' e2) <$> eval' e1
  
  EMod (EInt n1) (EInt n2) -> Just $ EInt (n1 `mod` n2)
  EMod e1@(EInt _) e2 -> (\e2' -> EMod e1 e2') <$> eval' e2
  EMod e1 e2 -> (\e1' -> EMod e1' e2) <$> eval' e1
  
  EOr ETrue _ -> Just ETrue
  EOr EFalse e2 -> Just e2
  EOr e1 e2   -> (\e1' -> EOr e1' e2) <$> eval' e1
  
  EAnd EFalse _ -> Just EFalse
  EAnd ETrue e2 -> Just e2
  EAnd e1 e2   -> (\e1' -> EAnd e1' e2) <$> eval' e1

  ENot ETrue -> Just EFalse
  ENot EFalse -> Just ETrue
  ENot e -> (\e' -> ENot e') <$> eval' e

  ELt (EInt n1) (EInt n2) -> Just $ if n1 < n2 then ETrue else EFalse
  ELt e1@(EInt _) e2 -> (\e2' -> ELt e1 e2') <$> eval' e2
  ELt e1 e2 -> (\e1' -> ELt e1' e2) <$> eval' e1

  EGt (EInt n1) (EInt n2) -> Just $ if n1 > n2 then ETrue else EFalse
  EGt e1@(EInt _) e2 -> (\e2' -> EGt e1 e2') <$> eval' e2
  EGt e1 e2 -> (\e1' -> EGt e1' e2) <$> eval' e1

  ELeq (EInt n1) (EInt n2) -> Just $ if n1 <= n2 then ETrue else EFalse
  ELeq e1@(EInt _) e2 -> (\e2' -> ELeq e1 e2') <$> eval' e2
  ELeq e1 e2 -> (\e1' -> ELeq e1' e2) <$> eval' e1

  EGeq (EInt n1) (EInt n2) -> Just $ if n1 >= n2 then ETrue else EFalse
  EGeq e1@(EInt _) e2 -> (\e2' -> EGeq e1 e2') <$> eval' e2
  EGeq e1 e2 -> (\e1' -> EGeq e1' e2) <$> eval' e1

  EEq (EInt n1) (EInt n2) -> Just $ if n1 == n2 then ETrue else EFalse
  EEq e1@(EInt _) e2 -> (\e2' -> EEq e1 e2') <$> eval' e2
  EEq e1 e2 -> (\e1' -> EEq e1' e2) <$> eval' e1

  ENeq (EInt n1) (EInt n2) -> Just $ if n1 /= n2 then ETrue else EFalse
  ENeq e1@(EInt _) e2 -> (\e2' -> ENeq e1 e2') <$> eval' e2
  ENeq e1 e2 -> (\e1' -> ENeq e1' e2) <$> eval' e1
  
  _ -> Nothing


nf :: Expr -> Expr
nf x = fromMaybe x (nf <$> eval' x)

eval :: Expr -> Maybe Expr
eval e = case nf e of
  nfe | isVal nfe -> Just nfe
      | otherwise -> Nothing
