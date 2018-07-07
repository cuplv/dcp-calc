module PatternMatch where

import Data.Maybe
import Syntax

f a b = pure (++) <*> a <*> b

getMapping v p = go [] v p
  where
    go acc v (PVar x) = Just ((x, v) : acc)
    go acc (VInt n) (PInt n') | n == n'   =  Just acc
                              | otherwise = Nothing
    go acc (VBool b) (PBool b') | b == b'   =  Just acc
                                | otherwise = Nothing
    go acc (VString s) (PString s') | s == s'   =  Just acc
                                    | otherwise = Nothing
    go acc (VTag t) (PTag t') | t == t'   =  Just acc
                              | otherwise = Nothing
    go acc (VList vs) (PList ps) = gos acc vs ps
    go acc (VList (v:vs)) (PCons p ps) = foldl1 f [acc1, acc2, Just acc]
      where
        acc1 = getMapping v p
        acc2 = getMapping (VList vs) ps
    go acc (VList []) (PCons _ _) = Nothing
    go acc (VSet vs) (PSet ps) = gos acc vs ps
    go acc (VTuple vs) (PTuple ps) = gos acc vs ps
    go acc VUnit PUnit = Just acc
    go acc _     PWildcard = Just acc

    gos acc vs ps | length vs == length ps = foldl f (Just []) accs
                  | otherwise              = Nothing
      where
        accs  = map (\pair -> case pair of (v, p) -> go acc v p) vp
        vp    = zip vs ps
