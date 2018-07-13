module Pretty
    (
      ppexpr
    , ppval
    ) where

import Text.PrettyPrint (Doc, (<>), (<+>))
import qualified Text.PrettyPrint as PP

import Syntax
import Eval

parensIf :: Bool -> Doc -> Doc
parensIf True = PP.parens
parensIf False = id

class Pretty p where
  ppr :: Int -> p -> Doc

instance Pretty Expr where
  ppr _ (ELit (LInt n)) = PP.text $ show n
  ppr _ (ELit (LString s)) = PP.text $ show s
  ppr _ (ELit (LBool b)) = PP.text $ show b
  ppr _ (ELit LUnit) = PP.text "()"
  {-ppr p (EList (e:es)) = ppr p e <+> ppr p (EList es)
  ppr p (EList []) = PP.text "[]"-}
  {-ppr p (EPair e1 e2) =
        PP.text "(" <+> ppr p e1
    <+> PP.text "," <+> ppr p e2
    <+> PP.text ")"-}
  -- ppr p (EIsZero a) = (parensIf (p > 0) $ PP.text "iszero" <+> ppr (p+1) a)
  ppr p (EIf e1 e2 e3) =
        PP.text "if"   <+> ppr p e1
    <+> PP.text "then" <+> ppr p e2
    <+> PP.text "else" <+> ppr p e3
  ppr _ _ = PP.text "expr"

instance Pretty Value where
    ppr _ (VInt n) = PP.text $ show n
    ppr _ (VBool True) = PP.text "true"
    ppr _ (VBool False) = PP.text "false"
    ppr _ (VString s) = PP.text $ show s
    ppr _ (VTag s) = PP.text s
    ppr p (VList vs) = PP.brackets $ ppList p vs
    ppr p (VSet vs) = PP.braces $ ppList p vs
    ppr p (VTuple vs) = PP.parens $ ppList p vs
    ppr _ VUnit = PP.text "()"
    ppr _ (VClosure x _ _) = PP.text "closure"
    ppr p (VThunk _ e) = PP.text "thunk(" <> ppr p e <> PP.text ")"
    ppr _ (VChannel x _) = PP.text x
    ppr _ (VRef x) = PP.text $ show x

ppList p vs = PP.hcat $ PP.punctuate PP.comma $ map (ppr p) vs

ppexpr :: Expr -> String
ppexpr = PP.render . ppr 0

ppval :: Value -> String
ppval = PP.render . ppr 0
