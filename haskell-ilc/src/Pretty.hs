module Pretty
    (
      ppexpr
    , ppval
    ) where

import Text.PrettyPrint (Doc, (<>), (<+>))
import qualified Text.PrettyPrint as PP

import Syntax

parensIf :: Bool -> Doc -> Doc
parensIf True = PP.parens
parensIf False = id

class Pretty p where
  ppr :: Int -> p -> Doc

instance Pretty Expr where
  ppr _ (EInt n) = PP.text $ show n
  ppr _ (EString s) = PP.text $ show s
  ppr _ (EBool b) = PP.text $ show b
  --ppr _ EUnit = PP.text "()"
  {-ppr p (EList (e:es)) = ppr p e <+> ppr p (EList es)
  ppr p (EList []) = PP.text "[]"-}
  ppr p (EPair e1 e2) =
        PP.text "(" <+> ppr p e1
    <+> PP.text "," <+> ppr p e2
    <+> PP.text ")"
  -- ppr p (EIsZero a) = (parensIf (p > 0) $ PP.text "iszero" <+> ppr (p+1) a)
  ppr p (EIf e1 e2 e3) =
        PP.text "if"   <+> ppr p e1
    <+> PP.text "then" <+> ppr p e2
    <+> PP.text "else" <+> ppr p e3

instance Pretty Value where
    ppr _ (VInt n) = PP.text $ show n
    ppr _ (VBool b) = PP.text $ show b
    ppr _ (VString s) = PP.text s
    ppr _ _           = error "not implemented"

ppexpr :: Expr -> String
ppexpr = PP.render . ppr 0

ppval :: Value -> String
ppval = PP.render . ppr 0
