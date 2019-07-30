module Implication where

import           Grammar

implies :: Formula -> Formula -> Bool
implies p q = case (p, q) of
  (FormulaImprecise p, FormulaImprecise q) -> error "TODO: imprecise implication"
  (FormulaImprecise p, FormulaPrecise   q) -> error "TODO: imprecise implication"
  (FormulaPrecise   p, FormulaImprecise q) -> error "TODO: imprecise implication"
  (FormulaPrecise   p, FormulaPrecise   q) -> error "TODO"

(==>) = implies
