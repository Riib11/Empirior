module Implication where

import           Context
import           Grammar

implies :: Formula -> Formula -> ProgramState Bool
implies (Formula g p) (Formula h q) = case (g, h) of
  (Precise,   Precise)   -> impliesPrecise p q
  (Imprecise, Precise)   -> error "TODO: imprecise implication"
  (Precise,   Imprecise) -> error "TODO: imprecise implication"
  (Imprecise, Imprecise) -> error "TODO: imprecise implication"

(==>) = implies

impliesPrecise :: PreciseFormula -> PreciseFormula -> ProgramState Bool
impliesPrecise p q = case p of
  FormulaExpression e -> case e of
    ExpressionValue v          -> error "TODO"
    ExpressionVariable x       -> error "TODO"
    ExpressionApplication n es -> error "TODO"
  FormulaNegation p -> error "TODO"
  FormulaOperation o os -> error "TODO"
  FormulaPredication n es  -> error "TODO"
  FormulaIfThenElse e p p' -> error "TODO"
