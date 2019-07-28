module WeakestPrecondition where

import           Grammar

formulaTrue = FormulaPrecise $ FormulaExpression $ ExpressionValue $ ValueBoolean True

weakestPrecondition :: Statement -> Formula -> Formula
weakestPrecondition s p = case s of
  Function n args t p q s -> error "TODO"

  Predicate n args p -> error "TODO"

  Assert q -> conjoin p q

  IfThenElse e s s' -> conjoin p $
    FormulaPrecise $ FormulaIfThenElse e
      (weakestPrecondition s p) (weakestPrecondition s' p)

  WhileLoop e p s -> error "TODO"

  Declaration n t -> error "TODO"

  Assignment n e -> error "TODO"

  Skip -> error "TODO"

  Return e -> error "TODO"

  Sequence (s:ss) -> weakestPrecondition s $ weakestPrecondition (Sequence ss) p
  Sequence []     -> formulaTrue

conjoin :: Formula -> Formula -> Formula
conjoin p q = case (p, q) of
  (FormulaPrecise   p, FormulaPrecise   q) -> FormulaPrecise   (FormulaOperation And p q)
  (FormulaPrecise   p, FormulaImprecise q) -> FormulaImprecise (FormulaOperation And p q)
  (FormulaImprecise p, FormulaPrecise   q) -> FormulaImprecise (FormulaOperation And p q)
  (FormulaImprecise p, FormulaImprecise q) -> FormulaImprecise (FormulaOperation And p q)
