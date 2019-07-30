{-# LANGUAGE LambdaCase #-}

module WeakestPrecondition where

import           Control.Lens

import           Grammar

formulaBool = FormulaPrecise . FormulaExpression . ExpressionValue . ValueBoolean

weakestPrecondition :: Statement -> Formula -> Formula
weakestPrecondition s wp = case s of
  Function n args t p q s -> error "TODO"

  Predicate n args p -> error "TODO"

  Assert p -> conjunct p wp

  IfThenElse e s s' -> conjuncts
    [ FormulaPrecise $ FormulaIfThenElse
        e (weakestPrecondition s wp) (weakestPrecondition s' wp)
    , wp ]

  WhileLoop e p s -> conjuncts
    [ FormulaPrecise $ FormulaExpression e
    , FormulaPrecise $ FormulaIfThenElse
        e (weakestPrecondition s (conjunct p wp)) (formulaBool True)
    , wp ]

  Declaration n t -> wp

  Assignment n e -> substituteFormula e n wp

  Skip -> wp

  Return e -> substituteFormula e "result" wp

  Sequence (s:ss) -> weakestPrecondition s $ weakestPrecondition (Sequence ss) wp
  Sequence []     -> formulaBool True

conjunct :: Formula -> Formula -> Formula
conjunct p q = case (p, q) of
  (FormulaPrecise   _, FormulaPrecise   _) -> FormulaPrecise   (FormulaOperation And p q)
  (FormulaPrecise   _, FormulaImprecise _) -> FormulaImprecise (FormulaOperation And p q)
  (FormulaImprecise _, FormulaPrecise   _) -> FormulaImprecise (FormulaOperation And p q)
  (FormulaImprecise _, FormulaImprecise _) -> FormulaImprecise (FormulaOperation And p q)

conjuncts :: [Formula] -> Formula
conjuncts = \case
  []     -> formulaBool True
  [p]    -> p
  (p:ps) -> foldl conjunct p ps

substituteFormula :: Expression -> Name -> Formula -> Formula
substituteFormula e y =
  let substitutePreciseFormula = \case
        FormulaExpression  e     -> FormulaExpression $ substituteExpression e
        FormulaOperation   o p q -> FormulaOperation o (substituteFormula e y p)
                                                       (substituteFormula e y q)
        FormulaPredication n es  -> FormulaPredication n $ map substituteExpression es
        FormulaIfThenElse  e p q -> FormulaIfThenElse (substituteExpression e)
                                                      (substituteFormula e y p)
                                                      (substituteFormula e y q)
      substituteExpression = \case
        ExpressionValue v          -> ExpressionValue v
        ExpressionVariable x       -> if x == y then e else ExpressionVariable x
        ExpressionApplication n es -> ExpressionApplication n $
                                      map substituteExpression es
  in \case
    FormulaPrecise   p -> FormulaPrecise   $ substitutePreciseFormula p
    FormulaImprecise p -> FormulaImprecise $ substitutePreciseFormula p
